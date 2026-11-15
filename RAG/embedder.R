# Build or update the vector store of research papers
#
# Converts every PDF in the library folder to markdown, removes the reference
# list, splits it into chunks, embeds the chunks with Gemini and saves them in
# a DuckDB store. Each chunk is tagged with the paper's citation key (from
# `references.csv`) so other scripts can search within a single paper. On
# later runs, only PDFs that aren't in the store yet are processed.
#
# input:   PDF files in `references`
#          `references.csv` in the same folder, with columns `citekey` and
#          `file`, e.g.  citekey = "Smith 2020", file = "Smith et al. (2020)
#          Trust.pdf". It's created and kept up to date automatically: blank
#          keys are guessed from filenames in the form "Surname ... (Year) ...".
#          Keys you edit by hand are kept.
# output:  `papers.duckdb` in the same folder, with a search index
# note:    close any chat session using the store first. DuckDB won't let the
#          file be opened for writing while another process has it open.
#          A store built by the old version of this script (without the
#          `citekey` column) is deleted and rebuilt automatically, once.
library(ragnar)
library(dplyr)

references  <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library)"
db_path     <- file.path(references, "papers.duckdb")
lookup_path <- file.path(references, "references.csv")

files <- list.files(references, pattern = "\\.pdf$", full.names = TRUE)


# --- Citation key lookup ------------------------------------------------------

# Guesses a citekey from a filename like "Heltne et al. (2026) MyADHD.pdf":
# the first word becomes the surname and the year in brackets the year, giving
# "Heltne 2026". Returns NA if the filename doesn't follow that pattern.
guess_citekey <- function(file) {
  m <- regmatches(file, regexec("^([^ (]+).*?\\((\\d{4}[a-z]?)\\)", file, perl = TRUE))
  vapply(m, \(x) if (length(x) == 3) paste(x[2], x[3]) else NA_character_,
         character(1))
}

# Reads references.csv whether it uses commas or semicolons (Excel with
# Norwegian settings saves with semicolons) and with or without the invisible
# byte-order mark Excel adds to the start of the file. Starts an empty table
# if the file doesn't exist yet.
if (file.exists(lookup_path)) {
  sep <- if (grepl(";", readLines(lookup_path, n = 1, warn = FALSE))) ";" else ","
  lookup <- read.csv(lookup_path, sep = sep, colClasses = "character",
                     na.strings = "", strip.white = TRUE, fileEncoding = "UTF-8-BOM")
  if (!all(c("citekey", "file") %in% names(lookup))) {
    stop("references.csv needs columns 'citekey' and 'file', but has: ",
         paste(names(lookup), collapse = ", "))
  }
} else {
  lookup <- data.frame(citekey = character(), file = character())
}

# Add a row for any PDF not listed yet, then fill every blank citekey with a
# guess from the filename. Keys you've typed in yourself are left alone.
unlisted <- setdiff(basename(files), lookup$file)
lookup   <- rbind(lookup, data.frame(citekey = rep(NA_character_, length(unlisted)),
                                     file    = unlisted))
blank <- is.na(lookup$citekey)
lookup$citekey[blank] <- guess_citekey(lookup$file[blank])

if (any(blank)) {
  write.csv(lookup, lookup_path, row.names = FALSE, na = "")
  filled <- lookup[blank & !is.na(lookup$citekey), ]
  message("Filled in citekeys from filenames (edit references.csv to change them):\n",
          paste0("  ", filled$citekey, "  <-  ", filled$file, collapse = "\n"))
}

# Two papers with the same key can't be told apart, e.g. two "Smith 2020"
dups <- unique(lookup$citekey[duplicated(lookup$citekey) & !is.na(lookup$citekey)])
if (length(dups) > 0) {
  warning("These citekeys are used by more than one paper. Add a letter ",
          "(e.g. 'Smith 2020a', 'Smith 2020b') in references.csv:\n",
          paste0("  ", dups, collapse = "\n"), call. = FALSE)
}

# Returns the citekey for a file (matched by name), or NA if it has none
citekey_for <- function(file) lookup$citekey[match(basename(file), lookup$file)]

no_key <- basename(files)[is.na(citekey_for(files))]
if (length(no_key) > 0) {
  warning("These PDFs have no citekey in references.csv. They are still added ",
          "to the store, but the citation checker can't find them:\n",
          paste0("  ", no_key, collapse = "\n"), call. = FALSE)
}


# --- Open or create the store -------------------------------------------------

# Closes the store completely. Disconnecting alone leaves DuckDB's database
# instance alive in this R session, which blocks other scripts from opening
# the file in a different mode (read-only vs. read-write) until R restarts.
close_store <- function(store) {
  drv <- store@con@driver
  DBI::dbDisconnect(store@con)
  duckdb::duckdb_shutdown(drv)
}

# The `citekey` column must be declared when the store is created; it can't be
# added later. The embedding function is saved inside the store, so it doesn't
# need to be given again when connecting.
create_store <- function() {
  ragnar_store_create(
    db_path,
    embed      = \(x) embed_google_gemini(x, model = "gemini-embedding-001"),
    extra_cols = data.frame(citekey = character())
  )
}

if (file.exists(db_path)) {
  store <- ragnar_store_connect(db_path, read_only = FALSE)
  
  # Stores built by the old script have no citekey column: rebuild from scratch
  if (!"citekey" %in% DBI::dbListFields(store@con, "embeddings")) {
    message("Store has no citekey column - deleting and rebuilding it.")
    close_store(store)
    file.remove(db_path)
    if (file.exists(paste0(db_path, ".wal"))) file.remove(paste0(db_path, ".wal"))
    store <- create_store()
  }
} else {
  store <- create_store()
}


# --- Remove reference lists ---------------------------------------------------

# Cuts everything from the last "References" / "Bibliography" heading onward.
# Reference lists are full of other papers' titles, which match search queries
# well and lead to false hits. The cut is skipped if the heading appears in
# the first half of the paper, as that's more likely a table of contents.
strip_references <- function(doc) {
  lines <- strsplit(doc, "\n", fixed = TRUE)[[1]]
  heading <- paste0(
    "^\\s*#*\\s*\\**\\s*(\\d+\\.?\\s*)?",
    "(references|bibliography|literature cited|works cited|reference list)",
    "\\s*\\**\\s*:?\\s*$"
  )
  hits <- grep(heading, lines, ignore.case = TRUE)
  
  if (length(hits) == 0 || max(hits) < 0.5 * length(lines)) return(doc)
  
  kept <- lines[seq_len(max(hits) - 1)]
  MarkdownDocument(paste(kept, collapse = "\n"), origin = doc@origin)
}


# --- Add new files --------------------------------------------------------------

# Find the PDFs that aren't in the store yet. Files are matched by name rather
# than full path, because the path stored in `origin` may be formatted
# differently (e.g. forward vs. back slashes).
existing  <- DBI::dbGetQuery(store@con, "SELECT origin FROM documents")$origin
new_files <- files[!basename(files) %in% basename(existing)]

if (length(new_files) == 0) {
  message("No new files - store is up to date.")
} else {
  message("Adding ", length(new_files), " new file(s):\n",
          paste0("  ", basename(new_files), collapse = "\n"))
  
  # Convert, strip references, chunk, tag and embed each new file. A file that
  # fails (e.g. a scanned or damaged PDF) produces a warning instead of
  # stopping the whole run.
  for (file in new_files) {
    tryCatch({
      chunks <- file |>
        read_as_markdown() |>
        strip_references() |>
        markdown_chunk() |>
        mutate(citekey = citekey_for(file))
      ragnar_store_insert(store, chunks)
    }, error = \(e) warning("Failed on ", basename(file), ": ", conditionMessage(e)))
  }
  
  # Rebuild the search index so the new chunks can be retrieved
  ragnar_store_build_index(store)
}


# --- Sync citekeys for papers already in the store ------------------------------

# If you add or fix a citekey in references.csv after a paper was embedded,
# this updates its chunks without re-embedding anything.
docs <- DBI::dbGetQuery(store@con, "SELECT doc_id, origin FROM documents")
for (i in seq_len(nrow(docs))) {
  DBI::dbExecute(
    store@con,
    "UPDATE embeddings SET citekey = ? WHERE doc_id = ? AND citekey IS DISTINCT FROM ?",
    params = list(citekey_for(docs$origin[i]), docs$doc_id[i],
                  citekey_for(docs$origin[i]))
  )
}

# Close the connection and release the file lock
close_store(store)