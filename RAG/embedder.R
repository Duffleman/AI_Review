# Build or update the vector store of research papers
#
# Converts every PDF in the library folder to markdown, splits it into chunks,
# embeds the chunks with Gemini and saves them in a DuckDB store. On later
# runs, only PDFs that aren't in the store yet are processed.
#
# input:   PDF files in `references`
# output:  `papers.duckdb` in the same folder, with a search index
# note:    close any chat session using the store first. DuckDB won't let the
#          file be opened for writing while another process has it open.
library(ragnar)

references <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library)"
db_path    <- file.path(references, "papers.duckdb")

files <- list.files(references, pattern = "\\.pdf$", full.names = TRUE)

# Open the existing store, or create it on the first run. The embedding
# function is saved inside the store, so it doesn't need to be given again
# when connecting.
if (file.exists(db_path)) {
  store <- ragnar_store_connect(db_path, read_only = FALSE)
} else {
  store <- ragnar_store_create(
    db_path,
    embed = \(x) embed_google_gemini(x, model = "gemini-embedding-001")
  )
}

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
  
  # Convert, chunk and embed each new file. A file that fails (e.g. a scanned
  # or damaged PDF) produces a warning instead of stopping the whole run.
  for (file in new_files) {
    tryCatch({
      chunks <- file |> read_as_markdown() |> markdown_chunk()
      ragnar_store_insert(store, chunks)
    }, error = \(e) warning("Failed on ", basename(file), ": ", conditionMessage(e)))
  }
  
  # Rebuild the search index so the new chunks can be retrieved
  ragnar_store_build_index(store)
}

# Close the connection and release the file lock
DBI::dbDisconnect(store@con, shutdown = TRUE)
