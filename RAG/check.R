seed = 24601
reasoning_effort = "low"


# Check in-line citations against the papers in the vector store
#
# Finds every cited statement in a passage, looks up each cited paper in the
# store, retrieves the passages from that paper closest to the statement and
# asks Gemini whether they support it.
#
# input:   a passage of text with author-year citations, e.g. "(Smith, 2020)"
#          `papers.duckdb` and `references.csv`, built by the embedder script
# output:  a tibble with one row per statement:
#            statement     the cited claim
#            citations     list of the citations attached to it
#            verdict       the weakest verdict among its citations: supported,
#                          partially supported, insufficient evidence, not
#                          supported, or "not in library" if none were found
#            supported     TRUE if every cited paper found in the library
#                          fully supports it, FALSE if not, NA if none were found
#            explanation   each citation's verdict and a short explanation
#            per_citation  tibble with the verdict, explanation and supporting
#                          quote for each citation
#            unresolved    citations with no matching paper in references.csv
# note:    the store is only opened while check_citations() is running and is
#          closed when it finishes, so the embedder can run between checks
library(ellmer); library(ragnar); library(dplyr)

references  <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library)"
db_path     <- file.path(references, "papers.duckdb")
lookup_path <- file.path(references, "references.csv")

# Reads references.csv whether it uses commas or semicolons 
read_lookup <- function(path) {
  sep <- if (grepl(";", readLines(path, n = 1, warn = FALSE))) ";" else ","
  lookup <- read.csv(path, sep = sep, colClasses = "character", na.strings = "",
                     strip.white = TRUE, fileEncoding = "UTF-8-BOM")
  if (!all(c("citekey", "file") %in% names(lookup))) {
    stop("references.csv needs columns 'citekey' and 'file', but has: ",
         paste(names(lookup), collapse = ", "))
  }
  lookup
}

# Makes "Smith et al., 2020", "smith 2020" and "Smith & Lee 2020" comparable
normalise <- function(x) {
  x <- tolower(x)
  x <- gsub("et al\\.?|&|\\band\\b", " ", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

lookup     <- read_lookup(lookup_path)
known_keys <- na.omit(lookup$citekey)

# Closes the store completely. Disconnecting alone leaves DuckDB's read-only
# database instance alive in this R session, which then blocks the embedder
# from opening the file for writing until R is restarted.
close_store <- function(store) {
  drv <- store@con@driver
  DBI::dbDisconnect(store@con)
  duckdb::duckdb_shutdown(drv)
}

# A fresh chat for every call, so earlier statements don't influence later ones
new_chat <- function(system_prompt) {
  chat_google_gemini(
    system_prompt = system_prompt,
    params        = params(seed = seed, reasoning_effort = reasoning_effort)
  )
}


# --- Step 1: extract statements and their citations ---------------------------

# The model is given the citekeys in the library so it writes citations in the
# same form, which makes matching them to papers a simple lookup
extract_claims <- function(passage) {
  chat <- new_chat(paste0(
    "You extract cited statements from academic text. For every in-line ",
    "citation, return the statement it supports, rewritten as a self-contained ",
    "sentence, and the list of works cited for it. If several citations support ",
    "one statement, list them together. Write each citation as the first ",
    "author's surname followed by the year, e.g. 'Smith 2020'. If a citation ",
    "matches one of these known keys, use the key exactly: ",
    paste(known_keys, collapse = "; ")
  ))
  
  type <- type_object(
    claims = type_array(items = type_object(
      statement = type_string("The cited claim as a self-contained sentence."),
      citations = type_array(items = type_string("One cited work, e.g. 'Smith 2020'.")),
      sub_claims = type_array(items = type_string(paste(
        "One distinct fact in the statement, as a short sentence, e.g.",
        "'The study included 228 adults.' and '73.1% of participants were female.'",
        "A statement with a single fact has one sub-claim."
      )))
    ))
  )
  
  claims <- as_tibble(chat$chat_structured(passage, type = type)$claims)
  claims$citations  <- lapply(claims$citations,  as.character)
  claims$sub_claims <- lapply(claims$sub_claims, as.character)
  claims
}


# --- Step 2: retrieve evidence from one paper ---------------------------------

# Searches only the chunks tagged with this paper's citekey, once for each
# query (the full statement plus each of its sub-claims), and returns the
# unique chunk texts. Searching per fact matters for statements that combine
# details found in different parts of a paper, e.g. sample size in the
# abstract and gender split in a demographics table.
retrieve_evidence <- function(store, queries, paper_key, top_k = 10) {
  texts <- lapply(queries, \(query) {
    ragnar_retrieve(store, query, top_k = top_k,
                    filter = citekey == !!paper_key)$text
  })
  unique(unlist(texts))
}


# --- Step 3: judge whether the evidence supports the statement ----------------

# The possible verdicts, from weakest to strongest
verdict_levels <- c("not supported", "insufficient evidence",
                    "partially supported", "supported")

judge_claim <- function(statement, citation, evidence) {
  chat <- new_chat(paste(
    "You check whether a cited paper supports a statement made about it.",
    "Judge only from the passages provided; do not use outside knowledge.",
    "Use 'insufficient evidence' when the passages are unrelated to the",
    "statement, and 'not supported' when they contradict it or show the paper",
    "says something different."
  ))
  
  type <- type_object(
    verdict = type_enum(
      values      = verdict_levels,
      description = "Whether the passages support the statement."
    ),
    explanation    = type_string("One or two sentences explaining the verdict."),
    evidence_quote = type_string("The most relevant short quote from the passages, or '' if none.")
  )
  
  prompt <- paste0(
    "Statement: ", statement, "\n\n",
    "Cited paper: ", citation, "\n\n",
    "Passages from the paper:\n\n",
    paste(evidence, collapse = "\n\n---\n\n")
  )
  
  chat$chat_structured(prompt, type = type)
}


# --- Steps 1-3 together ---------------------------------------------------------

check_citations <- function(passage) {
  # Open the store for this check only. on.exit() closes it when the function
  # finishes, even if it stops with an error partway through.
  store <- ragnar_store_connect(db_path, read_only = TRUE)
  on.exit(close_store(store))
  
  claims  <- extract_claims(passage)
  results <- vector("list", nrow(claims))
  
  for (i in seq_len(nrow(claims))) {
    statement <- claims$statement[i]
    cites     <- claims$citations[[i]]
    keys      <- lookup$citekey[match(normalise(cites), normalise(lookup$citekey))]
    
    per_citation <- tibble(
      citation       = cites,
      citekey        = keys,
      verdict        = NA_character_,
      explanation    = NA_character_,
      evidence_quote = NA_character_
    )
    
    # Judge each citation that has a paper in the library
    for (j in which(!is.na(keys))) {
      queries  <- unique(c(statement, claims$sub_claims[[i]]))
      evidence <- retrieve_evidence(store, queries, keys[j])
      verdict  <- judge_claim(statement, cites[j], evidence)
      per_citation$verdict[j]        <- verdict$verdict
      per_citation$explanation[j]    <- verdict$explanation
      per_citation$evidence_quote[j] <- verdict$evidence_quote
    }
    
    # The statement's verdict is the weakest verdict among its citations, so a
    # statement citing one supporting and one contradicting paper is flagged.
    # `supported` is TRUE only if every checked paper fully supports it.
    checked   <- per_citation$verdict[!is.na(per_citation$verdict)]
    verdict   <- if (length(checked) == 0) "not in library" else
      verdict_levels[min(match(checked, verdict_levels))]
    supported <- if (length(checked) == 0) NA else verdict == "supported"
    
    explanation <- paste0(
      per_citation$citation, " (",
      coalesce(per_citation$verdict, "not in library"), ")",
      ifelse(is.na(per_citation$explanation), "", paste0(": ", per_citation$explanation)),
      collapse = " | "
    )
    
    results[[i]] <- tibble(
      statement    = statement,
      citations    = list(cites),
      verdict      = verdict,
      supported    = supported,
      explanation  = explanation,
      per_citation = list(per_citation),
      unresolved   = list(cites[is.na(keys)])
    )
  }
  
  bind_rows(results)
}


# --- Example ------------------------------------------------------------------

# passage <- "Trust in automation drops sharply after a single visible error
#   (Smith, 2020), although explanations can partly restore it (Jones et al.,
#   2021; Lee, 2019)."
#
# results <- check_citations(passage)
# results                                  # overview
# results$per_citation[[1]]                # detail for the first statement

