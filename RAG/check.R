seed             <- 24601
reasoning_effort <- "low"


# Fact-check statements against the papers they cite
#
# Finds the paper cited in each statement in the store built by the embedder,
# retrieves the passages most relevant to the statement from that paper, and
# asks Gemini whether they support it.
#
# input:   `papers.duckdb`, built by the embedder script
# output:  a list with the decision, the reason, the paper and the excerpts used
# note:    the store is opened read-only, but the embedder still can't write
#          to it while this connection is open
library(ellmer); library(ragnar)

db_path <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library\papers.duckdb)"
store   <- ragnar_store_connect(db_path, read_only = TRUE)

# The start of each paper (title, authors, year), used to find the cited paper
papers <- DBI::dbGetQuery(
  store@con,
  "SELECT origin, substr(text, 1, 3000) AS head FROM documents"
)

# The model gets the evidence in the prompt, so it needs no retrieval tool
check_chat <- chat_google_gemini(
  system_prompt = "
  You are a fact checker. You receive a statement and excerpts from the paper it cites.
  Base your decision only on these excerpts, not on your own knowledge.
  - True: the excerpts support the statement.
  - False: the excerpts contradict the statement.
  - Inconclusive: the excerpts don't address the statement.",
  params = params(seed = seed, reasoning_effort = reasoning_effort)
)

schema <- type_object(
  "Fact-check verdict for a statement against the paper it cites",
  decision = type_enum(
    "Fact check decision",
    values = c("True", "False", "Inconclusive")
  ),
  reason = type_string(
    "Two sentences justifying the decision, referring to the relevant excerpts"
  )
)

# Matches citations like "Heltne et al. (2026)", "(Heltne et al., 2026)",
# "Heltne (2026)", "(Heltne & Smith, 2026)" and "Heltne and Smith (2026)"
citation_pattern <- paste0(
  "(\\p{Lu}[\\p{L}'-]+)",                                         # surname
  "(?:\\s+(?:et\\s+al\\.?|(?:and|&)\\s+\\p{Lu}[\\p{L}'-]+))?",    # et al. / co-author
  ",?\\s*\\(?((?:19|20)\\d{2})[a-z]?\\)?"                         # year
)

# Split a query into the statement and the author and year it cites.
# If there are several matches, the last one is used, since citations
# usually come at the end of a sentence.
parse_citation <- function(query) {
  found <- regmatches(query, gregexpr(citation_pattern, query, perl = TRUE))[[1]]
  if (length(found) == 0) return(NULL)
  
  citation <- found[length(found)]
  parts    <- regmatches(citation, regexec(citation_pattern, citation, perl = TRUE))[[1]]
  
  statement <- sub(citation, "", query, fixed = TRUE)
  statement <- trimws(sub("[()[:space:]]+$", "", statement))  # leftover brackets
  
  list(author = parts[2], year = parts[3], citation = citation, statement = statement)
}

# Papers whose first page, or file name, contains both the author and the year
find_paper <- function(author, year) {
  in_text <- grepl(author, papers$head, fixed = TRUE) &
    grepl(year,   papers$head, fixed = TRUE)
  in_name <- grepl(author, basename(papers$origin), ignore.case = TRUE) &
    grepl(year,   basename(papers$origin), fixed = TRUE)
  papers$origin[in_text | in_name]
}

# Result for a statement that couldn't be checked, with a console message
not_checked <- function(decision, reason) {
  message(reason)
  list(decision = decision, reason = reason, paper = NA_character_, excerpts = character())
}

check <- function(query, top_k = 100, n_chunks = 8) {
  
  # 1. Find the citation in the query
  cit <- parse_citation(query)
  if (is.null(cit)) {
    return(not_checked("No citation",
                       "No citation like 'Author et al. (Year)' was found in the query."))
  }
  
  # 2. Find the cited paper in the store
  match <- find_paper(cit$author, cit$year)
  if (length(match) == 0) {
    return(not_checked("Paper not found",
                       paste0("The store has no paper by ", cit$author, " from ", cit$year, ".")))
  }
  if (length(match) > 1) {
    return(not_checked("Ambiguous citation",
                       paste0("Several papers match ", cit$author, " (", cit$year, "): ",
                              paste(basename(match), collapse = ", "))))
  }
  
  # 3. Search the store, then keep only chunks from the cited paper
  hits <- ragnar_retrieve(store, cit$statement, top_k = top_k)
  hits <- head(hits[hits$origin == match, ], n_chunks)
  
  if (nrow(hits) == 0) {
    return(list(
      decision = "Inconclusive",
      reason   = "No passages relevant to the statement were found in the cited paper.",
      paper    = basename(match),
      excerpts = character()
    ))
  }
  
  # 4. Ask the model, in a fresh copy of the chat so earlier checks don't leak in
  excerpts <- paste0("[Excerpt ", seq_len(nrow(hits)), "]\n", hits$text,
                     collapse = "\n\n")
  prompt <- paste0(
    "Statement: ", cit$statement, "\n\n",
    "Excerpts from ", basename(match), ":\n\n", excerpts
  )
  
  chat   <- check_chat$clone()
  result <- chat$chat_structured(prompt, type = schema)
  
  # Keep the evidence with the verdict so you can see what it was based on
  result$paper    <- basename(match)
  result$excerpts <- hits$text
  result
}

