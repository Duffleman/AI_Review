# Chat with research papers in a ragnar vector store
#
# Connects a Gemini chat to a store built by the embedder. The model gets a
# retrieval tool and uses it to look up relevant chunks before answering.
#
# input:   a .duckdb store built by the embedder script
# output:  the model's answers, printed to the console
# note:    each question opens the store read-only and closes it afterwards,
#          so the embedder can write to it between questions

library(ellmer)
library(ragnar)

seed <- 246014

default_system_prompt <- trimws(r"(
You are a research assistant helping with a systematic review. The user has
embedded a collection of research papers into a searchable store. You answer
questions about individual papers and across the collection, using only what
the search tool returns.

SEARCHING
- Always search before answering. Never answer from general knowledge.
- Search several times with different phrasings. One search returns only a
  few chunks and will miss relevant passages.
- For questions about one paper, include its author, title or file name in
  the search query.
- For questions across papers, search for each concept separately, and
  search again with synonyms and related outcome or method terms. Keep going
  until new searches stop turning up new papers.
- If a result looks relevant but incomplete, search again for the missing
  detail (for example the sample size or the confidence interval).

READING THE RESULTS
- Results are fragments, not whole papers. Do not assume a fragment reflects
  the paper's overall conclusion.
- Separate a paper's own findings from work it merely cites. Text from an
  introduction or discussion that describes other studies is not that paper's
  result.
- Note the study design, population and sample size where they are available,
  since they affect how much weight a finding deserves.

ANSWERING
- Cite the source file for every claim, in parentheses after the claim.
- Report numbers exactly as written: effect sizes, confidence intervals,
  p-values, sample sizes, follow-up periods. Do not round, convert or
  average them.
- Do not pool or combine results across studies unless the user asks.
- When papers disagree, report each position with its source rather than
  resolving the conflict.
- If the papers do not answer the question, say so plainly. If they answer
  it only partly, give what they contain and state what is missing.
- For questions across papers, end with a short note on coverage: which
  papers the answer draws on, and that papers not returned by the search may
  also be relevant.

STYLE
- Write plain text without markdown symbols such as asterisks, pound signs
  or bullet characters. Use short paragraphs, or numbered lines when listing
  studies.
- Answer the question directly in the first sentence, then give the
  supporting detail.
)")

# Fresh chat object, so the system prompt is always set
new_chat <- function(system_prompt = default_system_prompt,
                     model = "gemini-3-flash-preview") {
  chat_google_gemini(
    model         = model,
    system_prompt = system_prompt,
    params        = params(seed = seed)
  )
}

close_store <- function(store) {
  con <- store@con
  if (DBI::dbIsValid(con)) {
    drv <- con@driver
    DBI::dbDisconnect(con)
    duckdb::duckdb_shutdown(drv)
  }
}

# Build an ask() function bound to one database.
# Each call: connect -> register tool -> ask one question -> disconnect.
make_asker <- function(db_path,
                       system_prompt = default_system_prompt,
                       model = "gemini-3-flash-preview",
                       top_k = 10) {
  force(db_path); force(system_prompt); force(model); force(top_k)
  chat <- NULL
  
  function(query, follow_up = FALSE) {
    if (follow_up && is.null(chat)) {
      warning("No previous conversation; starting a new one.")
    }
    if (!follow_up || is.null(chat)) {
      chat <<- new_chat(system_prompt, model)
    }
    
    store <- ragnar_store_connect(db_path, read_only = TRUE)
    on.exit(close_store(store), add = TRUE)
    
    # Drop any tool bound to a previously closed store, then bind the open one
    chat$set_tools(list())
    ragnar_register_tool_retrieve(chat, store, top_k = top_k)
    
    chat$chat(query)
  }
}

# One asker per database -------------------------------------------------

ask_depression <- make_asker(
  r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Depression\depression.duckdb)"
)

# Later, for example:
# ask_anxiety <- make_asker("path/to/anxiety.duckdb")
# ask_sleep   <- make_asker("path/to/sleep.duckdb", top_k = 10)

# Usage:
# ask_depression("What are the main risk factors for depression?")
# ask_depression("Which of those were replicated?", follow_up = TRUE)