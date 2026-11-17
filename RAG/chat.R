seed = 24601
reasoning_effort = "low"


# Chat with the research papers in the vector store
#
# Connects a Gemini chat to the store built by the embedder. The model gets a
# retrieval tool, which it uses to look up relevant chunks before answering.
#
# input:   `papers.duckdb`, built by the embedder script
# output:  the model's answers, printed to the console
# note:    the store is opened read-only, but the embedder still can't write
#          to it while this connection is open
library(ellmer); library(ragnar)

db_path <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Depression\depression.duckdb)"

# The system prompt asks the model to search first, cite its sources and say
# when the papers don't answer the question, to keep it from making things up
query_chat <- chat_ollama(
  model         = "gemma2:9b",   # any locally pulled model; use a tool-capable one if you register tools
  base_url      = "http://localhost:11434",
  system_prompt = "
  You answer questions about the user's research papers.
  Always search the store before answering. Cite the source file
  for every claim. If the papers don't contain the answer, say so.

  Be sure to write clean human-readable text. Do not use special characters like '*' needlessly.
  Write straightforward text that answers the user's query cleanly.",
  params        = ellmer::params(seed = seed),
  api_args      = list(reasoning_effort = reasoning_effort)  # only honored by reasoning models, e.g. gpt-oss
)

close_store <- function(store) {
  drv <- store@con@driver
  DBI::dbDisconnect(store@con)
  duckdb::duckdb_shutdown(drv)
}

ask <- function(querry) {
  store <- ragnar_store_connect(db_path, read_only = TRUE)
  on.exit(close_store(store))
  
  querry_chat$set_tools(list())
  ragnar_register_tool_retrieve(querry_chat, store, top_k = 10)
  
  querry_chat$chat(querry)
}
