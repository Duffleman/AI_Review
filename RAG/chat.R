seed <- 24601

# Chat with the research papers in the vector store
#
# Connects a local Ollama chat to the store built by the embedder. The model
# gets a retrieval tool, which it uses to look up relevant chunks before answering.
#
# input:   depression.duckdb, built by the embedder script
# output:  the model's answers, printed to the console
# note:    the store is opened read-only, but the embedder still can't write
#          to it while this connection is open
# server:  set OLLAMA_CONTEXT_LENGTH=32768, OLLAMA_FLASH_ATTENTION=1,
#          OLLAMA_KV_CACHE_TYPE=q8_0, OLLAMA_KEEP_ALIVE=30m, then restart Ollama
library(ellmer); library(ragnar)

db_path <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Depression\depression.duckdb)"

query_chat <- chat_ollama(
  model         = "qwen3.5:9b",
  base_url      = "http://localhost:11434",
  system_prompt = "
  You answer questions about the user's research papers.
  Always search the store before answering. Cite the source file
  for every claim. If the papers don't contain the answer, say so.

  Write clean, plain, human-readable text without markdown symbols
  such as asterisks. Answer the question directly.",
  params   = params(seed = seed, temperature = 0.3),
  api_args = list(reasoning_effort = "none")   # thinking off; use "high" for synthesis questions
)

close_store <- function(store) {
  drv <- store@con@driver
  DBI::dbDisconnect(store@con)
  duckdb::duckdb_shutdown(drv)
}

ask <- function(query, follow_up = FALSE) {
  store <- ragnar_store_connect(db_path, read_only = TRUE)
  on.exit(close_store(store), add = TRUE)
  
  if (!follow_up) query_chat$set_turns(list())   # start fresh unless it's a follow-up
  
  query_chat$set_tools(list())
  ragnar_register_tool_retrieve(query_chat, store, top_k = 6)
  
  query_chat$chat(query)
}