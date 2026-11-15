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

db_path <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library\papers.duckdb)"

# The system prompt asks the model to search first, cite its sources and say
# when the papers don't answer the question, to keep it from making things up
querry_chat <- chat_google_gemini(   
  system_prompt = "
  You answer questions about the user's research papers.
  Always search the store before answering. Cite the source file
  for every claim. If the papers don't contain the answer, say so.",
  params        = ellmer::params(seed = seed, reasoning_effort = reasoning_effort)
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
