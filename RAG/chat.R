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
store <- ragnar_store_connect(db_path, read_only = TRUE)

# The system prompt asks the model to search first, cite its sources and say
# when the papers don't answer the question, to keep it from making things up
chat <- chat_google_gemini(   
  system_prompt = "
  You answer questions about the user's research papers.
  Always search the store before answering. Cite the source file
  for every claim. If the papers don't contain the answer, say so."
)

# Give the model a search tool for the store. Each search returns the
# 10 closest-matching chunks.
ragnar_register_tool_retrieve(chat, store, top_k = 10)

chat$chat("What was the research question(s) posed by the Heltne paper?")