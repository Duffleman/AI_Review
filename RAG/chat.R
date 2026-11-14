library(ellmer); library(ragnar)

db_path <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library\papers.duckdb)"
store <- ragnar_store_connect(db_path, read_only = TRUE)

chat <- chat_google_gemini(   # or chat_anthropic() if you have a key
  system_prompt = "
  You answer questions about the user's research papers.
  Always search the store before answering. Cite the source file
  for every claim. If the papers don't contain the answer, say so."
)

ragnar_register_tool_retrieve(chat, store, top_k = 10)
chat$chat("What was the research question(s) posed by the Heltne paper?")