library(ragnar)

setwd(r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library)")

references <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library)"
setwd(references)

store <- ragnar_store_create(
  "papers.duckdb",
  embed = \(x) embed_google_gemini(x, model = "gemini-embedding-001"),
  overwrite = TRUE
)

for (file in list.files("papers", pattern = "\\.pdf$", full.names = TRUE)) {
  chunks <- file |> read_as_markdown() |> markdown_chunk()
  ragnar_store_insert(store, chunks)
}
ragnar_store_build_index(store)