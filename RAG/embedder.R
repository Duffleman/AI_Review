library(ragnar)


references <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Library)"
setwd(references)
files <- list.files(references, pattern = "\\.pdf$", full.names = TRUE)




store <- ragnar_store_create(
  "papers.duckdb",
  embed = \(x) embed_google_gemini(x, model = "gemini-embedding-001"),
  overwrite = TRUE
)

for (file in files) {
  chunks <- file |> read_as_markdown() |> markdown_chunk()
  ragnar_store_insert(store, chunks)
}
ragnar_store_build_index(store)
DBI::dbDisconnect(store@con, shutdown = TRUE)
