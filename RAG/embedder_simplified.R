library(ragnar)
library(cli)

# Set directory path
references <- r"(C:\Users\Aleksander\Desktop\Coding\R\ai_blog\RAG\Depression)"

# Set up filepath for the database
db_path <- file.path(references, "depression.duckdb")

# Get file list (full paths, excluding the database itself)
to_embed <- list.files(references, full.names = TRUE)
to_embed <- to_embed[!grepl("\\.duckdb(\\.wal)?$", to_embed)]

# Close connection if the store is already in use
if (exists("store")) {
  DBI::dbDisconnect(store@con, shutdown = TRUE)
  rm(store)
}

# Set up embedder
store <- ragnar_store_create(
  db_path,
  embed = \(x) ragnar::embed_ollama(x, model = "embeddinggemma-2"),
  overwrite = TRUE
)

# Set up progress bar
cli_progress_bar(
  "Embedding",
  total  = length(to_embed),
  format = "{pb_bar} {pb_current}/{pb_total} | ETA {pb_eta} | {basename(file)}"
)

# Run embedder for each file in the file list
for (file in to_embed) {
  chunks <- file |>
    read_as_markdown() |>
    markdown_chunk()
  ragnar_store_insert(store, chunks)
  cli_progress_update()
}

cli_progress_done()

ragnar_store_build_index(store)

#ragnar_store_inspect(store)
