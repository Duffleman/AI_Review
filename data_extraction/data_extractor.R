# =============================================================================
# Setup:
#
# Set working directory where you stored the data_extractor.R and data_extractor_helpers.R and export fields files
# Set the target directory where fulltext PDF files are stored
# Set the file title for the export fields
# Set the output filepath for the .xlsx export
# Set the output filepath for the .rdata export
# =============================================================================

#setwd()
#target_dir <- 
#export_fields <- 
#excel_filepath <- 
#rdata_filepath <- 

# =============================================================================
# PDF data extraction with a large language model (Google Gemini via ellmer)
#
# Requirements:
#   - R (>= 4.1)
#   - The ellmer, glue and pdftools packages:
#       install.packages(c("ellmer", "glue", "pdftools"))
#   - A Google Gemini API key stored as GEMINI_API_KEY in your .Renviron file
# =============================================================================

# ---- Environment setup ------------------------------------------------------
# External imports:
library(ellmer)
library(glue)
library(pdftools)
library(writexl)

# Internal imports:
source("data_extraction_helpers.R")

# Stop early with a clear message if the API key is missing
if (Sys.getenv("GEMINI_API_KEY") == "") {
  stop("GEMINI_API_KEY is not set. Add it to your .Renviron file and restart R. The commented command below opens the .Renviron for edit")
}

#file.edit("~/.Renviron")


# Record the software environment for reproducibility
sessionInfo()


# ---- List PDF files ---------------------------------------------------------
max_pages  <- 35    # PDFs longer than this are skipped

pdf_files <- import_files(target_dir, max_pages)

# ---- Configure system prompt ------------------------------------------------
data_points   <- read_data_points(export_fields)
system_prompt <- build_system_prompt(data_points)

# Review system prompt before proceeding:
cat("\n========== SYSTEM PROMPT ==========\n\n",
    system_prompt,
    "\n\n===================================\n\n", sep = "")

# ---- Set up LLM client ------------------------------------------------------
model            <- "gemini-3.8-flash"
seed             <- 42
reasoning_effort <- "low"   # "low" or "high"
timeout_s        <- 600     # seconds to wait for each response

client <- build_client(system_prompt, model, seed, reasoning_effort, timeout_s)

# ---- Set up output schema ---------------------------------------------------
schema <- build_schema(data_points)


# ---- Run extraction ---------------------------------------------------------
# Concurrency limits; check your API rate limits before increasing these
max_active <- 5    # requests running at the same time
rpm        <- 10   # requests per minute

# The model returns every field as text
results <- run_extraction(client, pdf_files, schema, max_active, rpm)


# ---- Convert to declared types ----------------------------------------------
# Applies the [type] from data_points.txt to each column; "null" becomes NA,
# and any value that cannot be converted is listed in a warning
results <- convert_results(results, data_points)


# ---- Estimate cost ----------------------------------------------------------
# Prices in USD per million tokens; check https://ai.google.dev/pricing
price_input  <- 0.75
price_output <- 3.75
price_cached <- 0.075

results <- estimate_cost(results, price_input, price_output, price_cached)

# Optional: save results to disk
write_xlsx(results, excel_filepath)
save(results, file = rdata_filepath)

