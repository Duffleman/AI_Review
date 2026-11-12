# =============================================================================
# Setup:
#
# Set the working directory to where you stored run_screening.R and
# screening_helpers.R
# Set the directory holding context_statement.txt, inclusion_criteria.txt and
# exclusion_criteria.txt (one criterion per line; lines starting with # are ignored)
# Set the filepath to the .ris file with the references to screen
# Set the output filepath for the .csv export
# =============================================================================

#setwd()
#setup_dir    <- "."                        # folder with the three setup .txt files
#ris_filepath <- ""                          # e.g. "references.ris"
#csv_filepath <- ""

# =============================================================================
# Title/abstract screening with a large language model (Google Gemini via ellmer)
#
# Requirements:
#   - R (>= 4.1)
#   - The ellmer, synthesisr, glue and readr packages:
#       install.packages(c("ellmer", "synthesisr", "glue", "readr"))
#   - A Google Gemini API key stored as GEMINI_API_KEY in your .Renviron file
# =============================================================================

# ---- Environment setup ------------------------------------------------------
# External imports:
library(ellmer)
library(synthesisr)
library(glue)
library(readr)

# Internal imports:
source("abstract_screening_helpers.R")

# Stop early with a clear message if the API key is missing
if (Sys.getenv("GEMINI_API_KEY") == "") {
  stop("GEMINI_API_KEY is not set. Add it to your .Renviron file and restart R. ",
       "The commented command below opens the .Renviron for editing.")
}

#file.edit("~/.Renviron")   # add a line like: GEMINI_API_KEY=your-key

# Record the software environment for reproducibility
sessionInfo()


# ---- Read setup files -------------------------------------------------------
setup <- read_setup(setup_dir)


# ---- Import references ------------------------------------------------------
refs <- import_refs(ris_filepath)


# ---- Configure system prompt ------------------------------------------------
system_prompt <- build_system_prompt(setup)

# Review system prompt before proceeding:
cat("\n========== SYSTEM PROMPT ==========\n\n",
    system_prompt,
    "\n\n===================================\n\n", sep = "")

# Optional: preview the prompt for the first reference
#cat(build_prompt(refs[1, ]))


# ---- Set up LLM client ------------------------------------------------------
model            <- "gemini-3.8-flash"
seed             <- 42
reasoning_effort <- "low"   # "low" or "high"
timeout_s        <- 600     # seconds to wait for each response

client <- build_client(system_prompt, model, seed, reasoning_effort, timeout_s)


# ---- Set up output schema ---------------------------------------------------
schema <- build_schema()


# ---- Run screening ----------------------------------------------------------
# Concurrency limits; check your API rate limits before increasing these
max_active <- 10    # requests running at the same time
rpm        <- 250   # requests per minute; lower this if you hit 429s on the free tier

screened <- run_screening(client, refs, schema, max_active, rpm)

# Optional: re-run any references that failed (e.g. rate-limit errors)
#screened <- rerun_failed(client, screened, schema, max_active = max_active, rpm = rpm)

summarise_screening(screened)


# ---- Estimate cost (optional) -----------------------------------------------
# Prices in USD per million tokens for your model; check https://ai.google.dev/pricing
#price_input  <-
#price_output <-
#price_cached <-
#screened <- estimate_cost(screened, price_input, price_output, price_cached)


# ---- Save results -----------------------------------------------------------
write_csv(screened, csv_filepath)