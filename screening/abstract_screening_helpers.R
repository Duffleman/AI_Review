# =============================================================================
# Helper functions for the title/abstract screening pipeline
# =============================================================================

# Check that the required setup files exist in a directory
#
# setup_dir:      path to the directory holding the setup files
# required_files: names of the files that must be present
# returns:        invisibly TRUE; stops with a clear message if files are missing
check_setup_files <- function(setup_dir = ".",
                              required_files = c("context_statement.txt",
                                                 "inclusion_criteria.txt",
                                                 "exclusion_criteria.txt")) {
  missing <- required_files[!file.exists(file.path(setup_dir, required_files))]
  if (length(missing) > 0) {
    stop("Missing required file(s) in '", normalizePath(setup_dir), "':\n",
         paste0("  ", missing, collapse = "\n"))
  }
  invisible(TRUE)
}

# Read a criteria file
#
# file:    path to a text file with one criterion per line.
#          Blank lines and lines starting with "#" are ignored.
# returns: character vector with one element per criterion
read_criteria <- function(file) {
  lines <- trimws(readLines(file, warn = FALSE, encoding = "UTF-8"))
  lines <- lines[lines != "" & !startsWith(lines, "#")]
  
  if (length(lines) == 0) {
    stop("No criteria found in '", file, "'.")
  }
  return(lines)
}

# Read the context statement
#
# file:    path to a text file describing the review
# returns: the context statement as a single character string
read_context <- function(file) {
  context <- trimws(readr::read_file(file))
  if (context == "") {
    stop("The context statement in '", file, "' is empty.")
  }
  return(context)
}

# Read all setup files into one list
#
# setup_dir: path to the directory holding the setup files
# returns:   list with `context`, `inclusion` and `exclusion`
read_setup <- function(setup_dir = ".") {
  check_setup_files(setup_dir)
  list(
    context   = read_context(file.path(setup_dir, "context_statement.txt")),
    inclusion = read_criteria(file.path(setup_dir, "inclusion_criteria.txt")),
    exclusion = read_criteria(file.path(setup_dir, "exclusion_criteria.txt"))
  )
}

# Import references from a .ris (or other synthesisr-supported) file
#
# filepath: path to the reference file
# returns:  data frame with columns id, doi, title, year, source, abstract.
#           Columns missing from the file are added as NA so later steps work.
import_refs <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("Reference file not found: '", filepath, "'.")
  }
  
  refs <- synthesisr::read_refs(filepath)
  refs <- as.data.frame(refs)
  
  wanted <- c("doi", "title", "year", "source", "abstract")
  absent <- setdiff(wanted, names(refs))
  if (length(absent) > 0) {
    message("Column(s) not found in the reference file and set to NA: ",
            paste(absent, collapse = ", "))
    refs[absent] <- NA_character_
  }
  
  refs <- refs[, wanted]
  refs <- cbind(id = seq_len(nrow(refs)), refs)
  
  # Flag records the model cannot judge well
  no_abstract <- is.na(refs$abstract) | trimws(refs$abstract) == ""
  if (any(no_abstract)) {
    message(sum(no_abstract), " of ", nrow(refs),
            " reference(s) have no abstract; they will be screened on title only.")
  }
  
  message("Imported ", nrow(refs), " reference(s) from '", basename(filepath), "'.")
  return(refs)
}

# Build the system prompt sent to the model
#
# setup:   list returned by read_setup()
# returns: the system prompt as a single character string
build_system_prompt <- function(setup) {
  system_prompt <- glue::glue(
    "You are a systematic review screener. For each paper (title, year, abstract), ",
    "decide whether it meets the review's inclusion criteria below.\n\n",
    "Context: {setup$context}\n\n",
    "Inclusion criteria:\n{paste('-', setup$inclusion, collapse = '\n')}\n\n",
    "Exclusion criteria:\n{paste('-', setup$exclusion, collapse = '\n')}\n\n",
    "Evaluate each criterion individually before deciding. If the abstract doesn't ",
    "provide enough information to judge a criterion, do not guess: mark the ",
    "decision as 'maybe' and explain what's missing in your reason."
  )
  return(as.character(system_prompt))
}

# Create the LLM client
#
# system_prompt:    the system prompt as a single character string
# model:            Gemini model ID (see https://ai.google.dev/gemini-api/docs/models)
# seed:             integer seed for sampling; improves, but does not guarantee,
#                   identical output on repeat runs
# reasoning_effort: how much the model thinks before answering
#                   ("low" is faster and cheaper; "high" is the model default)
# timeout_s:        seconds to wait for each response before giving up
# returns:          an ellmer chat object
build_client <- function(system_prompt, model, seed,
                         reasoning_effort = "low", timeout_s = 600) {
  # ellmer reads the timeout from a global option, so it applies to all
  # requests in this R session
  options(ellmer_timeout_s = timeout_s)
  
  client <- ellmer::chat_google_gemini(
    system_prompt = system_prompt,
    model         = model,
    params        = ellmer::params(seed = seed,
                                   reasoning_effort = reasoning_effort)
  )
  return(client)
}

# Build the output schema
#
# returns: an ellmer object type with a decision and a one-sentence reason
build_schema <- function() {
  ellmer::type_object(
    "A screening decision for a single abstract",
    decision = ellmer::type_enum(
      "The screening decision",
      values = c("include", "exclude", "maybe")
    ),
    reason = ellmer::type_string("One sentence justifying the decision")
  )
}

# Build the prompt for a single reference
#
# paper:   one row of the data frame returned by import_refs()
# returns: the prompt as a single character string
build_prompt <- function(paper) {
  # Replace missing values so the model does not see a bare "NA"
  fill <- function(x, alt) if (is.na(x) || trimws(x) == "") alt else x
  
  prompt <- glue::glue(
    "Title: {fill(paper$title, '[no title available]')}\n",
    "Year: {fill(paper$year, '[no year available]')}\n",
    "Abstract: {fill(paper$abstract, '[no abstract available]')}"
  )
  return(as.character(prompt))
}

# Run the screening on a set of references
#
# client:     the ellmer chat object from build_client()
# refs:       data frame returned by import_refs()
# schema:     the output schema from build_schema()
# max_active: maximum number of requests sent to the model at the same time
# rpm:        maximum number of requests sent per minute
# returns:    the references with `decision`, `reason` and token-usage columns added
run_screening <- function(client, refs, schema, max_active = 10, rpm = 250) {
  # Build one prompt per reference
  prompts <- lapply(seq_len(nrow(refs)), function(i) build_prompt(refs[i, ]))
  
  # Send all prompts to the model in parallel; each is an independent request
  results <- ellmer::parallel_chat_structured(
    chat           = client,
    prompts        = prompts,
    type           = schema,
    include_tokens = TRUE,
    max_active     = max_active,
    rpm            = rpm,
    on_error       = "continue"
  )
  
  screened <- cbind(refs, as.data.frame(results))
  
  # Report references that did not get a decision (e.g. API errors)
  failed <- is.na(screened$decision)
  if (any(failed)) {
    warning(sum(failed), " reference(s) did not get a decision (ids: ",
            paste(screened$id[failed], collapse = ", "),
            "). Re-run these with rerun_failed().", call. = FALSE)
  }
  
  return(screened)
}

# Re-run screening for references that did not get a decision
#
# client:   the ellmer chat object from build_client()
# screened: data frame returned by run_screening()
# schema:   the output schema from build_schema()
# ...:      passed on to run_screening() (e.g. max_active, rpm)
# returns:  the screened data frame with the failed rows filled in where possible
rerun_failed <- function(client, screened, schema, ...) {
  failed <- is.na(screened$decision)
  if (!any(failed)) {
    message("No failed references to re-run.")
    return(screened)
  }
  
  ref_cols <- c("id", "doi", "title", "year", "source", "abstract")
  retry    <- run_screening(client, screened[failed, ref_cols], schema, ...)
  
  screened[failed, names(retry)] <- retry
  return(screened)
}

# Print a summary of screening decisions
#
# screened: data frame returned by run_screening()
# returns:  invisibly, a table of decision counts
summarise_screening <- function(screened) {
  counts <- table(screened$decision, useNA = "ifany")
  names(counts)[is.na(names(counts))] <- "no decision"
  
  message("Screening summary (", nrow(screened), " references):\n",
          paste0("  ", names(counts), ": ", counts, collapse = "\n"))
  invisible(counts)
}

# Estimate the cost of a screening run from its token counts
#
# screened:     data frame returned by run_screening()
# price_input:  price per million input tokens (USD)
# price_output: price per million output tokens, including thinking (USD)
# price_cached: price per million cached input tokens (USD)
# returns:      the data frame with a `cost_usd` column added; also prints the
#               total and the average cost per reference
estimate_cost <- function(screened, price_input, price_output, price_cached = 0) {
  cached <- if ("cached_input_tokens" %in% names(screened)) screened$cached_input_tokens else 0
  
  screened$cost_usd <- (screened$input_tokens  * price_input +
                          screened$output_tokens * price_output +
                          cached                 * price_cached) / 1e6
  
  message("Estimated total cost: $", round(sum(screened$cost_usd, na.rm = TRUE), 4),
          " (average $", round(mean(screened$cost_usd, na.rm = TRUE), 5),
          " per reference, ", nrow(screened), " references)")
  return(screened)
}