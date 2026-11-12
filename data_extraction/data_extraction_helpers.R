# =============================================================================
# Helper functions for the PDF data-extraction pipeline
# =============================================================================

# Return the paths of all PDF files in a directory, up to a maximum length
#
# target_dir: path to the directory to search
# max_pages:  PDFs with more pages than this are skipped
# returns:    character vector of full paths to the PDFs to process
import_files <- function(target_dir, max_pages = 20) {
  all_files <- list.files(path = target_dir, full.names = TRUE)
  pdf_files <- all_files[grepl("\\.pdf$", all_files, ignore.case = TRUE)]
  
  # Count pages; files that cannot be read get NA
  n_pages <- vapply(pdf_files, function(f) {
    tryCatch(as.numeric(pdftools::pdf_length(f)), error = function(e) NA_real_)
  }, numeric(1))
  
  # Report and drop files that are unreadable or too long
  unreadable <- is.na(n_pages)
  too_long   <- !unreadable & n_pages > max_pages
  
  if (any(unreadable)) {
    message("Skipping ", sum(unreadable), " unreadable PDF(s):\n",
            paste0("  ", basename(pdf_files[unreadable]), collapse = "\n"))
  }
  if (any(too_long)) {
    message("Skipping ", sum(too_long), " PDF(s) longer than ", max_pages, " pages:\n",
            paste0("  ", basename(pdf_files[too_long]),
                   " (", n_pages[too_long], " pages)", collapse = "\n"))
  }
  
  pdf_files <- unname(pdf_files[!unreadable & !too_long])
  
  # Stop with a clear message if nothing is left to process
  if (length(pdf_files) == 0) {
    stop("No PDF files to process in '", target_dir, "'.")
  }
  
  return(pdf_files)
}

# Read data-point definitions from a text file
#
# file:    path to a text file with one data point per line, as
#          "name [type]: definition". The type is optional (default "string").
#          Blank lines and lines starting with "#" are ignored.
# returns: data frame with columns `name`, `type` and `definition`
read_data_points <- function(file) {
  allowed_types <- c("string", "integer", "number", "boolean")
  
  lines <- trimws(readLines(file, warn = FALSE, encoding = "UTF-8"))
  lines <- lines[lines != "" & !startsWith(lines, "#")]
  
  # Every remaining line must contain a colon separating name and definition
  missing_colon <- !grepl(":", lines, fixed = TRUE)
  if (any(missing_colon)) {
    stop("Each line must be written as 'name [type]: definition'. Check: ",
         paste(lines[missing_colon], collapse = " | "))
  }
  
  # Split at the first colon only, so definitions may themselves contain colons
  header     <- trimws(sub(":.*$", "", lines))
  definition <- trimws(sub("^[^:]*:", "", lines))
  
  # Read the optional [type] at the end of the header; default to "string"
  has_type <- grepl("\\[[^]]*\\]$", header)
  type     <- ifelse(has_type,
                     tolower(sub("^.*\\[([^]]*)\\]$", "\\1", header)),
                     "string")
  name     <- trimws(sub("\\[[^]]*\\]$", "", header))
  
  # Check that all types are recognised
  bad_type <- !type %in% allowed_types
  if (any(bad_type)) {
    stop("Unknown type(s): ", paste(unique(type[bad_type]), collapse = ", "),
         ". Allowed types: ", paste(allowed_types, collapse = ", "))
  }
  
  data_points <- data.frame(name = name, type = type, definition = definition)
  
  # Names must be unique, as each becomes a column in the results
  if (anyDuplicated(data_points$name)) {
    stop("Duplicate data point names: ",
         paste(unique(data_points$name[duplicated(data_points$name)]), collapse = ", "))
  }
  
  return(data_points)
}

# Build the system prompt sent to the model
#
# data_points: data frame with columns `name`, `type` and `definition`
# returns:     the system prompt as a single character string



build_system_prompt <- function(data_points) {
  fields <- paste0("- ", data_points$name, " (", data_points$type, "): ",
                   data_points$definition, collapse = "\n")
  
  system_prompt <- glue::glue(
    "ROLE:
    You are a data extraction system for systematic reviews of research on
    health interventions. You extract specific information from one academic
    paper and return it in a structured format. Your output goes directly into
    a research dataset, so accuracy matters more than completeness: an honest
    null is always better than a guessed value.

    TASK:
    You will be given one academic paper as a PDF file. For each field listed
    under FIELDS TO EXTRACT, extract the requested information from the paper.
    Each field has a name, an expected type in brackets, and a definition.
    Follow each definition exactly; where a definition specifies a format, use
    that format.

    WHERE TO LOOK:
    - Read the whole paper: title page, abstract, main text, tables, figure
      captions, footnotes, declarations (funding, correspondence, author
      information) and any appendices included in the PDF.
    - Extract information about this paper's own study only. Ignore details of
      other studies described in the introduction, discussion or references.
    - If the abstract and main text disagree, use the main text. For numbers,
      prefer tables over running text.
    - If the paper reports more than one study, extract each field for all of
      them and label each part (e.g. Study 1: ...; Study 2: ...).

    WHAT COUNTS AS REPORTED:
    - Use only the paper. Do not use prior knowledge of the paper, its authors,
      the intervention or related work, even if you recognise them.
    - Information stated anywhere in the paper counts as reported. You may
      reformat it (abbreviate, convert units, combine statements from different
      sections), and where a definition asks you to classify or summarise, do
      so based on what the paper describes.
    - Do not add facts or details the paper does not state, and do not
      calculate statistics the paper does not report.
    - If the information for a field is not reported at all, return exactly
      the string \"null\" (lowercase, nothing else). Do not write 'not
      reported', 'N/A', 'unclear' or an explanation. If information is partly
      reported, return the part that is reported rather than null.

    OUTPUT FORMAT:
    - Plain text only: no markdown, bullet points or line breaks.
    - Separate multiple items within a field with '; '.
    - Give integer and number fields as a number only (e.g. 2021), and boolean
      fields as true or false.
    - Copy names, titles, labels and statistics exactly as printed, including
      decimal places, confidence intervals and p-value notation (e.g. p < .001).
    - For descriptive fields, write in your own words and keep it concise: one
      to three sentences unless the definition says otherwise.

    BEFORE ANSWERING:
    Check every value against the paper: right field, right study arm, right
    time point. Then check that values are consistent across fields (e.g.
    per-group numbers add up to totals; outcomes named in one field match the
    results reported in another).

    FIELDS TO EXTRACT:
    {fields}"
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

# Build the output schema from the data points
#
# data_points: data frame with columns `name`, `type` and `definition`
# returns:     an ellmer object type with one required text field per data point.
#              All fields are text so the model is never blocked mid-answer;
#              convert_results() applies the declared types afterwards.
build_schema <- function(data_points) {
  # One required text field per data point, described by its definition
  fields <- lapply(data_points$definition, ellmer::type_string)
  names(fields) <- data_points$name
  
  # Combine the fields into a single object describing one paper
  schema <- do.call(
    ellmer::type_object,
    c(list(.description = "Data extracted from a single academic paper"), fields)
  )
  
  return(schema)
}

# Build the prompt for a single PDF
#
# pdf_file: path to one PDF file
# returns:  a list containing a short instruction and the PDF content,
#           ready to be sent to the model as one prompt
build_prompt <- function(pdf_file) {
  prompt <- list(
    "Extract the data defined in your instructions from the attached paper.",
    ellmer::content_pdf_file(pdf_file)
  )
  return(prompt)
}

# Run the extraction on a set of PDFs
#
# client:     the ellmer chat object from build_client()
# pdf_files:  character vector of paths to PDF files
# schema:     the output schema from build_schema()
# max_active: maximum number of requests sent to the model at the same time
# rpm:        maximum number of requests sent per minute
# returns:    data frame with one row per PDF and one column per data point,
#             plus the file name and token usage
run_extraction <- function(client, pdf_files, schema, max_active = 5, rpm = 10) {
  # Build one prompt per PDF
  prompts <- lapply(pdf_files, build_prompt)
  
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
  
  # Add the file name as the first column, so each row can be traced to its PDF
  results$file <- basename(pdf_files)
  results <- results[, c("file", setdiff(names(results), "file"))]
  
  return(results)
}

# Convert extracted values to the types declared in data_points.txt
#
# results:     data frame returned by run_extraction()
# data_points: data frame with columns `name`, `type` and `definition`
# returns:     the results with each data-point column converted to its type.
#              "null" becomes NA. Values that cannot be converted also become NA,
#              and are listed in a warning so they can be checked against the paper.
convert_results <- function(results, data_points) {
  problems <- character(0)
  
  for (i in seq_len(nrow(data_points))) {
    name <- data_points$name[i]
    type <- data_points$type[i]
    
    # Treat "null" and empty answers as missing
    value <- trimws(as.character(results[[name]]))
    value[tolower(value) %in% c("null", "")] <- NA
    
    converted <- switch(type,
                        string  = value,
                        number  = suppressWarnings(as.numeric(value)),
                        integer = {
                          num <- suppressWarnings(as.numeric(value))
                          num[!is.na(num) & num != round(num)] <- NA   # reject decimals
                          as.integer(num)
                        },
                        boolean = unname(c(true = TRUE, false = FALSE)[tolower(value)])
    )
    
    # Record values that were present but could not be converted
    failed <- !is.na(value) & is.na(converted)
    if (any(failed)) {
      problems <- c(problems, paste0("  ", results$file[failed], " | ", name,
                                     ": \"", value[failed], "\""))
    }
    
    results[[name]] <- converted
  }
  
  if (length(problems) > 0) {
    warning("Some values could not be converted to their declared type and were set to NA:\n",
            paste(problems, collapse = "\n"), call. = FALSE)
  }
  
  return(results)
}


# Estimate the cost of an extraction run from its token counts
#
# results:      data frame returned by run_extraction() or convert_results()
# price_input:  price per million input tokens (USD)
# price_output: price per million output tokens, including thinking (USD)
# price_cached: price per million cached input tokens (USD)
# returns:      the results with a `cost_usd` column added; also prints the
#               total and the average cost per paper
estimate_cost <- function(results, price_input, price_output, price_cached = 0) {
  results$cost_usd <- (results$input_tokens        * price_input +
                         results$output_tokens       * price_output +
                         results$cached_input_tokens * price_cached) / 1e6
  
  message("Estimated total cost: $", round(sum(results$cost_usd), 4),
          " (average $", round(mean(results$cost_usd), 4), " per paper, ",
          nrow(results), " papers)")
  
  return(results)
}
