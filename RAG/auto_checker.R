library(readxl)
library(glue)

data <- readxl::read_xlsx(r"(C:\Users\Aleksander\Downloads\Depression_Results_PostCheck.xlsx)", sheet=1)
names(data) <- as.character(unlist(data[1, ]))
data <- data[-1, ]
data <- readr::type_convert(data)   # columns were read as text because of the header row; this fixes the types

for (i in seq_len(3)) {
  row   <- data[i, ]
  paper <- paste(row$authors, row$year)
  query <- glue("Is this an accurate representation of the study sample in the {paper} paper: {row$sample_details}?")
  
  stopifnot(length(query) == 1, nzchar(query))   # catches empty queries before they reach the model
  cat(query, "\n\n")                            # print what you're sending, if you want to see it
  ask(query)
}