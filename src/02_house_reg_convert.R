
# R alternative to 02_house_reg_convert.py, for machines without Python's
# camelot. Converts each registration PDF in data/house_district_reg/pdfs/ to
# data/house_district_reg/csvs/<year-month>.csv, skipping ones already
# converted. Needs Poppler's pdftotext (ships with Git for Windows).
#
# pdftotext prints each row's numbers on the line of the next row's label
# (District 001's numbers sit beside "District 002", District 100's beside
# "Totals"), so labels and number rows are read separately and paired in
# order. The column set comes from the header: reports from 2025 on have no
# Libertarian columns.

library(readr)
library(stringr)
library(purrr)

pdftotext <- Sys.which("pdftotext")
if(pdftotext == ""){
  pdftotext <- "C:/Program Files/Git/mingw64/bin/pdftotext.exe"
}
stopifnot(file.exists(pdftotext))

convert_registration_pdf <- function(pdf_path, csv_path){
  lines <- system2(pdftotext, c("-layout", shQuote(pdf_path), "-"), stdout = TRUE)

  parties <- c("Democratic", "Republican", "Libertarian", "No Party", "Other")
  header <- lines[str_detect(lines, "Democratic\\s+Republican")][1]
  parties <- parties[str_detect(header, fixed(parties))]
  col_names <- c(
    "State Representative District",
    paste(parties, "Active"), "Total Active",
    paste(parties, "Inactive"), "Total Inactive",
    "Grand Total"
  )
  n_values <- length(col_names) - 1

  labels <- str_extract(lines, "^(State Representative District \\d{3}|Totals)\\b")
  labels <- labels[!is.na(labels)]

  values <- lines |>
    str_remove("^(State Representative District \\d{3}|Totals)") |>
    str_trim() |>
    str_split("\\s+") |>
    keep(\(tokens) length(tokens) == n_values && all(str_detect(tokens, "^[\\d,]+$"))) |>
    map(\(tokens) as.numeric(str_remove_all(tokens, ",")))

  if(length(labels) != length(values)){
    stop(pdf_path, ": ", length(labels), " row labels but ", length(values), " rows of numbers")
  }

  table <- as.data.frame(do.call(rbind, values))
  names(table) <- col_names[-1]
  table <- cbind(setNames(data.frame(labels), col_names[1]), table)

  # Each row's parts must add up to its totals
  active <- rowSums(table[paste(parties, "Active")])
  inactive <- rowSums(table[paste(parties, "Inactive")])
  bad <- active != table$`Total Active` | inactive != table$`Total Inactive` |
    table$`Total Active` + table$`Total Inactive` != table$`Grand Total`
  if(any(bad)){
    stop(pdf_path, ": totals don't add up for ", paste(table[[1]][bad], collapse = ", "))
  }

  write_csv(table, csv_path)
}

pdfs <- list.files(file.path("data", "house_district_reg", "pdfs"), pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
csv_dir <- file.path("data", "house_district_reg", "csvs")

# Older reports use other layouts and were never converted; 03 reads 2014 on.
# A PDF that doesn't parse is reported and skipped
for(pdf_path in pdfs){
  csv_path <- file.path(csv_dir, str_replace(basename(pdf_path), "\\.pdf$", ".csv"))
  if(file.exists(csv_path)) next
  message(pdf_path)
  tryCatch(
    convert_registration_pdf(pdf_path, csv_path),
    error = \(e) warning(conditionMessage(e), call. = FALSE, immediate. = TRUE)
  )
}
