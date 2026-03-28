library(tabulapdf)

pdf_dir <- "Voter Registration/data_pdf"
csv_dir <- "Voter Registration/data_csv"

pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

for (pdf_file in pdf_files) {
  base_name <- tools::file_path_sans_ext(basename(pdf_file))
  csv_path  <- file.path(csv_dir, paste0(base_name, ".csv"))

  if (file.exists(csv_path)) {
    message("Already extracted, skipping: ", basename(pdf_file))
    next
  }

  tables <- extract_tables(pdf_file, output = "matrix")

  if (length(tables) == 0) {
    message("No tables found: ", basename(pdf_file))
    next
  }

  # First table: keep all rows (includes header)
  # Subsequent tables: drop first row (duplicate header)
  combined <- do.call(rbind, c(
    list(tables[[1]]),
    lapply(tables[-1], function(t) t[-1, , drop = FALSE])
  ))

  write.csv(combined, csv_path, row.names = FALSE)
  message("Written: ", basename(csv_path))
}
