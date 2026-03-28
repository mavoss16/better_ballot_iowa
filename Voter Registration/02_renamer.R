csv_dir <- "Voter Registration/data_csv"

month_mapping <- c(
  January = "01", February = "02", March = "03", April = "04",
  May = "05", June = "06", July = "07", August = "08",
  Sept = "09", September = "09", October = "10", November = "11", December = "12",
  Jan = "01", Feb = "02", Mar = "03", Apr = "04",
  Jun = "06", Jul = "07", Aug = "08",
  Sep = "09", Oct = "10", Nov = "11", Dec = "12"
)

files <- list.files(csv_dir, pattern = "^Co[A-Za-z]+\\d{2}\\.csv$")

for (filename in files) {
  match <- regmatches(filename, regexec("^Co([A-Za-z]+)(\\d{2})\\.csv$", filename))[[1]]

  if (length(match) != 3) next

  month_name <- match[2]
  year       <- match[3]
  month_num  <- month_mapping[month_name]

  if (is.na(month_num)) {
    message("Month name not found for: ", filename)
    next
  }

  new_filename <- paste0("20", year, "-", month_num, ".csv")
  file.rename(
    file.path(csv_dir, filename),
    file.path(csv_dir, new_filename)
  )
  message("Renamed: ", filename, " -> ", new_filename)
}
