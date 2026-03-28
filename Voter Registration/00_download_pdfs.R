library(glue)
library(httr2)

pdf_dir <- "Voter Registration/data_pdf"
base_url <- "https://sos.iowa.gov/elections/pdf/VRStatsArchive"

month_abbrs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Build list of (year, month_abbr, yy) for months not yet downloaded
# Existing data ends at CoJan25.pdf; download Feb 2025 through Mar 2026
to_download <- c(
  glue("Co{month_abbrs[2:12]}25"),   # Feb–Dec 2025
  glue("Co{month_abbrs[1:3]}26")     # Jan–Mar 2026
)

years <- c(rep(2025, 11), rep(2026, 3))

for (i in seq_along(to_download)) {
  filename <- paste0(to_download[i], ".pdf")
  out_path  <- file.path(pdf_dir, filename)
  url       <- glue("{base_url}/{years[i]}/{filename}")

  if (file.exists(out_path)) {
    message("Already exists, skipping: ", filename)
    next
  }

  tryCatch(
    {
      request(url) |>
        req_headers(
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
          `Referer`    = "https://sos.iowa.gov/"
        ) |>
        req_perform() |>
        resp_body_raw() |>
        writeBin(out_path)
      message("Downloaded: ", filename)
    },
    error = function(e) message("Failed: ", filename, " — ", conditionMessage(e))
  )
}
