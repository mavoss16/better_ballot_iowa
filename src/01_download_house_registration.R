
library(dplyr)
library(stringr)
library(fs)
library(httr2)

months <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

years <- 2008:2025

log <- data.frame(year = numeric(0), month = character(0), status = character(0))

for(year in years){
  year_substring <- str_sub(year, start = -2)
  dir_path <- file.path("data", "house_district_reg", "pdfs", year)
  dir_create(dir_path)
  
  for(month in months){
    
    month_num <- which(months == month)
    
    download_url <- paste0("https://sos.iowa.gov/elections/pdf/VRStatsArchive/", year, "/SH", month, year_substring, ".pdf")
    download_path <- file.path(dir_path, paste0(year, "-", str_pad(month_num, width = 2, side = "left", pad = "0"), ".pdf"))
    current_status <- try({
      request(download_url) |>
        req_headers(
          accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
          `accept-language` = "en-US,en;q=0.9",
          dnt = "1",
          priority = "u=0, i",
          `sec-ch-ua` = '"Google Chrome";v="135", "Not-A.Brand";v="8", "Chromium";v="135"',
          `sec-ch-ua-mobile` = "?1",
          `sec-ch-ua-platform` = '"Android"',
          `sec-fetch-dest` = "document",
          `sec-fetch-mode` = "navigate",
          `sec-fetch-site` = "none",
          `sec-fetch-user` = "?1",
          `upgrade-insecure-requests` = "1",
          `user-agent` = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Mobile Safari/537.36",
          cookie = "_ga=GA1.2.1325430264.1736718554; _ga_HC5JJR60WG=GS1.1.1742497360.11.0.1742497361.59.0.0"
        ) |>
        req_perform(path = download_path)
    }, silent = TRUE)
    
    error_tf <- class(current_status) == "try-error"

    if(error_tf){
      file_delete(download_path)
    }
    log <- bind_rows(
      log,
      data.frame(year = year, month = month, status = as.character(current_status), path = ifelse(error_tf, no = download_path, yes = NA))
    )
    
    print(paste0("Year: ", year, "; Month: ", month, "; Failed? ", error_tf))
    
    Sys.sleep(runif(1, 0, 1))
  }
  
}

readr::write_rds(log, "data/house_district_reg/pdfs/download_log.rds")
