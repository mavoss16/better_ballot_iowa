

library(tabulapdf)
library(purrr)
library(fs)
library(janitor)


pdf_files <- dir_ls("data/house_district_reg", recurse = TRUE, regexp = "pdf$")

pdf_files <- pdf_files[(file_size(pdf_files) >= 50000) |> as.logical()]


files <- data.frame(file = pdf_files)

tables <- map(
  pdf_files, .f = function(.x){
    print(.x)
    extract_tables(.x)
  }
)

map_vec(tables, length)

files$table_num <- map_vec(tables, length)

files$table_name_match <- map_vec(tables, compare_df_cols_same)
