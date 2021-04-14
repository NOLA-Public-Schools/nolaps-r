exclude <- readxl::read_excel("data-raw/exclude.xlsx", col_types = "text")

usethis::use_data(exclude, overwrite = TRUE)
