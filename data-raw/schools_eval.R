schools_eval <- readxl::read_excel("data-raw/schools_eval.xlsx")

usethis::use_data(schools_eval, overwrite = TRUE)
