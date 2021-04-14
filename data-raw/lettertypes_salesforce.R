lettertypes_salesforce <- readxl::read_excel("data-raw/lettertypes_salesforce.xlsx")

usethis::use_data(lettertypes_salesforce, overwrite = TRUE)
