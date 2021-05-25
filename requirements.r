#!/usr/bin/env Rscript
install.packages("tidyverse",repos="https://cloud.r-project.org/")
install.packages("remotes",repos="https://cloud.r-project.org/")
install.packages("credentials",repos="https://cloud.r-project.org/")
install.packages("keyring",repos="https://cloud.r-project.org/")
credentials::set_github_pat()
remotes::install_github("NOLA-Public-Schools/nolaps-r")
