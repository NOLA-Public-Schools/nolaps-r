# nolaps-r
R functions and data used across NOLA Public Schools.

1. Download R from https://cran.r-project.org/bin/windows/base/ and install.
1. Install tidyverse package with `install.packages("tidyverse")`. Other missing nolaps dependencies, like salesforcer, can be installed later with the same syntax.
1. Install nolaps package from the console or a command line session with `remotes::install_github("NOLA-Public-Schools/nolaps-r")`. You may need to update the GITHUB_PAT environment variable from the console with `credentials::set_github_pat()`.
1. Other missing credentials have to be added to the Windows credential manager from the console using the R command `keyring::key_set("[some key used in nolaps, like "googlemaps"]")`. Some keys will need two parts -- one for the overall service and one for the particular secret being specified, like `keyring::key_set("ldoe_test", "server")` or `keyring::key_set("ldoe_test", "password")`.
1. Although the GitHub repository is named "nolaps-r" to avoid confusion, the R package itself is just "nolaps", so you'll load it with `library(nolaps)`.
