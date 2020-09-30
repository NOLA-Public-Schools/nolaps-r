


salesforcequery <- NULL



.onLoad <- function(libname, pkgname) {

  salesforcequery <<- reticulate::import("salesforcequery", delay_load = TRUE)

}


