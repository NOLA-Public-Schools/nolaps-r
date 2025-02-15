% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getdata_salesforce_clean.R
\name{getdata_ep_choice}
\alias{getdata_ep_choice}
\title{Get eligibility/priority records by choice}
\usage{
getdata_ep_choice(term = term_next(), date_start = date_appstart())
}
\arguments{
\item{term}{character like "2020-2021"}

\item{date_start}{character in datetime format like "2020-01-01T00:00:00Z"}
}
\value{
tibble with new field names
}
\description{
Get eligibility/priority records by choice
}
