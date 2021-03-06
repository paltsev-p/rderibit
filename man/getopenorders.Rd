% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deribit_api.R
\name{getopenorders}
\alias{getopenorders}
\title{Lists open orders}
\usage{
getopenorders(keys, instrument = NULL, orderId = NULL, simplify = TRUE)
}
\arguments{
\item{keys}{list containing access and  secret keys in the form: keys <- list(key = ACCESS_KEY, secret = ACCESS_SECRET)}

\item{instrument}{(optional) Instrument name}

\item{orderId}{(optional)}

\item{simplify}{Boolean, defaults to TRUE}
}
\description{
Lists open orders
}
