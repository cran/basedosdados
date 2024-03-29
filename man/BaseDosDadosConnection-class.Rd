% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bd_connection.R
\docType{class}
\name{BaseDosDadosConnection-class}
\alias{BaseDosDadosConnection-class}
\alias{dbplyr_edition.BaseDosDadosConnection}
\title{Base dos dados specific connection to BigQuery}
\usage{
\method{dbplyr_edition}{BaseDosDadosConnection}(con)
}
\arguments{
\item{con}{A BaseDosDadosConnection object.}
}
\description{
Implements a connection class to prevent a bug caused when trying to use
BigQueryConnection. The bug is described in Pull Request #1563. Once fixed,
this class and its method should probably be removed.
}
