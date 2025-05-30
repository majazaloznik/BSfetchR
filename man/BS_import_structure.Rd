% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importing.R
\name{BS_import_structure}
\alias{BS_import_structure}
\title{Import structural metadata for a BS table}
\usage{
BS_import_structure(
  px_code,
  con,
  schema = "platform",
  all_levels = TRUE,
  keep_vintage = FALSE
)
}
\arguments{
\item{px_code}{BS code name of the table}

\item{con}{connection to database}

\item{schema}{schema name, defaults to "platform"}

\item{all_levels}{logical whether to let user use dimension selector, default
is true, which automatically selects all levels in all dimensions}

\item{keep_vintage}{logical indicating whether to keep vintages, defaults to F}
}
\value{
nothing
}
\description{
Umbrella function that prepares and import all the metadata tables into
the database. It uses the functions from the UMARimportR package to
insert the data into the database.
}
