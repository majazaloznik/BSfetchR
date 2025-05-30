% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_data.R
\name{prepare_bs_data_for_insert}
\alias{prepare_bs_data_for_insert}
\title{Prepare BS data table for insertion}
\usage{
prepare_bs_data_for_insert(px_code, con, schema = "platform")
}
\arguments{
\item{px_code}{the original BS code (e.g. i_36_6as)}

\item{con}{Database connection}

\item{schema}{the schema to use for the connection, default is "platform"}
}
\value{
A list containing:
 - data: The processed data frame
 - table_id: The table ID
 - time_dimension: The name of the time dimension
 - interval_id: The interval ID
 - dimension_ids: The non-time dimension IDs
 - dimension_names: The names of the dimensions
}
\description{
Processes raw BS data into a format ready for database insertion,
handling BS-specific quirks like flags in time periods.
}
