% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{convert_specific_column_names_to_data_type}
\alias{convert_specific_column_names_to_data_type}
\title{Create a vector of data types based on column names. Built so we can add in as many types as we want.}
\usage{
convert_specific_column_names_to_data_type(
  column_name_vector,
  text_names = NULL,
  numeric_names = NULL,
  skip_names = NULL,
  guess_names = NULL,
  logical_names = NULL,
  date_names = NULL,
  list_names = NULL
)
}
\arguments{
\item{column_name_vector}{Column names vector for the dataframe}

\item{text_names}{Which column names should be labeled as text?}

\item{numeric_names}{Which column names should be labeled as numeric?}

\item{skip_names}{Which column names should be labeled as skip?}

\item{guess_names}{Which column names should be labeled as guess?}

\item{logical_names}{Which column names should be labeled as logical?}

\item{date_names}{Which column names should be labeled as date?}

\item{list_names}{Which column names should be labeled as list?}
}
\value{
data_types
}
\description{
Create a vector of data types based on column names. Built so we can add in as many types as we want.
}
