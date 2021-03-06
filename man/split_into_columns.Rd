% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/split_into_columns.R
\name{split_into_columns}
\alias{split_into_columns}
\title{Returns a data frame that contains the original data from the input character vector split over multiple columns.}
\usage{
split_into_columns(char_vector, num_rows)
}
\arguments{
\item{char_vector}{A character vector}

\item{num_rows}{The number of rows in the resulting data frame}
}
\value{
A data frame with the list wrapped over columns of length num_rows
}
\description{
Returns a data frame that contains the original data from the input character vector split over multiple columns.
}
\examples{
split_into_columns(data, 12)
}

