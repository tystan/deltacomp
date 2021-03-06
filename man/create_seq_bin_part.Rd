% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_seq_bin_part.R
\name{create_seq_bin_part}
\alias{create_seq_bin_part}
\title{Create a sequential binary partition indication matrix}
\usage{
create_seq_bin_part(n_comp)
}
\arguments{
\item{n_comp}{the number of compositional variables to partition}
}
\value{
A \code{n_comp} by \code{n_comp - 1} matrix where each column relates to one partition.

The partitions are made so that the numerator (\code{1} values) for the \code{i}th column is in the \code{i}th row. 
All values below the \code{1} in the column are set to \code{-1} (the denominator).

The generated sequential binary partition for 3 comp vars is \code{(1, -1, -1), (0, 1, -1)}.

The generated sequential binary partition for 4 comp vars is \code{(1, -1, -1, -1), (0, 1, -1, -1), (0, 0, 1, -1)}.

etc
}
\description{
Create a sequential binary partition indication matrix
}
\examples{
create_seq_bin_part(3)
create_seq_bin_part(4)
create_seq_bin_part(10)
}
