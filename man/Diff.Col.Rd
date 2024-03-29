% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_functions.R
\name{Diff.Col}
\alias{Diff.Col}
\title{Difference of Subset Columns}
\usage{
Diff.Col(Data, Diff.ColNames)
}
\arguments{
\item{Data}{ts object.}

\item{Diff.ColNames}{character. Name of the columns to be differenced.}
}
\value{
A first difference taken ts object with a user selected name.
}
\description{
This function takes the difference of specified columns in a ts object. It should not be used for taking differences of all variables. Only a subset of variables can be used. Other variables are bind to the differenced data at the end.
}
\examples{
x <- data.frame(V1 = c(1:10), V2 = c(1:10), V3 = c(1:10))
x.ts <- ts(x)
data.diff <- Diff.Col(Data = x.ts, Diff.ColNames = c("V1", "V3"))
data.diff

}
\author{
\href{mailto:omer.kara.ylsy@gmail.com}{Omer Kara}
}
