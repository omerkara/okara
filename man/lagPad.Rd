% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_functions.R
\name{lagPad}
\alias{lagPad}
\title{Lag Function with Padding}
\usage{
lagPad(x, k)
}
\arguments{
\item{x}{numeric. A numeric vector.}

\item{k}{numeric. A numeric lag value.}
}
\value{
A lag taken numeric vector with padding.
}
\description{
This function pads the lag taken numeric vector with NA values.
}
\examples{
x <- 1:10
x.t1 <- lagPad(x, 2)
cbind(x, x.t1)

}
\author{
\href{mailto:omer.kara.ylsy@gmail.com}{Omer Kara}
}
