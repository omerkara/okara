% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_functions.R
\name{northarrow}
\alias{northarrow}
\title{Adding North Arrow to Maps}
\usage{
northarrow(loc, size, bearing = 0, cols, cex = 1, ...)
}
\arguments{
\item{loc}{A two value numeric vector. The longitude and latitude where the north arrow is to be.}

\item{size}{numeric. Size of the north arrow.}

\item{bearing}{numeric. Angle of the north arrow.}

\item{cols}{character. Color of the north arrow.}

\item{cex}{numeric. Size of the letters.}

\item{...}{Other arguments}
}
\value{
A sp::plot object with a north arrow.
}
\description{
This function adds a north arrow to a map created with sp::plot.
}
\examples{
\dontrun{
northarrow(c(-124, 26), size = 1, cex = 1, bearing = 0) ## Do not run.
}

}
\references{
Taken from Auxiliary Cartographic Functions in R: North Arrow, Scale Bar, and Label with a Leader Arrow, Tanimura, 2007. For more information see \href{https://www.jstatsoft.org/article/view/v019c01/v19c01.pdf}{here}.
}
\author{
\href{mailto:omer.kara.ylsy@gmail.com}{Omer Kara}
}
