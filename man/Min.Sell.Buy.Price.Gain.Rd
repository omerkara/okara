% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/personal_functions.R
\name{Min.Sell.Buy.Price.Gain}
\alias{Min.Sell.Buy.Price.Gain}
\title{Minimum Sell or Buy Prices}
\usage{
Min.Sell.Buy.Price.Gain(Price, Gain.Per = 1, Fee.Per = 0.1, Round = 2)
}
\arguments{
\item{Price}{numeric. Buy or sell price.}

\item{Gain.Per}{numeric. Gain percentage. Default is 1.}

\item{Fee.Per}{numeric. Fee percentage. Default is 0.1.}

\item{Round}{numeric. Decimal rounding. Default is 2.}
}
\value{
Two messages: one for BUY Price, one for SELL Price.
}
\description{
This function gives you the minimum sell price after you buy or the minimum buy price after you sell with a selected fee percentage and desired gain percentage.
}
\details{
This is a personal function of the package author. So, it won't make sense to others.
}
\examples{
Min.Sell.Buy.Price.Gain(Price = 100)
Min.Sell.Buy.Price.Gain(Price = 100, Gain.Per = 1.5)
Min.Sell.Buy.Price.Gain(Price = 100, Fee.Per = 0.01)
Min.Sell.Buy.Price.Gain(Price = 100, Gain.Per = 2.5, Fee.Per = 0.05, Round = 4)

}
\author{
\href{mailto:omer.kara.ylsy@gmail.com}{Omer Kara}
}
