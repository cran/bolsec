\name{import_data}
\alias{import_data}
\title{Imports daily data of varying securities.
}
\description{Primary function that imports daily data of securities presenting the highest and lowest price variations in the Bolivian Securities Exchange.
}
\usage{
  import_data(verbose = TRUE)
}
\arguments{
  \item{verbose}{Logical. If \code{TRUE}, display messages about the data import process. Default is \code{TRUE}.}
}
\value{
{TICKER SYMBOL} {This column lists the code names of each traded security.}
{ISSUER NAME} {This column lists the company names that issue each traded security.}
{PRICE} {This column lists the today's prices of each traded security.}
{RATE} {This column lists the percentage interest rate of each traded security.}
{VAR.} {This column lists the price variations between yesterday and today for each traded security.}
}
\examples{
# Import market data

data <- import_data()

head(data)
}
\references{The Bolivian Stock Exchange (Bolsa Boliviana de Valores), based in La Paz, Bolivia, offers trading of equities, indexes, gold, and local commodities.
}
\author{Alvaro Ignacio Tapia
}
