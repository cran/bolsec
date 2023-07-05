\name{get_stats}
\alias{get_stats}
\title{Descriptive statistics of varying securities.
}
\description{This daily chart exhibits the descriptive statistics of price, rate and price variation from securities in the Bolivian Securities Exchange.
}
\usage{
get_stats(data)
}
\arguments{\item{data}{A defined variable storing the daily data from the import_data function.}
}
\value{
{MEAN}{This row displays the mean calculations of today's prices, rates, and price variations of all traded securities.}
{MEDIAN}{This row displays the median calculations of today's prices, rates, and price variations of all traded securities.}
{ST. DEV.}{This row displays the standard deviation calculations of today's prices, rates, and price variations of all traded securities.}
{MIN.}{This row displays the minimum value of today's prices, rates, and price variations of all traded securities.}
{MAX.}{TThis row displays the maximum value of today's prices, rates, and price variations of all traded securities.}
}
\examples{

data <- import_data()

get_stats(data)

}
\references{The Bolivian Stock Exchange (Bolsa Boliviana de Valores), based in La Paz, Bolivia, offers trading of equities, indexes, gold, and local commodities.
}
\author{Alvaro Ignacio Tapia
}
