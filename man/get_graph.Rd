\name{get_graph}
\alias{get_graph}
\title{Plots correlation of varying securities.}
\description{This daily scatter plot illustrates the correlation between rate and price variations of securities in the Bolivian Securities Exchange.}
\usage{get_graph(data)}
\arguments{\item{data}{A defined variable storing the daily data from the import_data function.}}
\value{
  \item{ISSUER NAME}{Above the scatter dots are the company names that issue each traded      security.}
  \item{COEFFICIENT}{Red colored number with two decimals, representing the correlation of     all traded securities.}
  \item{VAR.}{Lying on the x-axis, range of the price variations between yesterday and today   from all traded securities.}
  \item{RATE}{Lying on the y-axis, range of the percentage interest rates of all traded       securities.}
}
\examples{data <- import_data()
get_graph(data)}
\references{The Bolivian Stock Exchange (Bolsa Boliviana de Valores), based in La Paz, Bolivia, offers trading of equities, indexes, gold, and local commodities.}
\author{Alvaro Ignacio Tapia}
