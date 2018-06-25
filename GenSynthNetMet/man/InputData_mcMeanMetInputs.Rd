\name{InputData_mcMeanMetInputs}
\alias{InputData_mcMeanMetInputs}
\docType{data}
\title{mcMeanMetInputs
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A Comma Separated Value file containing parameters and pathnames to be used by the Monte Carlo based search function.
}
\usage{mcMetIns <- as.data.frame(read.csv("../data/mcMeanMetInputs.csv",header=FALSE))}
\format{
  A CSV file the specifies a time limit, a real world sociomatrix, and pathnames for generated results and reports.
  \describe{
    \item{\code{column 1}}{time limit}
    \item{\code{column 2}}{a numeric vector}
    \item{\code{remaining columns}}{pathnames for generated results and reports}
  }
}

\references{
%%  ~~ possibly secondary sources and usages ~~
Refer to other sections of this manual for descriptions of the datasets containing real world sociomatrices.
}
\examples{
Refer to the Monte Carlo vignette, which includes code that reads this file.
}
\keyword{inputdata}
