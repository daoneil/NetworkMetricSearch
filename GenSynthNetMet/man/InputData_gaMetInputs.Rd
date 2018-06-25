\name{InputData_gaMetInputs}
\alias{InputData_gaMetInputs}
\docType{data}
\title{gaMetInputs
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A Comma Separated Value file containing parameters and pathnames.
}
\usage{gaMetIns <- as.data.frame(read.csv("../data/gaMetInputs.csv",header=FALSE))}
\format{
  Columns are for parameters that define population size and time limit and pathnames for generated results and reports.
  \describe{
    \item{\code{column 1}}{population size}
    \item{\code{column 2}}{time limit}
    \item{\code{column 3}}{path to a real world sociomatrix}
    \item{\code{remaining columns}}{path names for }
  }
}

\examples{
Refer to the Genetic Algorithm vignette, which includes code that reads this file.
}
\keyword{inputdata}
