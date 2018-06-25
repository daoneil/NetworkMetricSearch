\name{Dataset_Zachary_Karate_Club}
\alias{Dataset_Zachary_Karate_Club}
\docType{data}
\title{Zachary_Karate_Club
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 34x34 asymmetric unweighted sociomatrix, observed by Wayne Zachary, of a university karate club.
}
\usage{ds <- as.matrix(read.csv("../data/Zachary_Karate_Club.txt", header=FALSE))}
\format{
  A plain text tab delimited file with ones and zeros where the ones represent links.
  \describe{
    \item{\code{rows}}{represent individuals}
    \item{\code{columns}}{represent individuals}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
This dataset was produced from a file that was downloaded from a repository of data sets maintained by Dr. Linton Freeman.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
\url{http://moreno.ss.uci.edu/data.html#zachary}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Zachary W. (1977). An information flow model for conflict and fission in small groups. Journal of Anthropological Research, 33, 452-473.
}
\examples{
ds <- as.matrix(read.csv("../data/Zachary_Karate_Club.txt", header=FALSE))
}
\keyword{dataset}
