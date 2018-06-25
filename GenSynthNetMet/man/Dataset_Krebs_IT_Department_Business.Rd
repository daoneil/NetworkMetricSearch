\name{Dataset_Krebs_IT_Department_Business}
\alias{Dataset_Krebs_IT_Department_Business}
\docType{data}
\title{Krebs_IT_Department_Business
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 56x56 asymmetric weighted sociomatrix developed by Valdis Krebs based upon observations of a Fortune 500 Business IT Department.
}
\usage{ds <- as.matrix(read.csv("../data/KrebsITDept_matrix_only_1.txt", header=FALSE))}
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
\url{http://moreno.ss.uci.edu/data.html#krebs}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Chen C (2007) Social networks at Sempra Energy’s IT division are key to building strategic capabilities. Global Business and Organizational Excellence 26:16–24
}
\examples{
ds <- as.matrix(read.csv("../data/KrebsITDept_matrix_only_1.txt", header=FALSE))
}
\keyword{dataset}
