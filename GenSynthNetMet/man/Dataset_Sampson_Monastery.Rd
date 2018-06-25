\name{Dataset_Sampson_Monastery}
\alias{Dataset_Sampson_Monastery}
\docType{data}
\title{Sampson_Monastery
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
An 18x18 asymmetric weighted sociomatrix observed by Sampson.
}
\usage{ds <- as.matrix(read.csv("../data/SampsonMonestary_matrix_1.txt", header=FALSE))}
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
\url{http://moreno.ss.uci.edu/data.html#sampson}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Breiger R., Boorman S. and Arabie P. (1975). An algorithm for clustering relational data with applications to social network analysis and comparison with multidimensional scaling. Journal of Mathematical Psychology, 12, 328-383.
Sampson, S. (1969). Crisis in a cloister. Unpublished doctoral dissertation, Cornell University.
}
\examples{
ds <- as.matrix(read.csv("../data/SampsonMonestary_matrix_1.txt", header=FALSE))
}
\keyword{dataset}
