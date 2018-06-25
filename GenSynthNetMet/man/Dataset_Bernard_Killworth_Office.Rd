\name{Dataset_Bernard_Killworth_Office}
\alias{Dataset_Bernard_Killworth_Office}
\docType{data}
\title{Bernard_Killworth_Office
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 40x40 symmetric weighted sociomatrix observered by Bernard & Killworth with the help of Sailer.
}
\usage{ds <- as.matrix(read.csv("../data/BernardKillworth_OfficeInteractions.txt", header=FALSE))}
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
\url{http://moreno.ss.uci.edu/data.html#bkoff}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Bernard H. R., Killworth P. and Sailer L. (1980). Informant accuracy in social network data IV. Social Networks, 2, 191-218.
Bernard H. R., Killworth P. and Sailer L. (1982). Informant accuracy in social network data V. Social Science Research, 11, 30-66.
}
\examples{
ds <- as.matrix(read.csv("../data/BernardKillworth_OfficeInteractions.txt", header=FALSE))
}
\keyword{dataset}
