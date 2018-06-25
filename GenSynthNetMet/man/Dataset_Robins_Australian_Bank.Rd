\name{Dataset_Robins_Australian_Bank}
\alias{Dataset_Robins_Australian_Bank}
\docType{data}
\title{Robins_Australian_Bank}
\description{
An 11x11 binary asymmetric tab delimited plain text file.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{dataset <- as.matrix(read.csv("../data/Robins_Australian_Bank.txt", header=FALSE))}
\format{
  A plain text tab delimited file with ones and zeros where the ones represent links.
  \describe{
    \item{\code{rows}}{represent individuals}
    \item{\code{columns}}{represent individuals}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
Based upon data gathered by Garry Robins. This dataset was produced from a file that was downloaded from a repository of data sets maintained by Dr. Linton Freeman.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
 \url{http://moreno.ss.uci.edu/data.html#bank}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Pattison P., S. Wasserman, G. Robins and A. M. Kanfer. 2000. "Statistical Evaluation of Algebraic Constraints for Social Networks." Journal of Mathematical Psychology, 44: 536-568.
}
\examples{
rab <- as.matrix(read.csv("../data/Robins_Australian_Bank.txt", header=FALSE))
}
\keyword{dataset}
