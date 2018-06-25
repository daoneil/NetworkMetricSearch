\name{Dataset_Thurman_Office}
\alias{Dataset_Thurman_Office}
\docType{data}
\title{Thurman_Office
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 15x15 symmetric unweighted sociomatrix.

}
\usage{ds <- as.matrix(read.csv("../data/Thurman_Office_matrix_2.txt", header=FALSE))}
\format{
  A plain text tab delimited file with ones and zeros where the ones represent links.
  \describe{
    \item{\code{rows}}{represent individuals}
    \item{\code{columns}}{represent individuals}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
This dataset wase produced from a file that is bundled with the UCINet software.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
\url{https://sites.google.com/site/ucinetsoftware/datasets/thurmanoffice}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Thurman B. (1979). In the office: Networks and coalitions. Social Networks, 2, 47-63.
}
\examples{
ds <- as.matrix(read.csv("../data/Thurman_Office_matrix_2.txt", header=FALSE))
}
\keyword{dataset}
