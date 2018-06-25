\name{Dataset_Lazega_Law_Firm}
\alias{Dataset_Lazega_Law_Firm}
\docType{data}
\title{Lazega_Law_Firm}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 71x71 asymmetric unweighted sociomatrix based upon a network study of a corporate law firm.
}
\usage{
ds <- as.matrix(read.csv("../data/Lazega_LawFirm_matrix_2.txt", header=FALSE))
}
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
\url{http://moreno.ss.uci.edu/data.html#lazega}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Emmanuel Lazega, The Collegial Phenomenon: The Social Mechanisms of Cooperation Among Peers in a Corporate Law Partnership, Oxford University Press (2001).
Tom A.B. Snijders, Philippa E. Pattison, Garry L. Robins, and Mark S. Handcock. New specifications for exponential random graph models. Sociological Methodology (2006), 99-153.
}
\examples{
ds <- as.matrix(read.csv("../data/Lazega_LawFirm_matrix_2.txt", header=FALSE))
}
\keyword{dataset}
