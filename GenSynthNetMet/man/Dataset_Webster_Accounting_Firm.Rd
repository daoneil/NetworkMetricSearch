\name{Dataset_Webster_Accounting_Firm}
\alias{Dataset_Webster_Accounting_Firm}
\docType{data}
\title{Webster_Accounting_Firm
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 24x24 symmetric weighted sociomatrix observed by C.M. Webster.
}
\usage{ds <- as.matrix(read.csv("../data/WebsterAcctObservedSocial_matrix_1.txt", header=FALSE))}
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
\url{http://moreno.ss.uci.edu/data.html#acct}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Webster, C. M. (1993). Task-related and context-based constraints in observed and reported relational data. Doctoral Dissertation, University of California, Irvine.
Webster, C. M. (1995). "Detecting context-based constraints in social perception." Journal of Quantitative Anthropology, 5:285-303.
}
\examples{
ds <- as.matrix(read.csv("../data/WebsterAcctObservedSocial_matrix_1.txt", header=FALSE))
}
\keyword{dataset}
