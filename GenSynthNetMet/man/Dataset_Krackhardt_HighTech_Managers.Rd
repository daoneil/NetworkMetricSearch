\name{Dataset_Krackhardt_HighTech_Managers}
\alias{Dataset_Krackhardt_HighTech_Managers}
\docType{data}
\title{Krackhardt_HighTech_Managers
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 21x21 symmetric unweighted sociomatrix developed by Krackhardt based upon data collected from managers withing a high tech company.
}
\usage{ds <- as.matrix(read.csv("../data/KrackhardtMgrsFriendship_matrix_1.txt", header=FALSE))}
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
\url{http://moreno.ss.uci.edu/data.html#krackht}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Krackhardt D. (1987). Cognitive social structures. Social Networks, 9, 104-134.
}
\examples{
ds <- as.matrix(read.csv("../data/KrackhardtMgrsFriendship_matrix_1.txt", header=FALSE))
}
\keyword{dataset}
