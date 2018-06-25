\name{Dataset_Roethlisberger_Dickson_Wiring_Room}
\alias{Dataset_Roethlisberger_Dickson_Wiring_Room}
\docType{data}
\title{Roethlisberger_Dickson_Wiring_Room
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Observations of 14 employees in the Western Electric Hawthorne bank wiring room. A 14x14 plain text symmetric unweighted sociomatrix. Data file was downloaded from a repository of data sets maintained by Dr. Linton Freeman.
}
\usage{ds <- as.matrix(read.csv("../data/HawthornPlant_WiringRoom.txt", header=FALSE))}
\format{
  A plain text tab delimited file with ones and zeros where the ones represent links.
  \describe{
    \item{\code{rows}}{represent individuals}
    \item{\code{columns}}{represent individuals}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
\url{http://moreno.ss.uci.edu/data.html#wiring}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Roethlisberger F. and Dickson W. (1939). Management and the worker. Cambridge: Cambridge University Press.
}
\examples{
ds <- as.matrix(read.csv("../data/HawthornPlant_WiringRoom.txt", header=FALSE))
}
\keyword{dataset}
