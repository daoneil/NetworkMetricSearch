\name{Dataset_Schwimmer_Taro_Exchange}
\alias{Dataset_Schwimmer_Taro_Exchange}
\docType{data}
\title{Schwimmer_Taro_Exchange
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A 22x22 symmetric unweighted sociomatrix developed by Schwimmer based upon observations of gift exchanges among housholds in a Papuan village.
}
\usage{ds <- as.matrix(read.csv("../data/Schwimmer_Taro_Exchange.txt", header=FALSE))}
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
\url{http://moreno.ss.uci.edu/data.html#taro}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Hage P. and Harary F. (1983). Structural models in anthropology. Cambridge: Cambridge University Press.
Schwimmer E. (1973). Exchange in the social structure of the Orokaiva. New York: St Martins.
}
\examples{
ds <- as.matrix(read.csv("../data/Schwimmer_Taro_Exchange.txt", header=FALSE))
}
\keyword{dataset}
