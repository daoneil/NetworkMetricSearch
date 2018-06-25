\name{InputData_CustomCompatibilityTable_MBTI}
\alias{InputData_CustomCompatibilityTable_MBTI}
\docType{data}
\title{Custom Compatibility Table for MBTI
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A Myers Briggs Type Indicator (MBTI) compatibility table.  Values in the table represent a likelihood for link formation.
}
\usage{data("InputData_CustomCompatibilityTable_MBTI")}
\format{
  Column headers identify MBTI profiles.  Rows represent MBTI profiles.
  \describe{
    \item{\code{x,y}}{a likelihood value for probable link formation}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
The author developed this compatibility table based upon an analysis of the MBTI profiles described in Dr. David Kiersey's book 'Please Understand Me II'. The analysis invovled identification of work environment factors and assessing whether each MBTI would have a high or low opinion of those factors.  Pairwise comparisons identified the number of opinions in common and the percentages of those common opinions were scaled to fit a normal distribution.
}

\references{
%%  ~~ possibly secondary sources and usages ~~
Keirsey, D. (1998). Please understand me 2. Prometheus Nemesis Book Company.
}
\examples{
Refer to the vignettes, which read this data file.
}
\keyword{inputdata}
