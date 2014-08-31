\name{annotTrack}
\alias{annotTrack}
\docType{data}
\title{Example Annotation Track}
\description{
  Example annotation track.
}
\usage{data(annotTrack)}

\format{
  A data frame with 1001 rows, each representing one annotation with columns as provided from standard gtf format. 
}

\details{
  This is an example bos taurus (Cow) annotation track as it was imported from the Ensemble ftp download page. In total there are 1001 annotations from the bovine genome in release 76.
  The data was imported with the \code{hoardeR::importGTF} function.
}

\source{
  http://www.ensembl.org/info/data/ftp/index.html
}

\seealso{
  \link{\code{hoardeR::importGTF}}
}

\examples{
data(annotTrack)
head(annotTrack)
library(hoardeR)
## Not run:
importGTF(gtf="Bos_taurus.UMD3.1.76.gtf", nrow=1001, skip=5)
## End(Not Run)
}
\keyword{datasets}
 
