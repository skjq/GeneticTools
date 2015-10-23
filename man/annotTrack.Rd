\name{annotTrack}
\alias{annotTrack}
\docType{data}
\title{Example Annotation Track}
\description{
  Example annotation track.
}
\usage{data(annotTrack)}

\format{
  A data frame with 1001 rows, each representing one annotation with columns as in standard gtf format. 
}

\details{
  This is an example Bos Taurus (Cow) annotation track as it was imported from the Ensembl ftp download page. In total there are 1001 annotations from   
  the bovine genome, release 76.
  The data was imported with the \code{hoardeR::importGTF} function.
}

\source{
  http://www.ensembl.org/info/data/ftp/index.html
}

\seealso{
  \link[hoardeR]{importGTF}
}

\examples{
data(annotTrack)
head(annotTrack)
}
\keyword{datasets}
 
