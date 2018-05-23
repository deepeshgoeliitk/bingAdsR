\name{getDataFromURL}
\alias{getDataFromURL}
\title{
This generates the download url for the report requested using Poll Generate Report.
}
\description{
This function generate dataframe after download the zipped file from the downloadUrl obtained from downloadUrl function.
}
\usage{
getDataFromURL(downloadUrl)
}
\arguments{
  \item{downloadUrl}{
This is the final url which when put in browser downloads the zipped version of the csv report.
}
}
\details{
}
\value{
\item{df}{The is the final data of the report requested.}
}
\references{
}
\author{
Deepesh Goel
}
\note{
}
\seealso{
}
\examples{
##Not run
df <- getDataFromURL(downloadUrl)
##End(Not run)
}
\keyword{ ~bing ads reporting api in r}
\keyword{ ~bing ads api r}
\keyword{ ~bing get report id}
\keyword{ ~bing api reporting data}
