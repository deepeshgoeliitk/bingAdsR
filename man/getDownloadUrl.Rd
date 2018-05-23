\name{getDownloadUrl}
\alias{getDownloadUrl}
\title{
This generates the download url for the report requested using Poll Generate Report.
}
\description{
This function generate the url using which the report can be downloaded in the csv format. Using this downloadUrl, you can the data in a dataframe using getDataFromURL.
}
\usage{
getDownloadUrl(credentials, reportId)
}
\arguments{
  \item{credentials}{
The credentials is a list of all the important parameters required for sending a report request. 
It is the list of client_id, client_secret, username, password, developer_token, customer_id, account_id, access_token.

The reportId is a unique long number generated in getReportId function.

}
}
\details{
}
\value{
\item{downloadUrl }{This is the final url which when put in browser downloads the zipped version of the csv report.}
}
\references{
https://docs.microsoft.com/en-us/bingads/reporting-service/pollgeneratereport?view=bingads-12
}
\author{
Deepesh Goel
}
\note{
}
\seealso{
https://docs.microsoft.com/en-us/bingads/reporting-service/pollgeneratereport?view=bingads-12
}
\examples{
##Not run
downloadUrl <- getDownloadUrl(credentials, reportId)
##End(Not run)
}
\keyword{ ~bing ads reporting api in r}
\keyword{ ~bing ads api r}
\keyword{ ~bing get report id}
\keyword{ ~bing api poll generate report}
