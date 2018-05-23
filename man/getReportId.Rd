\name{getReportId}
\alias{getReportId}
\title{
This generates the accessToken using a refresh token or authorization code.
}
\description{
This function get the report id for the report you are trying to generate. Using this report id, you can download the report in csv format using getDownloadUrl function. Using this downloadUrl, you can the data in a dataframe using getDataFromURL.
}
\usage{
getReportId(credentials, report, columns, startDate, endDate)
}
\arguments{
  \item{credentials}{
The credentials is a list of all the important parameters required for sending a report request. 
It is the list of client_id, client_secret, username, password, developer_token, customer_id, account_id, access_token.

The report is the name of the report request you are making. For more info: https://docs.microsoft.com/en-us/bingads/reporting-service/submitgeneratereport?view=bingads-12
Example: report <- "CampaignPerformanceReport"
Acceptable values: AccountPerformanceReport, PerformanceReport, AdDynamicTextPerformanceReport, AdGroupPerformanceReport, AdPerformanceReport, KeywordPerformanceReport, DestinationUrlPerformanceReport, BudgetSummaryReport, AgeGenderDemographicReport, AgeGenderAudienceReport, ProfessionalDemographicsAudienceReport, UserLocationPerformanceReport, PublisherUsagePerformanceReport, SearchQueryPerformanceReport, ConversionPerformanceReport, GoalsAndFunnelsReport, NegativeKeywordConflictReport, SearchCampaignChangeHistoryReport, AdExtensionByAdReport, AdExtensionByKeywordReport, AudiencePerformanceReport, AdExtensionDetailReport, ShareOfVoiceReport, ProductDimensionPerformanceReport, ProductPartitionPerformanceReport, ProductPartitionUnitPerformanceReport, ProductSearchQueryPerformanceReport, ProductMatchCountReport, CallDetailReport, GeographicPerformanceReport, DSASearchQueryPerformanceReport, DSAAutoTargetPerformanceReport, DSACategoryPerformanceReport.

The columns is a character array of valid columns for a report.
Example: columns <- c("AccountName", "CampaignName", "TimePeriod", "Impressions", "Clicks", "Spend", "Conversions")
Acceptable values: The columns valid for that particular reports are accepted.

The startDate is the start date as a character input in the format yyyy-mm-dd
Example: startDate <- "2018-01-01"

The endDate is the end date as a character input in the format yyyy-mm-dd
Example: endDate <- "2018-01-01"

}
}
\details{
}
\value{
\item{reportId }{This generates the final reportId in a character format. This reportId has to be send to downloadURL to download the csv file.}
}
\references{
https://docs.microsoft.com/en-us/bingads/reporting-service/submitgeneratereport?view=bingads-12
}
\author{
Deepesh Goel
}
\note{
}
\seealso{
https://docs.microsoft.com/en-us/bingads/reporting-service/submitgeneratereport?view=bingads-12
}
\examples{
##Not run
reportId <- getReportId(credentials, report, columns, startDate, endDate)
##End(Not run)
}
\keyword{ ~bing ads reporting api in r}
\keyword{ ~bing ads api r}
\keyword{ ~bing get report id}
\keyword{ ~bing api submit generate report}
