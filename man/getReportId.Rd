\name{getReportId}
\alias{getReportId}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This generates the accessToken using a refresh token or authorization code.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getReportId(credentials, report, columns, startDate, endDate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{credentials}{
%%     ~~Describe \code{x} here~~
credentials is a list of all the important parameters required for sending a report request. 
It is the list of client_id, client_secret, username, password, developer_token, customer_id, account_id.

report is the name of the report request you are making. For more info: https://docs.microsoft.com/en-us/bingads/reporting-service/submitgeneratereport?view=bingads-12
Example: report <- "CampaignPerformanceReport"
Acceptable values: AccountPerformanceReport, PerformanceReport, AdDynamicTextPerformanceReport, AdGroupPerformanceReport, AdPerformanceReport, KeywordPerformanceReport, DestinationUrlPerformanceReport, BudgetSummaryReport, AgeGenderDemographicReport, AgeGenderAudienceReport, ProfessionalDemographicsAudienceReport, UserLocationPerformanceReport, PublisherUsagePerformanceReport, SearchQueryPerformanceReport, ConversionPerformanceReport, GoalsAndFunnelsReport, NegativeKeywordConflictReport, SearchCampaignChangeHistoryReport, AdExtensionByAdReport, AdExtensionByKeywordReport, AudiencePerformanceReport, AdExtensionDetailReport, ShareOfVoiceReport, ProductDimensionPerformanceReport, ProductPartitionPerformanceReport, ProductPartitionUnitPerformanceReport, ProductSearchQueryPerformanceReport, ProductMatchCountReport, CallDetailReport, GeographicPerformanceReport, DSASearchQueryPerformanceReport, DSAAutoTargetPerformanceReport, DSACategoryPerformanceReport.

columns is a character array of valid columns for a report.
Example: columns <- c("AccountName", "CampaignName", "TimePeriod", "Impressions", "Clicks", "Spend", "Conversions")
Acceptable values: The columns valid for that particular reports are accepted.

startDate is the start date as a character input in the format yyyy-mm-dd
Example: startDate <- "2018-01-01"

endDate is the end date as a character input in the format yyyy-mm-dd
Example: endDate <- "2018-01-01"



}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This generates the final reportId in a character format. This reportId has to be send to downloadURL to download the csv file.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
