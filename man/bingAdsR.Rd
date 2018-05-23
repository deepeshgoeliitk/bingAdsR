\name{bingAdsR}
\alias{bingAdsR}
\title{
The aim of the package is to load bing ads reporting data in R. This requires 4 major steps:
\strong{1. Authentication} - Oauth Authentication
\strong{2. Generating the report Id} for the report sought - SOAP Request using XMLs
\strong{3. Generating the download url} - SOAP Request using XMLs
\strong{4. Download} the zipped data, read it and present it in a data frame
}
\description{
This function starts the authentication process with Microsoft Bing. 
One would need to set up a developer account to use the api. Follow the process here: https://docs.microsoft.com/en-us/bingads/guides/customer-accounts?view=bingads-12#accountpermissions

https://developers.bingads.microsoft.com/Account

}
\examples{
	##Not run
	rm(list=ls())
	library(devtools)
	install_github("deepeshgoeliitk/bingAdsR")
	library(bingAdsR)

	##Variables##
	credentials <- list(
	  ###For authorization and access token###
	  client_id = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
	  client_secret = "XXXXXXXXXXXXXXX",
	  ###To get data###
	  username = "abc@abc.com",
	  password = "***********",
	  developer_token = "XXXXXXXXXXXXXXXXX",
	  customer_id = "XXXXXXXXXX",
	  account_id = "XXXXXXX"
	)

	####AUTHENTICATION#####
	credentials <- baAuthentication(credentials)

	#####Generate the data
	startDate <- "2018-05-17"
	endDate <- "2018-05-17"
	report <- "CampaignPerformanceReport"
	columns <- c("AccountName", "CampaignName", "TimePeriod", "Impressions", "Clicks", "Spend", "Conversions")

	reportId <- getReportId(credentials, report, columns, startDate, endDate)
	downloadUrl <- getDownloadUrl(credentials, reportId)
	df <- getDataFromURL(downloadUrl)


	report <- "AccountPerformanceReport"
	columns <- c("AccountName", "DeviceType", "TimePeriod", "Impressions", "Clicks", "Spend", "Conversions")
	reportId <- getReportId(credentials, report, columns, startDate, endDate)
	downloadUrl <- getDownloadUrl(credentials, reportId)
	df <- getDataFromURL(downloadUrl)
	##End(Not run)
}
\keyword{ ~bing ads reporting api in R }
\keyword{ ~bing ads api }
\keyword{ ~bingAdsR }
\keyword{ ~bing ads report r }
