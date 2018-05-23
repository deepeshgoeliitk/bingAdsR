rm(list=ls())
# library(devtools)
# install_github("deepeshgoeliitk/bingAdsR")
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
