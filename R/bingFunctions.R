rm(list=ls())

###Dummy Variables###
client_id <- ""
client_secret <- ""
###To get data###
username = ""
password = ""
developer_token = ""
customer_id = ""
account_id = ""
reportId = ""


####AUTHENTICATION#####
getAccessToken <- function(client_id, client_secret, grant_type, redirect_uri, code, refresh_token){
  baseURL <- 'https://login.live.com/oauth20_token.srf'
  redirect_uri <- 'http://localhost'
  #Based on grant_type, choose the authorization_code
  if(grant_type == "authorization_code"){
    requestBody <- list(client_id=client_id, client_secret=client_secret, grant_type=grant_type, redirect_uri=redirect_uri, code=code)
  }else{
    #grant_type = refresh_token, code is not required
    requestBody <- list(client_id=client_id, client_secret=client_secret, grant_type=grant_type, redirect_uri=redirect_uri, refresh_token=refresh_token)
  }
  requestHeader <- c("Content-Type" =  "application/x-www-form-urlencoded",  "Accept" = "application/json", "Host" = "login.live.com")
  response <- POST(baseURL, body = requestBody, encode = "form", add_headers(.headers = requestHeader))
  contents = fromJSON(txt = as.character(rawToChar(response$content)))
  #Get the tokens and save refresh_token
  access_token <<- contents$access_token
  refresh_token <- contents$refresh_token
  write(refresh_token, "refresh_token")
  # return(access_token)
}


authenticateFirstTimeUser <- function(client_id, client_secret){
  # Authenticate user to get code
  redirect_uri <- 'http://localhost'
  url <- sprintf('https://login.live.com/oauth20_authorize.srf?client_id=%s&scope=bingads.manage&response_type=code&redirect_uri=%s&state=ClientStateGoesHere', client_id,redirect_uri)
  browseURL(url)
  code <- readline("Please enter the code from the URL: ")
  # Use the code to get accessToken and refreshToken
  getAccessToken(client_id, client_secret, grant_type="authorization_code", redirect_uri, code, refresh_token=NULL)
}


baAuthentication <- function(client_id, client_secret){
  if(!file.exists("refresh_token")){
    authenticateFirstTimeUser(client_id, client_secret)
  }else{
    refresh_token <- readLines("refresh_token")
    getAccessToken(client_id, client_secret, grant_type="refresh_token", redirect_uri, code=NULL, refresh_token=refresh_token)
  }
}

baAuthentication(client_id, client_secret)


##Report Type 2: CampaignPerformanceReportRequest
getReportId <- function(client_id, access_token, customer_id, account_id, developer_token, password, username){
  url <- "https://reporting.api.bingads.microsoft.com/Api/Advertiser/Reporting/v12/ReportingService.svc"
  SOAPAction <- "SubmitGenerateReport"
  report <- "CampaignPerformanceReportRequest"
  header <- paste(readLines("R/reporting.header.xml"), collapse = "")
  bodyXML <- paste(readLines("R/reporting.campaignPerformance.xml"), collapse = "")
  bodyXML <- sprintf(bodyXML, report, account_id)
  # bodyXML <- sprintf(bodyXML, report, account_id, endDate$day, endDate$month, endDate$year, startDate$day, startDate$month, startDate$year)
  body <- sprintf(header, SOAPAction, client_id, access_token, customer_id, account_id, developer_token, password, username, bodyXML)
  headerFields =  c(Accept = "text/xml", Accept = "multipart/*", 'Content-Type' = "text/xml;charset=utf-8", SOAPAction = SOAPAction)
  h = basicTextGatherer()
  curlPerform(url = url, httpheader = headerFields, postfields = body, writefunction = h$update)
  reportId <- xmlToList(h$value())$Body$SubmitGenerateReportResponse$ReportRequestId
  return(reportId)
}

#Download the file and read it
getDownloadUrl <- function(client_id, access_token, customer_id, account_id, developer_token, password, username, reportId){
  url <- "https://reporting.api.bingads.microsoft.com/Api/Advertiser/Reporting/v12/ReportingService.svc"
  SOAPAction <- "PollGenerateReport"
  report <- "PollGenerateReportRequest"
  header <- paste(readLines("R/reporting.header.xml"), collapse = "")
  bodyXML <- '<PollGenerateReportRequest xmlns="https://bingads.microsoft.com/Reporting/v12"><ReportRequestId i:nil="false">%s</ReportRequestId></PollGenerateReportRequest>'
  bodyXML <- sprintf(bodyXML, reportId)
  body <- sprintf(header, SOAPAction, client_id, access_token, customer_id, account_id, developer_token, password, username, bodyXML)
  headerFields =  c(Accept = "text/xml", Accept = "multipart/*", 'Content-Type' = "text/xml;charset=utf-8", SOAPAction = SOAPAction)
  h = basicTextGatherer()
  curlPerform(url = url, httpheader = headerFields, postfields = body, writefunction = h$update)
  downloadUrl <- xmlToList(h$value())$Body$PollGenerateReportResponse$ReportRequestStatus$ReportDownloadUrl
  return(downloadUrl)
}

getCampaignPerformance <- function(client_id, access_token, customer_id, account_id, developer_token, password, username){
  reportId <- getReportId(client_id, access_token, customer_id, account_id, developer_token, password, username)
  downloadUrl <- getDownloadUrl(client_id, access_token, customer_id, account_id, developer_token, password, username, reportId)
  download.file(url = downloadUrl, destfile = "tmp.zip", method ='auto', mode = 'wb')
  unzip("tmp.zip")
  files <- unzip("tmp.zip", list = TRUE)
  df <- read.csv(files$Name[1])
  file.remove("tmp.zip")
  file.remove(files$Name[1])
  df
}

