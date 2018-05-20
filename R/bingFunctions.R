rm(list=ls())
path <- paste0(.libPaths()[1],"/bingAdsR/data/")


###Dummy Variables###
reportId = ""
credentials <- list(
  ###To get accessToken###
  client_id <- "",
  client_secret <- "",
  ###To get data###
  username = "",
  password = "",
  developer_token = "",
  customer_id = "",
  account_id = ""
)

setAccessToken <- function(credentials, access_token){
  credentials <- list(
    client_id = credentials$client_id,
    client_secret = credentials$client_secret,
    username = credentials$username,
    password = credentials$password,
    developer_token = credentials$developer_token,
    customer_id = credentials$customer_id,
    access_token = access_token,
    account_id = credentials$account_id
  )
  print("Access Token Generated.")
  return(credentials)
}

####AUTHENTICATION#####
getAccessToken <- function(credentials, grant_type, redirect_uri, code, refresh_token){
  baseURL <- 'https://login.live.com/oauth20_token.srf'
  redirect_uri <- 'http://localhost'
  #Based on grant_type, choose the authorization_code
  if(grant_type == "authorization_code"){
    requestBody <- list(client_id=credentials$client_id, client_secret=credentials$client_secret,
                        grant_type=grant_type, redirect_uri=redirect_uri, code=code)
  }else{
    #grant_type = refresh_token, code is not required
    requestBody <- list(client_id=credentials$client_id, client_secret=credentials$client_secret,
                        grant_type=grant_type, redirect_uri=redirect_uri, refresh_token=refresh_token)
  }
  requestHeader <- c("Content-Type" =  "application/x-www-form-urlencoded",  "Accept" = "application/json", "Host" = "login.live.com")
  response <- POST(baseURL, body = requestBody, encode = "form", add_headers(.headers = requestHeader))
  contents = fromJSON(txt = as.character(rawToChar(response$content)))
  #Get the tokens and save refresh_token
  access_token <- contents$access_token
  refresh_token <- contents$refresh_token
  write(refresh_token, "refresh_token")
  credentials <- setAccessToken(credentials, access_token)
  return(credentials)
}

authenticateFirstTimeUser <- function(credentials){
  # Authenticate user to get code
  redirect_uri <- 'http://localhost'
  url <- sprintf('https://login.live.com/oauth20_authorize.srf?client_id=%s&scope=bingads.manage&response_type=code&redirect_uri=%s&state=ClientStateGoesHere', credentials$client_id,redirect_uri)
  browseURL(url)
  code <- readline("Please enter the code from the URL: ")
  # Use the code to get accessToken and refreshToken
  credentials <- getAccessToken(credentials, grant_type="authorization_code", redirect_uri, code)
  return(credentials)
}


baAuthentication <- function(credentials){
  if(!file.exists("refresh_token")){
    print("Refresh Token not present. Will Use code.")
    credentials <- authenticateFirstTimeUser(credentials)
  }else{
    refresh_token <- readLines("refresh_token")
    if(refresh_token==""){
      print("Refresh Token is empty. Will Use code.")
      credentials <- authenticateFirstTimeUser(credentials)
    }else{
      print("Getting Access Token using Refresh Token.")
      credentials <- getAccessToken(credentials, grant_type="refresh_token", redirect_uri, code=NULL, refresh_token = refresh_token)
    }
  }
  return(credentials)
}


##Report Any Type
getReportId <- function(credentials, report, columns, startDate, endDate){
  dateSplitter <- function(x){
    x <- as.Date(x, origin = "1970-01-01")
    tmp <- list()
    tmp$year <- as.integer(format(x, "%Y"))
    tmp$month <- as.integer(format(x, "%m"))
    tmp$day <- as.integer(format(x, "%d"))
    return(tmp)
  }
  
  getColumnsXML <- function(report, columms){
    columnsXML <- ""
    for(column in columns){
      columnsXML <- paste0(columnsXML, "<", report, "Column>", column, "</", report, "Column>")
    }
    return(columnsXML)
  }
  
  startDate <- dateSplitter(startDate)
  endDate <- dateSplitter(endDate)
  url <- "https://reporting.api.bingads.microsoft.com/Api/Advertiser/Reporting/v12/ReportingService.svc"
  SOAPAction <- "SubmitGenerateReport"
  header <- paste(readLines(paste0(path,"reporting.header.xml")), collapse = "")
  bodyXML <- paste(readLines(paste0(path,"reporting.SubmitGenerateReportRequest.xml")), collapse = "")
  columnsXML <- getColumnsXML(report, columms)
  bodyXML <- sprintf(bodyXML, report, columnsXML, credentials$account_id, endDate$day, endDate$month, endDate$year, startDate$day, startDate$month, startDate$year)
  body <- sprintf(header, SOAPAction,
                  credentials$client_id, credentials$access_token, credentials$customer_id, credentials$account_id, credentials$developer_token, credentials$password, credentials$username,
                  bodyXML)
  # write(body, "body.XML")
  headerFields =  c(Accept = "text/xml", Accept = "multipart/*", 'Content-Type' = "text/xml;charset=utf-8", SOAPAction = SOAPAction)
  h = basicTextGatherer()
  curlPerform(url = url, httpheader = headerFields, postfields = body, writefunction = h$update)
  reportId <- xmlToList(h$value())$Body$SubmitGenerateReportResponse$ReportRequestId
  return(reportId)
}

#Download the file and read it
getDownloadUrl <- function(credentials, reportId){
  url <- "https://reporting.api.bingads.microsoft.com/Api/Advertiser/Reporting/v12/ReportingService.svc"
  SOAPAction <- "PollGenerateReport"
  report <- "PollGenerateReportRequest"
  header <- paste(readLines(paste0(path,"reporting.header.xml")), collapse = "")
  bodyXML <- '<PollGenerateReportRequest xmlns="https://bingads.microsoft.com/Reporting/v12"><ReportRequestId i:nil="false">%s</ReportRequestId></PollGenerateReportRequest>'
  bodyXML <- sprintf(bodyXML, reportId)
  body <- sprintf(header, SOAPAction,
                  credentials$client_id, credentials$access_token, credentials$customer_id, credentials$account_id, credentials$developer_token, credentials$password, credentials$username,
                  bodyXML)
  headerFields =  c(Accept = "text/xml", Accept = "multipart/*", 'Content-Type' = "text/xml;charset=utf-8", SOAPAction = SOAPAction)
  h = basicTextGatherer()
  curlPerform(url = url, httpheader = headerFields, postfields = body, writefunction = h$update)
  downloadUrl <- xmlToList(h$value())$Body$PollGenerateReportResponse$ReportRequestStatus$ReportDownloadUrl
  return(downloadUrl)
}

#Download the data
getDataFromURL <- function(downloadUrl){
  zip(zipfile = 'tmp.zip', files = 'refresh_token')
  download.file(url = downloadUrl, destfile = "tmp.zip", mode = 'wb', method ='auto')
  unzip("tmp.zip")
  files <- unzip("tmp.zip", list = TRUE)
  df <- read.csv(files$Name[1])
  file.remove("tmp.zip")
  file.remove(files$Name[1])
  return(df)
}





