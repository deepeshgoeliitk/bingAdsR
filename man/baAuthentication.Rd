\name{baAuthentication}
\alias{baAuthentication}
\title{
This generates the accessToken using a refresh token or authorization code.
}
\description{
This function starts the authentication process with Microsoft Bing. 
If the user is initiating it for the first time, this function needs user interaction.

Assign the credentials <- baAuthentication(credentials). This updates the access token in the credentials.
}
\usage{
baAuthentication(credentials = credentials)
}
\arguments{
  \item{credentials}{
	All the credentials related paramters are put in a list named - 'credentials' as shown below.
	##Not run
	credentials <- list(
	  client_id = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
	  client_secret = "XXXXXXXXXXXXXXX",
	  username = "abc@abc.com",
	  password = "***********",
	  developer_token = "XXXXXXXXXXXXXXXXX",
	  customer_id = "XXXXXXXXXX",
	  account_id = "XXXXXXX"
	)
	##End(Not run)
}
}
\details{
}
\value{
\item{credentials}{
				The credentials is the list of parameters like client_id, client_secret, username, password, developer_token, customer_id and account_id. 
				The refresh_token generated is saved in the working directory. The access_token is passed as a value in the credentials list itself. Hence it is necessary to assign the output to credentials.
				}
}
\references{
Getting Started with Bing API: https://docs.microsoft.com/en-us/bingads/guides/get-started?view=bingads-12
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
	credentials <- baAuthentication(credentials)
	##End(Not run)
}
\keyword{ ~bing ads reporting api in R }
\keyword{ ~bing ads api authentication }
\keyword{ ~baAuthentication }
