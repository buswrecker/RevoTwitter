\name{sentimentFeedByTime}
\alias{sentimentFeedByTime}
\title{Draw Sentiment Analysis}
\usage{
  sentimentFeedByTime(conn = NULL, searchString, startDate, endDate,
    window = 2, timeframe = "hour")
}
\arguments{
  \item{conn}{A DBI-compatible database connecttion}
  \item{searchString}{A character vector of searchStrings}

  \item{startDate}{A character string of the start date, eg
  "2012-12-30"}

  \item{endDate}{A character string of the end date, eg
  "2012-12-30"}

  \item{window}{The moving average window size,
  default=100}

  \item{timeframe}{A character, default is 'hour'. Other
  options 'min','day','week','month','year'}
}
\value{
  Boolean. TRUE if update from twitter is successful, FALSE
  otherwise
}
\description{
  This function will take in a character vector of
  searchStrings and draw the Sentiment
}
\author{
  Julian Lee \email{julian.lee@revolutionanalytics.com}
}
\seealso{
  \code{\link{ntwitterPerTimeFrame}}
}

