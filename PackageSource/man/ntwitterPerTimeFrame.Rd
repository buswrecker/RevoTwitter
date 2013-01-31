\name{ntwitterPerTimeFrame}
\alias{ntwitterPerTimeFrame}
\title{Draw Volume of Tweets}
\usage{
  ntwitterPerTimeFrame(searchString, startDate, endDate,
    timeframe = "hour")
}
\arguments{
  \item{searchString}{A character vector of searchStrings}

  \item{startDate}{A character string of the start date, eg
  "2012-12-30"}

  \item{endDate}{A character string of the end date, eg
  "2012-12-30"}

  \item{timeframe}{A character, default is 'hour'. Other
  options 'min','day','week','month','year'}
}
\description{
  This function will take in a character vector of
  searchStrings and draw the Volume of Tweets
}
\author{
  Julian Lee \email{julian.lee@revolutionanalytics.com}
}
\seealso{
  \code{\link{sentimentFeedByTime}}
}

