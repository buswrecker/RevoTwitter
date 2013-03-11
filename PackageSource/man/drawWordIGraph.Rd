\name{drawWordIGraph}
\alias{drawWordIGraph}
\title{Draws Word Graph}
\usage{
  drawWordIGraph(conn = NULL, searchString, startDate, endDate,
    sparse = 0.95)
}
\arguments{
  \item{conn}{A DBI-compatible database connecttion}
  \item{searchString}{A character string to be wordclouded}

  \item{startDate}{A character string of the start date, eg
  "2012-12-30"}

  \item{endDate}{A character string of the end date, eg
  "2012-12-30"}
}
\value{
  NULL
}
\description{
  This function will take in an string of the XDF file
  location and return TRUE if successfully updated
  twitterFeeds
}
\author{
  Julian Lee \email{julian.lee@revolutionanalytics.com}
}
\seealso{
  \code{\link{drawUsersIGraph}}
}

