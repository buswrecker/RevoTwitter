\name{initializeFeed}
\alias{initializeFeed}
\title{Initialize Twitter Feed into SQLite}
\usage{
  initializeFeed(conn = NULL, searchString, N = 1500)
}
\arguments{
  \item{conn}{A DBI-compatible database connecttion}
  \item{searchString}{A string that is to be searched on
  Twitter.}

  \item{N}{Number of initialize feeds to populate the
  SQLites}
}
\value{
  Boolean. TRUE if pull from twitter is successful, FALSE
  otherwise
}
\description{
  This function will take in a \code{searchString} and
  return TRUE if successfully obtained twitterFeeds.
  TwitterFeeds are stored into an SQLite file
  \code{options('revoSQLiteFile')} will reveal location of
  SQLitefile. TableName is as the searchString
}
\author{
  Julian Lee \email{julian.lee@revolutionanalytics.com}
}
\seealso{
  \code{\link{updateFeed}}
}

