\name{updateFeed}
\alias{updateFeed}
\title{Update Twitter Feed}
\usage{
  updateFeed(conn = NULL, searchString, N = 100)
}
\arguments{
  \item{conn}{A DBI-compatible database connecttion}
  \item{searchString}{The \code{searchString} that is to be
  updated}
  \item{N}{Number of feeds to populate}
}
\value{
  Boolean. TRUE if update from twitter is successful, FALSE
  otherwise
}
\description{
  This function will take in the searchString and update
  the TwitterFeed on SQLite
}
\author{
  Julian Lee \email{julian.lee@revolutionanalytics.com}
}
\seealso{
  \code{\link{initializeFeed}}
}

