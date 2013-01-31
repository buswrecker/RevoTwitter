\name{wordCloudAnalysis}
\alias{wordCloudAnalysis}
\title{Draw a WordCloud}
\usage{
  wordCloudAnalysis(searchString, startDate, endDate, ...)
}
\arguments{
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
  This function will take in a searchString, startDate,
  endDate, and pull from sql all the text from those dates
  to draw a wordcloud
}
\author{
  Julian Lee \email{julian.lee@revolutionanalytics.com}
}
\seealso{
  \code{\link{wordCloudMultiple}}
}

