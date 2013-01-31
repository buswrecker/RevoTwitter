\name{wordCloudMultipleComparison}
\alias{wordCloudMultipleComparison}
\title{Draw a Comparison WordCloud}
\usage{
  wordCloudMultipleComparison(searchString)
}
\arguments{
  \item{searchString}{A character vector to be wordclouded}
}
\value{
  NULL
}
\description{
  This function will take in a character vector of
  searchStrings to be compared. The function will retrieve
  the last 20 tweets of the searchStrings, merge and draw
  the Comparison wordcloud
}
\author{
  Julian Lee \email{julian.lee@revolutionanalytics.com}
}
\seealso{
  \code{\link{wordCloudAnalysis}}
}

