library(RPostgreSQL)
library(RevoTwitter)

createConnection <- function() {
  return(dbConnect(dbDriver("PostgreSQL"), dbname="active_citizen", user="dev", host="localhost"))
}

conn <- createConnection()
res <- wordCloudAnalysis(conn, searchString, NULL, startDate, endDate)
