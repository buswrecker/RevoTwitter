library(RPostgreSQL)
library(RevoTwitter)

createConnection <- function() {
  return(dbConnect(dbDriver("PostgreSQL"), dbname="active_citizen", user="dev", host="localhost"))
}

tryCatch({
  conn <- createConnection()
  res <- wordCloudAnalysis(conn, searchString, NULL, startDate, endDate)
}, error=function(err) {
  paste("Error: ", err)
})
