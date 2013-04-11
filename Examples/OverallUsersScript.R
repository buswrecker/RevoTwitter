library(RPostgreSQL)
library(RevoTwitter)

createConnection <- function() {
  return(dbConnect(dbDriver("PostgreSQL"), dbname="active_citizen", user="dev", host="localhost"))
}

conn <- createConnection()
res <- ntwitterPerTimeFrame(conn, searchString, startDate, endDate)
