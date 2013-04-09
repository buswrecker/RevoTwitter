library(RPostgreSQL)
library(RevoTwitter)

createConnection <- function() {
  return(dbConnect(dbDriver("PostgreSQL"), dbname="active_citizen", user="dev", host="localhost"))
}

checkTableExist <- function(conn, str) {
  tableName <- tolower(gsub('\\s|@*#*','',str))
  if (!(tableName %in% dbListTables(conn))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

tryCatch({
  conn <- createConnection()
  if (!checkTableExist(conn, searchString)) {
    initializeFeed(conn, searchString)
  } else {
    updateFeed(conn, searchString)
  }
}, error=function(err) {
  paste("Error: ", err)
})
