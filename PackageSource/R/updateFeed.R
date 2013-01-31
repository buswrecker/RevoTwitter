#
# Copyright (c) 2011, 2012, Revolution Analytics. All rights reserved.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#' Update Twitter Feed
#' 
#' This function will take in the searchString and update the TwitterFeed
#' on SQLite
#' 
#'
#' @param searchString The \code{searchString} that is to be updated 
#' @param N Number of feeds to populate 
#'
#' @return Boolean. TRUE if update from twitter is successful, FALSE otherwise
#' 
#' @seealso \code{\link{initializeFeed}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}

updateFeed <- function(searchString,N=100){
  
  ##Load Pos/Neg word dictionary
  data(dictionary)
  
  ##Set the SQLiteFile
  tableName <- tolower(gsub('\\s|@*#*','',searchString))
  TwitterSQLITE <- options('revoSQLiteFile')[[1]]
  
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname =TwitterSQLITE)
  
  ##Check if table is in the database
  if(!tableName %in% dbListTables(con)){
    cat('No such table in database\n')
    return(FALSE)
  }
  
  SQLstatement <- paste("SELECT id from",tableName,
                       "ORDER BY id DESC limit 1")
  
  rs <- dbGetQuery(con, SQLstatement)
  ##dbClearResult(rs)
    
  tweetData <- searchTwitter(searchString,n=N,lan='en',
          sinceID=rs$id[1])
  
  if(length(tweetData)==0){
    cat('Feed is Upto Date\n')
    return(TRUE)}
  
  tweetData <- twListToDF(tweetData)
  
  ##Remove non-ASCII characters that would screw up tm
  grepList <-grep('[\x80-\xFF]',tweetData$text,useBytes=T)
  if(length(grepList)!=0){
    tweetData<-tweetData[-grepList,]}
  
  ##If no tweets are returned, then return the original list
  if(nrow(tweetData)==0){
    cat('Feed is Upto Date\n')
    return(TRUE)}
  
  ##Manipulate the sources for Human readable
  sources <- gsub(";&gt;", ">", tweetData$statusSource)
  sources <- gsub("&lt;/a&gt", ">", sources)
  sources <- strsplit(sources, ">")
  sources <- sapply(sources, function(x) ifelse(length(x)>1,x[2],x[1]))
  tweetData$statusSource <- sources
  
  ##Build a text Corpus
  text.corpus <- Corpus(VectorSource(tweetData$text))
  
  text.corpus <- tm_map(text.corpus, removePunctuation)
  text.corpus <- tm_map(text.corpus, RemoveHTTP)
  text.corpus <- tm_map(text.corpus, RemoveAtPeople)
  text.corpus <- tm_map(text.corpus, tolower)
  text.corpus <- tm_map(text.corpus,
                        removeWords, c(tableName,stopwords('en')))
  
  ##Scoring Tweets
  finaloutput <- score.sentiment(unlist(text.corpus),pos.words,neg.words)
  tweetData$score <- finaloutput$score
  
  ##Change Time from GMT to SGT 
  sgttime <- as.POSIXct(tweetData$created,tz='GMT')
  attributes(sgttime)$tzone <- "Singapore" 
  tweetData$created <- as.character(sgttime)
  
  ##Add SearchString Column
  tweetData$searchString <- searchString
  
  ##Order the Tweets from Earliest to Latest
  tweetData <- tweetData[order(tweetData$created),]
  
  SUCCESS <- dbWriteTable(con,tableName,tweetData,append=T,
                          row.names=F)
  dbDisconnect(con)
  
  return(SUCCESS)
}