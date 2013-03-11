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
#' Initialize Twitter Feed in database
#' 
#' This function will take in a \code{searchString} and return TRUE if 
#' successfully obtained twitterFeeds. TwitterFeeds are stored into database
#' TableName is as the searchString
#'
#' @param searchString A string that is to be searched on Twitter. 
#' @param N Number of initialize feeds to populate the database
#'
#' @return Boolean. TRUE if pull from twitter is successful, FALSE otherwise
#' 
#' @seealso \code{\link{updateFeed}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}

initializeFeed <- function(conn=NULL, searchString, N=1500){
  if (is.null(conn)) {
	  stop("Database connection not specified")
  }
  
  data(dictionary)
  
  ##Curl web to get only ENGLISH tweets
  tweetData <- searchTwitter(searchString,n=N,lan='en')
  
  if(length(tweetData)==0){
    cat('No Such Entry\n')
    return(TRUE)}
  
  tweetData <- twListToDF(tweetData)
  
  ##Prepare to Export Out to database
  tableName <- tolower(gsub('\\s|@*#*','',searchString))
  
  ##Remove non-ASCII characters that would screw up tm
  grepList <-grep('[\x80-\xFF]',tweetData$text,useBytes=T)
  if(length(grepList)!=0){
    tweetData<-tweetData[-grepList,]}
  
  ##Manipulate Sources for Human Readable
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
  
  ##Check if write was successful
  ##Ensure that field types are correct
  SUCCESS <- dbWriteTable(conn, tableName, tweetData,
                row.names=F,
                field.types=list(text='TEXT',favourited='BOOLEAN',
                  replyToSN='INTEGER',created='TIMESTAMP',truncated='BOOLEAN',
                  replytoSID='TEXT',id='TEXT',replyToUID='INTEGER',
                  statusSource='TEXT',screenName='TEXT',
                  score='INTEGER',searchString='TEXT'))
  
  dbDisconnect(conn)

  return(SUCCESS)
}
