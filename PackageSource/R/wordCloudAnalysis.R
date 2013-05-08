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
#' Draw a WordCloud
#' 
#' This function will take in a searchString, startDate, endDate,
#' and pull from sql all the text from those dates to draw a wordcloud
#' 
#'
#' @param searchString A character string to be wordclouded 
#' @param focusString A character for PAP,WP,RP or SDA. 
#' @param startDate A character string of the start date, eg "2012-12-30"
#' @param endDate A character string of the end date, eg "2012-12-30" 
#'
#' @return NULL
#' 
#' @seealso \code{\link{wordCloudMultiple}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}

wordCloudAnalysis <- function(conn=NULL,searchString,focusString=NULL,startDate,endDate,type='ALL',...){
  if (is.null(conn)) {
	  stop("Database connection not specified")
  }
  
  require(wordcloud)
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  print("Getting data from db")
  print(proc.time())
  ##Grab the Data
  SQLstatement <- paste('SELECT text,score FROM ',tableName,
                        ' WHERE created BETWEEN \'',
                        startDate, '\' AND date \'', endDate, '\' + 1', sep='')
  
  tweetData <- dbGetQuery(conn,SQLstatement)
  ##tweetData <- do.call("rbind",lapply(SQLstatement,function(x) dbGetQuery(conn,x)))
  
  print("Data retrieved")
  print(proc.time())
  print(nrow(tweetData))
  dbDisconnect(conn)
  
  ##If you want wordcloud of positive,negative or neutral tweets
  if(type=='POS'){
    tweetData <- tweetData[tweetData$score > 0,]
  }else if(type=='NEG'){
    tweetData <- tweetData[tweetData$score < 0,]
  }else if(type=='NEU'){
    tweetData <- tweetData[tweetData$score == 0,]
  }
  
  ##Check volume of data
  if(nrow(tweetData) < 3){
    stop('Insufficient Data')
  }
  
  ##compileTweets <- daply(tweetData,'searchString',function(x) return(paste(x$text,collapse=';'))) 
  
  print("Filter out text")
  print(proc.time())
  tweetData$text <- lapply(tweetData$text, RemoveAtPeople)
  tweetData$text <- lapply(tweetData$text, tolower)
  tweetData$text <- lapply(tweetData$text, removePunctuation)
  tweetData$text <- lapply(tweetData$text, RemoveHTTP)

  print("Filter out stop words")
  print(proc.time())
  stopWords <- c('amp', 'yahoo', 'singapore', stopwords('en'))
  '%nin%' <- Negate('%in%')
  tweetData$text <- lapply(tweetData$text, function(x) {
      t <- unlist(strsplit(x, "\\s"))
      paste(t[t %nin% stopWords], collapse=" ")
  })
  ##Build Text Corpus
  print("Building text corpus")
  print(proc.time())
  text.corpus <- Corpus(VectorSource(tweetData$text))
  #text.corpus <- tm_map(text.corpus, RemoveAtPeople)
  #text.corpus <- tm_map(text.corpus, tolower)
  #text.corpus <- tm_map(text.corpus, removePunctuation)
  #text.corpus <- tm_map(text.corpus, RemoveHTTP)
  
  print("Creating term matrix")
  print(proc.time())
  tdm <- TermDocumentMatrix(text.corpus)
  m <- as.matrix(tdm)
  #colnames(m) <- names(compileTweets)
  
  print("Counting and sorting words")
  print(proc.time())
  v <- sort(rowSums(m),decreasing=TRUE)
  if (!is.null(v)) {
    print("Preparing data to output")
    print(proc.time())
    d <- data.frame(word = names(v),freq=v)
    d <- d[1:300,]
  } else {
    stop("No data available")
    return()
  }
  
  print("Output data")
  print(proc.time())
  return(d)
}
