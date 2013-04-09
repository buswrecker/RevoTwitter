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
#' Draw a Collective WordCloud from all searchStrings
#' 
#' This function will take in a character vector of searchStrings
#' to be compared. The function will retrieve the last 20 tweets
#' of the searchStrings, merge and draw the Comparison wordcloud
#' 
#'
#' @param searchString A character vector to be wordclouded 
#'
#' @return NULL
#' 
#' @seealso \code{\link{wordCloudAnalysis}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}
#' 
wordCloudMultiple <- function(conn=NULL,searchString){
  searchString <- gsub('[^0-9a-z\\s]', '', searchString, ignore.case=TRUE, perl=TRUE)
  if (searchString=='') {
	  stop("Invalid searchString")
  }

  if (is.null(conn)) {
	  stop("Database connection not specified")
  }
  
  require(wordcloud)
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  
  ##Grab the Data
  SQLstatement <- paste('SELECT "created","text","searchString" from ',tableName,
                        "ORDER BY id DESC limit 20")
  
  tweetData <- do.call("rbind",lapply(SQLstatement,function(x) dbGetQuery(conn,x)))
  
  dbDisconnect(conn)
  
  compileTweets <- daply(tweetData,'searchString',function(x) return(paste(x$text,collapse=';'))) 
  
  ##Build Text Corpus
  text.corpus <- Corpus(VectorSource(compileTweets))
  text.corpus <- tm_map(text.corpus, RemoveAtPeople)
  text.corpus <- tm_map(text.corpus, tolower)  
  text.corpus <- tm_map(text.corpus, removePunctuation)
  text.corpus <- tm_map(text.corpus, RemoveHTTP)
  text.corpus <- tm_map(text.corpus, removeWords, c('singapore',tableName,stopwords('en')))
  
  tdm <- TermDocumentMatrix(text.corpus)
  m <- as.matrix(tdm)
  colnames(m) <- names(compileTweets)
  
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  ##comparison.cloud(m,max.words=250,random.order=F)
  
  wordcloud(d$word,d$freq,
            c(5,.2),3,500,random.order=F,random.color=T,
            rot.per=.35,main=paste("WordCloud of",searchString),colors=brewer.pal(8,"Dark2"))

}
