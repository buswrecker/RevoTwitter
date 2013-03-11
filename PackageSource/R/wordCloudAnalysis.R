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
  
  ##0.2.1 - politicians
  politicians <- c('Koh Poh Koon','Lee Li Lian','Kenneth Jeyaretnam','Desmond Lim')
  politicians <- tolower(do.call('c',strsplit(politicians,' ')))
  politicians <- c(politicians,'punggoleast','byelection')
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  
  ##Grab the Data
  SQLstatement <- paste('SELECT text,score FROM ',tableName,
                        ' WHERE created BETWEEN "',
                        startDate, '" AND "', endDate, '"', sep='')
  
  tweetData <- dbGetQuery(conn,SQLstatement)
  ##tweetData <- do.call("rbind",lapply(SQLstatement,function(x) dbGetQuery(conn,x)))
  
  dbDisconnect(conn)
  
  ##If focusstring is not null
  if(!is.null(focusString)){
    if(focusString=='PAP'){
      ssIndex <-  grep('[pP][aA][pP] | [kK][oO][hH]',tweetData$text)
    }else if(focusString=='WP'){
      ssIndex <- grep('[wW][pP][sS]*[gG]* | [lL][eE]{2}',tweetData$text)
    }else if(focusString=='RP'){
      ssIndex <- grep('[rR][pP] | Kenneth',tweetData$text)
    }else if(focusString=='SDA'){
      ssIndex <- grep('[sS][dD][aA] | [Ll][iI][mM]',tweetData$text)
    }else{
      return("No Known focusString")
    }
    
    tweetData <- tweetData[ssIndex,]
  }
  
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
    plot(0:1,0:1,xaxt='n',yaxt='n',bty='n',pch='',xlab='',ylab='')
    text(0.5,0.5,'Insufficient Data - Select Larger Date Range',cex=1.2)
    return()
  }
  
  ##compileTweets <- daply(tweetData,'searchString',function(x) return(paste(x$text,collapse=';'))) 
  
  ##Build Text Corpus
  text.corpus <- Corpus(VectorSource(tweetData$text))
  text.corpus <- tm_map(text.corpus, RemoveAtPeople)
  text.corpus <- tm_map(text.corpus, tolower)  
  text.corpus <- tm_map(text.corpus, removePunctuation)
  text.corpus <- tm_map(text.corpus, RemoveHTTP)
  text.corpus <- tm_map(text.corpus, removeWords, c(searchString, politicians,'amp','yahoo','singapore',stopwords('en')))
  
  tdm <- TermDocumentMatrix(text.corpus)
  m <- as.matrix(tdm)
  #colnames(m) <- names(compileTweets)
  
  
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  ##comparison.cloud(m,max.words=250,random.order=F)
  
  wordcloud(d$word,d$freq,
              c(5,.2),2,500,random.order=F,random.color=T,
              rot.per=.35,main=paste("WordCloud of",searchString),colors=brewer.pal(8,"Dark2"))
    
}
