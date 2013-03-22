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
#' Draw Sentiment Analysis
#' 
#' This function will take in a character vector of searchStrings and draw the Sentiment
#'
#' @param searchString A character vector of searchStrings 
#' @param startDate A character string of the start date, eg "2012-12-30"
#' @param endDate A character string of the end date, eg "2012-12-30"
#' @param window The moving average window size, default=100
#' @param timeframe A character, default is 'hour'. Other options 'min','day','week','month','year'
#'
#' @return Boolean. TRUE if update from twitter is successful, FALSE otherwise
#' 
#' @seealso \code{\link{nTwitterPerTimeFrame}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}

sentimentFeedByTime <- function(conn=NULL,searchString,startDate, endDate, window=2, timeframe='hour'){
  if (is.null(conn)) {
	  stop("Database connection not specified")
  }
  
  require(ggplot2)
  require(TTR)
  require(scales)
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  
  ##Grab the Data
  SQLstatement <- paste('SELECT "created","score","searchString" from ',tableName,
                        ' WHERE created BETWEEN \'',
                        startDate, '\' AND \'', endDate, '\'', sep='')
  
  tweetData <- do.call("rbind",lapply(SQLstatement,function(x) dbGetQuery(conn,x)))
  
  dbDisconnect(conn)
  
  ##Check volume of data
  if(nrow(tweetData) < 5){
    plot(0:1,0:1,xaxt='n',yaxt='n',bty='n',pch='',xlab='',ylab='')
    text(0.5,0.5,'Insufficient Data - Select Larger Date Range',cex=1.2)
    return()
  }
  
  tweetData$created <- as.POSIXct(tweetData$created,tz='Singapore')
  tweetData$searchString <- factor(tweetData$searchString)
  
  ##0.2.1 - Using Moving Averages
  if(0){
  computeSentiment <- function(tweetData,...){
    myhour <- unique(tweetData$created)
    tmp <- tweetData
    score <- numeric(0)
    for(i in 1:length(myhour)){
      if(i==1){
        score <- mean(head(tmp$score,window))
      }else if (i==length(myhour)){
        score <- c(score,0)
      }else{
        score <- c(score,mean(head(tmp$score[!tmp$created %in% myhour[1:i]],window)))
      }
    }
  
    mySUM <- data.frame(created=myhour,V1=score)
    return(mySUM)
  }
  }
  
  ##0.2.1 - Just compute the mean
  #tweetData2 <- ddply(tweetData,'searchString',computeSentiment)

  tmp <- split(tweetData, as.factor(tweetData$searchString))
  tmp2 <- do.call('c',lapply(tmp,function(x) SMA(x$score,n=window)))
  tweetData2 <- cbind(tweetData,smoothed=tmp2)
  

  pp <- ggplot(data=tweetData2, aes(x=created,y=smoothed,group=searchString)) + geom_hline(aes(yintercept=0),linetype='dotted')
  pp <- pp + geom_line(aes(colour=searchString),linetype=1,size=1.3) + xlab('Time of Analysis') + ylab('Overall Sentiment')
  pp <- pp + scale_x_datetime(labels=date_format('%m/%d %H')) +  opts(title=paste('Moving Average,n=',window,sep=''))
  pp
  #return(mySUM)
}
