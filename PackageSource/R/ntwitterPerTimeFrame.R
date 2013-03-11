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
#' Draw Volume of Tweets
#' 
#' This function will take in a character vector of searchStrings and draw the Volume of Tweets
#'
#' @param searchString A character vector of searchStrings 
#' @param startDate A character string of the start date, eg "2012-12-30"
#' @param endDate A character string of the end date, eg "2012-12-30"
#' @param timeframe A character, default is 'hour'. Other options 'min','day','week','month','year'
#' 
#' @seealso \code{\link{sentimentFeedByTime}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}

ntwitterPerTimeFrame <- function(conn=NULL,searchString,startDate,endDate,timeframe='hour'){
  if (is.null(conn)) {
	  stop("Database connection not specified")
  }
  
  require(ggplot2)
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  
  SQLstatement <- paste('SELECT created,searchString from ',tableName,
                        ' WHERE created BETWEEN "',
                        startDate, '" AND "', endDate, '"', sep='')
  
  tweetData <- do.call("rbind",lapply(SQLstatement,function(x) dbGetQuery(conn,x)))
  
  dbDisconnect(conn)
  
  ##Check volume of data
  if(nrow(tweetData) < 5){
    plot(0:1,0:1,xaxt='n',yaxt='n',bty='n',pch='',xlab='',ylab='')
    text(0.5,0.5,'Insufficient Data - Select Larger Date Range',cex=1.2)
    return()
  }
  tweetData$created <- trunc(as.POSIXct(tweetData$created,tz='Singapore'),timeframe)
  
  myTweetDF <- ddply(tweetData,c('created','searchString'),function(x) nrow(x))
  
  mtitle <- paste("Number of Tweets per",timeframe)
  
  pp <- ggplot(data=myTweetDF, aes(x=created,y=V1,fill=searchString)) + geom_bar(aes(col=searchString),stat='identity')
  pp <- pp + xlab('Time') + ylab('Number of Tweets') + opts(title=mtitle)
  
  pp
  
}
