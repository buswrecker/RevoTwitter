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
#' This function will take in the searchString, startDate and endDate and draw the Sentiment of the 4 parties
#'
#' @param searchString A character string twitterSearch
#' @param startDate A character string of the start date, eg "2012-12-30"
#' @param endDate A character string of the end date, eg "2012-12-30"
#' @param window The moving average window size, default=100
#'
#' @return Boolean. TRUE if update from twitter is successful, FALSE otherwise
#' 
#' @seealso \code{\link{sentimentFeedByTime}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}



sentimentFeedPunggol <- function(searchString, startDate, endDate, window=2){
  
  require(ggplot2)
  require(TTR)
  require(scales)
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  
  ##Grab the Data
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname = options('revoSQLiteFile')[[1]])
  
  SQLstatement <- paste('SELECT text,created,score,searchString from ',tableName,
                        ' WHERE created BETWEEN "',
                        startDate, '" AND "', endDate, '"', sep='')
  
  tweetData <- do.call("rbind",lapply(SQLstatement,function(x) dbGetQuery(con,x)))
  
  dbDisconnect(con)
  
  ##PAP
  pap <- grep('[pP][aA][pP] | [kK][oO][hH]',tweetData$text)
  
  ##wpsg
  wp <- grep('[wW][pP][sS]*[gG]* | [lL][eE]{2}',tweetData$text)
  
  ##reformparty
  rp <- grep('[rR][pP] | Kenneth',tweetData$text)
  
  ##SDA
  sda <- grep('[sS][dD][aA] | [Ll][iI][mM]',tweetData$text)
  
  ##Prep the tweets
  PAPtweets <- tweetData[pap,]
  PAPtweets$PARTY <- 'PAP'
  WPtweets <- tweetData[wp,]
  WPtweets$PARTY <- 'WP'
  RPtweets <- tweetData[rp,]
  RPtweets$PARTY <- 'RP'
  SDAtweets <- tweetData[sda,]
  SDAtweets$PARTY <- 'SDA'
  
  ##Combine to a new dataframe
  tweetDataFil <- rbind(PAPtweets,WPtweets,RPtweets,SDAtweets)
  
  ##DataCleaning
  rm(tweetData,PAPtweets,WPtweets,RPtweets,SDAtweets)
  
  ##
  tweetDataFil$created <- as.POSIXct(tweetDataFil$created,tz='Singapore')
  tweetDataFil$PARTY <- factor(tweetDataFil$PARTY)
  
  tmp <- split(tweetDataFil, as.factor(tweetDataFil$PARTY))
  tmp2 <- do.call('c',lapply(tmp,function(x) SMA(x$score,n=window)))
  tweetData2 <- cbind(tweetDataFil,smoothed=tmp2)
  
  pp <- ggplot(data=tweetData2, aes(x=created,y=smoothed,group=PARTY)) + geom_hline(aes(yintercept=0),linetype='dotted')
  pp <- pp + geom_line(aes(colour=PARTY),linetype=1,size=1.3) + xlab('Time of Analysis') + ylab('Overall Sentiment')
  pp <- pp + scale_x_datetime(labels=date_format('%m/%d %H')) +  opts(title=paste('Moving Average,n=',window,sep=''))
  pp
}
              
  