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
#' Draws Retweet Graph
#'
#' This function will take in an string of the XDF file location and return TRUE if
#' successfully updated twitterFeeds
#'
#' @param searchString A character string to be wordclouded
#' @param startDate A character string of the start date, eg "2012-12-30"
#' @param endDate A character string of the end date, eg "2012-12-30"
#'
#' @return NULL
#'
#' @seealso \code{\link{drawWordIGraph}}
#'
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}
#'
drawUsersIGraph <- function(conn=NULL,searchString,startDate,endDate){
  if (is.null(conn)) {
	  stop("Database connection not specified")
  }

  require(igraph)
  require(stringr)
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  
  ##Grab Data
  SQLstatement <- paste('SELECT "text", "screenName" from ',tableName,
                        ' WHERE created BETWEEN \'',
                        startDate, '\' AND \'', endDate, '\'', sep='')
  
  tweetData <- dbGetQuery(conn, SQLstatement)
  
  dbDisconnect(conn)
  
  ##Check volume of data
  if(nrow(tweetData) < 10){
    plot(0:1,0:1,xaxt='n',yaxt='n',bty='n',pch='',xlab='',ylab='')
    text(0.5,0.5,'Insufficient Data - Select Larger Date Range',cex=1.2)
    return()
  }
  
  ##rt_patterns
  rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)",
                     tweetData$text, ignore.case=TRUE)
  
  # create list to store user names
  who_retweet = list(1:length(rt_patterns))
  who_post = list(1:length(rt_patterns))
  
  for (i in 1:length(rt_patterns))
  {
    # get tweet with retweet entity
    twit = tweetData$text[rt_patterns[i]]
    # get retweet source
    poster = str_extract_all(twit,
                             "(RT|via)((?:\\b\\W*@\\w+)+)")
    #remove ':'
    poster = gsub(":", "", unlist(poster))
    # name of retweeted user
    who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE)
    # name of retweeting user
    who_retweet[[i]] = rep(tweetData$screenName[rt_patterns[i]], length(poster))
  }
  
  # unlist
  who_post = unlist(who_post)
  who_retweet = unlist(who_retweet)
  
  # two column matrix of edges
  retweeter_poster = cbind(who_retweet, who_post)
  
  # generate graph
  rt_graph = graph.edgelist(retweeter_poster)
  
  # get vertex names
  ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))
  ver_labs <- tolower(substring(ver_labs,0,11))
  
  # choose some layout
  glay = layout.fruchterman.reingold(rt_graph)
  
  V(rt_graph)$label <- ver_labs
  
  V(rt_graph)$label.cex <- 0.75
  ##V(rt_graph)$label.family <-'sans'
  V(rt_graph)$label.color <- rgb(0, 0, .2, .8)
  V(rt_graph)$frame.color <- 'orange'
  E(rt_graph)$arrow.size=0.5
  E(rt_graph)$arrow.width=0.5
  E(rt_graph)$width <- 3
  
  plot(rt_graph,layout=glay)
  title(paste("\nFollowers on",searchString),
        cex.main=1, col.main="black")
}
