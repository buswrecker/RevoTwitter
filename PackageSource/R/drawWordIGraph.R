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
#' Draws Word Graph
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
#' @seealso \code{\link{drawUsersIGraph}}
#' 
#' @author Julian Lee \email{julian.lee@@revolutionanalytics.com}

drawWordIGraph <- function(searchString,startDate,endDate,sparse=0.95){
  
  require(igraph)
  
  ##0.1.1 - remove white spaces
  tableName <- gsub('\\s|@*#*','',searchString)
  
  ##Grab Data
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname = options('revoSQLiteFile')[[1]])
  
  SQLstatement <- paste('SELECT text from ',tableName,
                        ' WHERE created BETWEEN "',
                        startDate, '" AND "', endDate, '"', sep='')
  
  tweetData <- dbGetQuery(con, SQLstatement)
  
  dbDisconnect(con)
  
  ##Check volume of data
  if(nrow(tweetData) < 10){
    plot(0:1,0:1,xaxt='n',yaxt='n',bty='n',pch='',xlab='',ylab='')
    text(0.5,0.5,'Insufficient Data - Select Larger Date Range',cex=1.2)
    return()
  }
  
  text.corpus <- Corpus(VectorSource(tweetData$text))
  
  ##Remove words/tolower cases/etc
  text.corpus <- tm_map(text.corpus, RemoveAtPeople)
  text.corpus <- tm_map(text.corpus, tolower)	
  text.corpus <- tm_map(text.corpus, removePunctuation)
  text.corpus <- tm_map(text.corpus, RemoveHTTP)
  text.corpus <- tm_map(text.corpus, 
            removeWords, c(tableName,stopwords('en')))  
  
  ##Create a TermDocumentMatrix
  mydata.dtm <- TermDocumentMatrix(text.corpus)
  mydata.dtm <- removeSparseTerms(mydata.dtm, sparse=sparse)
  
  ##Coerce TermDocumentMatrix into a regular Matrix for input into IGraph
  mydata.matrix <- as.matrix(mydata.dtm)
  mydata.matrix[mydata.matrix>=1] <- 1
  termMatrix <- mydata.matrix %*% t(mydata.matrix)
  
  ##Create an Igraph object
  g <- graph.adjacency(termMatrix, weighted=T, mode = 'directed')
  g <- simplify(g)
  
  ##Manipulate the Vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
    
  ##Add some colour and size of vertices to match
  V(g)$label.cex <- 0.8
  #V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
  V(g)$label.color <- rgb(0, 0, .2, .8)
  V(g)$frame.color <- NA
  #egam <- (log(E(g)$weight)) / max(log(E(g)$weight))
  #E(g)$color <- rgb(.5, .5, 0, egam)
  #E(g)$width <- egam
  
  set.seed(1243)
  layout1 <- layout.fruchterman.reingold(g)
  plot(g,layout=layout1)
}