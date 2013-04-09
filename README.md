RevoTwitter
===========

An R package that does Twitter Analysis on a specified database

Usage
=====

	library(RevoTwitter)
	library(RPostgreSQL) # Use Postgres here, MySql should work as well
	library(RJSONIO)

	conn <- dbConnect(dbDriver("PostgreSQL"), dbname="active_citizen", user="dev")
	initializeFeed(conn, 'search_string')
	dbDisconnect(conn)


Dependencies
============

Run this command to install all the dependencies
install.packages(c('twitteR', 'tm', 'XML', 'DBI', 'TTR', 'stringr', 'igragh', 'plyr', 'RPostgreSQL'))

* twitteR requires RCurl which in turn requires libcurl-devel
* XML requires libxml2-devel

