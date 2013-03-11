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
