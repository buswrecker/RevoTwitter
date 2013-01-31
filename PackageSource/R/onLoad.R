.onLoad <- function(libname, pkgname)
{
  cat('Options SQLFile set\n')
  options(revoSQLiteFile = '/mnt/data/revoSQLite.sqlite')
}