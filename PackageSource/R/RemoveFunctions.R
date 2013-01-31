RemoveAtPeople <- function(tweet) {
  gsub("@\\w+", "", tweet)
}

RemoveHTTP <- function(tweet) {
  gsub("http[[:alnum:][:punct:]]+", "", tweet)
}