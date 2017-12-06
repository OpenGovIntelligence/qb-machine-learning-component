
library(ghql) # for querying 
library(jsonlite) # for parsing the json response
library(httr) # for working with URLs
library(tidyverse) # for tidying data

runQuery <- function (query) {
  client <- GraphqlClient$new(url ="http://195.251.218.39:8085/graphql") #define the GraphqlClient
  qry <- Query$new() #define a new query
  qry$query('query', query)
  responses <- client$exec(qry$queries$query) #gets the responses of the query
  df <- as.data.frame(responses) #repsonses to data frame
  return(df)
}