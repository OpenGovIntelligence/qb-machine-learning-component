
getCubeObservations<- function(schema, fixedvalues, freedimensions){
  client6 <- GraphqlClient$new(url = "http://195.251.218.39:8085/graphql")
  #hold the total number of matches
  qry_totalmatches <- Query$new() 
  
  #create a string with all the fixed values concatenated
  fixed_str<-paste(fixedvalues, collapse=' ')
  #create a string with all the free values concatenated
  free_str<-paste(freedimensions, collapse=' ')
  
  #GraphQL query to get the total matches
  x<-paste('{',schema,'{observations(dimensions:{',fixed_str,'} ) {total_matches}}}')
  qry_totalmatches$query('query', paste('{',schema,'{observations(dimensions:{',
                                        fixed_str,'} ) {total_matches}}}'))
  
  #Execute the query 
  total_matches <- client6$exec(qry_totalmatches$queries$query)
  #Get the total natches
  total<- total_matches[["data"]][[schema]][["observations"]][["total_matches"]]
  
  #print(total)
  #initialize the total data frame
  df_total<- NULL
  
  #If there are more that 1000 matches (max_mathces returned =1000)
  if(total > 100000){
    nextpage<- NULL
    
    #Run the query to collect all matches
    while(total>0){
      first<- NULL
      
      #If there are more than 100 matches left
      if(total>100000){
        first= 100000
      }else{ # if there are less than 1000 matches left
        first= total
      }
      
      #The first query does not require an "after" parameter
      if(is.null(nextpage)){
        str2Append<-first
      }else{ #the rest queries (after the first) require an "after"
        str2Append<-paste(first," after:\"",nextpage,"\"",sep="")
      }
      
      #the query to get the matches
      qry_getObservations <- Query$new()
      qry_getObservations$query('query',paste('{',schema,'{
                                              observations(dimensions:{',fixed_str,'} ) {',
                                              'page(first:',str2Append,'){',
                                              'next_page result {',
                                              free_str,'}}}}}') )
      #Get the result
      responses <- client6$exec(qry_getObservations$queries$query)
      #store to a data frame the results
      #df <- as.data.frame(responses$data$dataset_house_prices$observations$page$result)
      df <- as.data.frame(responses[["data"]][[schema]][["observations"]][["page"]][["result"]])
      
      
      
      #store the next page
      #nextpage<-responses$data$dataset_house_prices$observations$page$next_page
      nextpage<-responses[["data"]][[schema]][["observations"]][["page"]][["next_page"]]
      
      #Add the partial df results to the total results
      if(is.null(df_total)){
        df_total<-df
      }else{
        df_total<-rbind(df_total,df)  
      }
      
      #recrease the total by 1000
      total = total- 100000
    } # end while
    
    #there are less than 1000 matches  
  }else{
    qry_getObservations <- Query$new()
    qry_getObservations$query('query',paste('{',schema,'{observations(dimensions:{',fixed_str,'} ) {',
                                                        'page(first:100000){',
                                                        'next_page result {',
                                                        free_str,'}}}}}') )
    #Get the result
    responses <- client6$exec(qry_getObservations$queries$query)
                                                
    #store to a data frame the results
  df_total <- as.data.frame(responses[["data"]][[schema]][["observations"]][["page"]][["result"]])
                                                                                        }
  return(df_total)
}


