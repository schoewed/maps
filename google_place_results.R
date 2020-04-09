google_place_results <- function(search_list, search_location_ctr,
                                 radius_in_miles, api_key_input){
  
  #inputs for search - can change to custom list or list of vectors from csv
  search_type <- search_list
  search_loc <- search_location_ctr
  rad <- 1609.34*radius_in_miles
  register_google(key = api_key_input, write = TRUE)
  api_key <- api_key_input
  output <- NULL
  
  #for each item in search list, do search
  for (i in search_type){
    res <- googleway::google_places(search_string = paste0(i," ",search_loc),
                                    radius=rad,
                                    key=api_key)
    
    #if results, pull first 20 records
    res_results <- as.data.frame(res$results)
    geometry <- res_results$geometry$location
    res_results <- select(res_results, c(name, formatted_address))
    res_results <- cbind(res_results,geometry) %>% 
      mutate(search_id = i)
    
    #define pagination token  
    token <- res$next_page_token
    #sleep for two secs
    message("first page pulled, sleeping")
    Sys.sleep(3)
    
    #if token isn't null (i.e. there are more results), then pull next 20, otherwise move to next item in list
    if (!is.null(token)) {
      res2 <- googleway::google_places(search_string = paste0(i," ",search_loc),
                                       radius=rad,
                                       page_token=token,
                                       key=api_key)
      res_next <- as.data.frame(res2$results)
      geometry <- res_next$geometry$location
      res_next <- select(res_next, c(name, formatted_address))
      res_next <- cbind(res_next,geometry)%>%
        mutate(search_id = i)
      
      #redefine token and rest
      token <- res2$next_page_token
      message("second page pulled, sleeping")
      Sys.sleep(3)
    } else {
      res_next <- NA
      next
    }
    
    #if pg 2 token isn't null (i.e. there are more results), then pull next 20, otherwise move to next item in list
    if (!is.null(token)) {
      res3 <- googleway::google_places(search_string = paste0(i," ",search_loc),
                                       radius=rad,
                                       page_token=token,
                                       key=api_key)
      res_last <- as.data.frame(res3$results)
      geometry <- res_last$geometry$location
      res_last <- select(res_last, c(name, formatted_address))
      res_last <- cbind(res_last,geometry)%>%
        mutate(search_id = i)
      
      #redefine token and rest
      token <- res3$next_page_token
      message("third page pulled, sleeping")
      Sys.sleep(3)
    }
    else {
      res_last <- NA
      next
    }
    
    #combine pages
    res_final <- rbind(res_results,res_next,res_last)
    #assign pages for each variable to output dataframe
    assign('output', rbind(res_final, output))
  }
  return (output)
}