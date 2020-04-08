
#define metro and index to county codes
county_codes <- function(metro_name){
  cb <- core_based_statistical_areas(cb = TRUE)
  co <- counties(cb = TRUE)
  
  metro <- filter(cb, NAME == metro_name)
  
  #getting centroid for map
  #metro_ct <-st_centroid(metro) %>% 
    #st_coordinates(metro_ct)
  
  co_surr <- co[metro,]
  co_list <- st_within(co_surr, metro)
  co_in <- map_lgl(co_list, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  co_fin <- co_surr[co_in,]
  co_fin <- co_fin$GEOID
 message("counties selected") 
  
  
  
  #retrieve tracts
  tracts_output <- NULL
  for (i in co_fin) {
    cty_tracts <- get_acs(geography = "tract",
                          variables = c(med_inc = "B19013_001", pop = "B01003_001", hh = "B09019_001"),
                          state = substr(i, 1, 2),
                          county = substr(i, 3, 5),
                          year = 2018,
                          output = "wide",
                          geometry = TRUE) %>%
      mutate(cty_code = i)
    assign('tracts_output', rbind(cty_tracts, tracts_output))
  }
  tracts_output <- st_transform(tracts_output, crs = "+init=epsg:4326")
  return(tracts_output)
}