
#define metro and index to county codes
state_roads <- function(metro_name){
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
  
 #get roads available at state level
 st_roads <- NULL
 uni_states <- unique(substr(co_fin, 1, 2))
 for (i in uni_states) {
   int_rd <- primary_secondary_roads(as.numeric(substr(i, 1, 2)), year = 2018) %>% 
     filter(RTTYP %in% c("I", "S", "U"))
   assign("st_roads", rbind(int_rd, st_roads))}
 st_roads <- st_transform(st_roads, crs = "+init=epsg:4326")
 return(st_roads)
}



