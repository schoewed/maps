
#define metro and index to county codes
county_area_water <- function(metro_name){
  cb <- core_based_statistical_areas(cb = TRUE)
  co <- counties(cb = TRUE)
  
  metro <- filter(cb, NAME == metro_name)

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
 
 #loop through area water
 county_lake <- NULL
 for (i in co_fin) {
   int_lake <- area_water(as.numeric(substr(i, 1, 2)),
                          as.numeric(substr(i, 3, 5)))
   assign("county_lake", rbind(int_lake, county_lake))}
 county_lake <-st_transform(county_lake, crs = "+init=epsg:4326")

  return(county_lake)
}