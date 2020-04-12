library(devtools)
library(tidyverse)
library(tigris)
library(sf)
devtools::install_github("jamgreen/lehdr")
library(lehdr)
library(leaflet)
library(RColorBrewer)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#or_od <- grab_lodes(state = "or", year = 2014, lodes_type = "od", job_type = "JT01", 
                    #segment = "S000", state_part = "main", agg_geo = "tract")


#msa_jobs <- function(metro_name, select_year){
metro_name <- "Lynchburg, VA"

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
  View(co_fin)
  message("counties selected") 
  
  #retrieve all msa tracts
  tracts_output <- NULL
  for (i in co_fin) {
    cty_tracts <- tracts(state = substr(i, 1, 2),
                         county = substr(i, 3, 5),
                         cb = TRUE)%>%
      mutate(cty_code = i)
    assign('tracts_output', rbind(cty_tracts, tracts_output))
  }
  
  #get state jobs
  state_jobs_output <- NULL
  st_fin <- unique(substr(co_fin, 1, 2))
  for (i in st_fin) {
    st_jobs <- grab_lodes(state = "va",
                          year = 2016,
                          lodes_type = "od",
                          job_type = "JT01",
                          state_part = "main",
                          segment = "S000",
                          agg_geo = "tract")
    assign('state_jobs_output', rbind(st_jobs, state_jobs_output))
  }
  
  head(state_jobs_output)
  head(tracts_output)
  #get jobs just for tracts of interest
  msa_jobs_output <- left_join(tracts_output, state_jobs_output, by = c("GEOID" = "w_tract")) %>% 
    st_transform(msa_jobs_output, crs = "+init=epsg:4326") %>% 
    mutate(jobs_dens = S000/(ALAND*0.00000038610215855))

  pal <- colorNumeric("Spectral", msa_jobs_output$jobs_dens)
  msa_jobs_output$jobs_dens
  
  leaflet() %>% 
    addProviderTiles("CartoDB") %>% 
    addPolygons(data = msa_jobs_output,
                fillColor = ~pal(msa_jobs_output$jobs_dens),
                popup = msa_jobs_output$jobs_dens,
                color = "black",
                weight = 0,
                opacity = 1)
  