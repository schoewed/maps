library(tidycensus)
library(tidyverse)
library(here)
library(sf)
library(ggplot2)
library(tidycensus)
library(leaflet)
library(broom)
library(scales)
install.packages("leaflet.extras")
library(leaflet.extras)
# Set the tigris_use_cache option
options(tigris_use_cache = TRUE)

# Get seasonal dataset with geometry set to TRUE
seasonal <- get_acs(geography = "county",
                        variables = c(seasonal = "B25004_006", owner_occ = "B07013_002"), 
                        geometry = TRUE,
                      output = "wide") %>% 
  mutate(szn_share = seasonalE/(seasonalE + owner_occE))

View(seasonal)
st_crs(seasonal)

# Plot the estimate to view a map of the data
plot(seasonal["estimate"])
View(seasonal)

seasonal2 <- st_transform(seasonal, crs = "+init=epsg:4326") %>% 
  filter(!is.na(szn_share)) %>% 
  arrange(desc(szn_share))
head(seasonal2)

pal <- colorNumeric(palette = "viridis", 
                    domain = seasonal2$szn_share)

bins <- (c(0,.1,.2,.3,.5,.1))
pal2 <- colorBin("YlOrRd", domain = seasonal2$szn_share, bins = bins)
pal3 <- colorNumeric(
  palette = "Blues",
  domain = seasonal2$szn_share)

#define pop-up
pop1 <- paste0("<strong>County: </strong>",
               str_extract(seasonal2$NAME, "^([^,]*)"),
               "<br><strong>2017 Seasonal Housing Estimate: </strong>",
               round(seasonal2$szn_share*100, digits = 2),
               "%")

#map seasonal housing
leaflet() %>% 
  addProviderTiles("CartoDB") %>%
  setView(-96, 37, zoom = 4) %>% 
  addPolygons(data = seasonal2,
              fillColor = ~pal(szn_share),
              weight = 0,
              opacity = .9,
              fillOpacity = .7,
              smoothFactor = 0,
              stroke = FALSE,
              popup = pop1) %>% 
  addLegend(data = seasonal2,
            position = "bottomright",
            pal = pal,
            values = seasonal2$szn_share,
            labFormat = labelFormat(suffix = "%", digits = 3, transform = function(x) 100*x),
            title = "Seasonal Housing<br>by County, 2017") %>% 
  addResetMapButton()

#scf chart
data <- read.csv("SCFP2016.csv")
second_homes <-data %>% 
  filter(NETWORTH > 100000 & NETWORTH <100000000) %>% 
  filter(ORESRE > 1000 & ORESRE < 10000000) %>%
  select(WGT, ORESRE, AGE, NETWORTH)

#good plot
p5 <- ggplot(second_homes, aes(x = NETWORTH, y = ORESRE, size = WGT, color=AGE)) +
  geom_point() +
  scale_x_continuous(name = "Household Net Worth (Log Scale)", breaks = c(100000,500000,1000000,5000000,10000000,50000000), labels = dollar) +
  scale_y_continuous(name = "Second Home(s) Value", breaks = seq(0,10000000,1000000), labels = dollar) +
  coord_trans(x = "log10") + 
  ggtitle("Net Worth and Second Home Value") + 
  theme(text = element_text(family="Arial Narrow")) + 
  scale_color_viridis_c() +
  theme_bw()

p5
