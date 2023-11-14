### QUA TI PUOI SBIZZARRIRE

library(dplyr)
library(leaflet)
library(rgdal)
library(sf)

# Import Italy shapefile
ita_map = readOGR('C:/[...]/Limiti01012020_g/ProvCM01012020_g', 
                  'ProvCM01012020_g_WGS84', stringsAsFactors = FALSE)

# Merge
colnames(data_19)[1] = "COD_PROV" # <--- This is the numerical provincial code
ita_map_sf = st_as_sf(ita_map)
ita_map_sf$COD_PROV <- as.numeric(ita_map_sf$COD_PROV)
ita_map_data <- left_join(ita_map_sf, data_19, by = "COD_PROV")

# Specify Color Palette
pal <- colorQuantile("Blues", domain = ita_map_data$unilav_ula, n=5)

# Generate map with leaflet
ita_map_data %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lat = 41.8719, lng = 12.5674, zoom = 5) %>%
  addPolygons(
    fillColor = ~pal(unilav_ula),
    stroke = FALSE,
    smoothFactor = 0.2,
    fillOpacity = 0.7,
    label=~unilav_ula
  )