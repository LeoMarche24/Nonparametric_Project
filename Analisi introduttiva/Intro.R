library(readr)

nati <- read_csv("Nati totali.csv", 
                 col_types = cols(ITTER107 = col_skip(), 
                                  TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
                                  `Seleziona periodo` = col_skip(), 
                                  `Flag Codes` = col_skip(), Flags = col_skip()))
length(unique(nati$Territorio))
prov <- sort(unique(nati$Territorio))
years <- sort(unique(nati$TIME))[4:21]

birth <- matrix(rep(0, length(years)*length(prov)), nrow = length(prov), ncol = length(years))
for(p in 1:length(prov))
{
  for(y in 1:length(years))
  {
    birth[p,y] <- nati$Value[which(nati$Territorio == prov[p] & nati$TIME == years[y])]
  }
}

row.names(birth) <- prov
colnames(birth) <- years
rm(nati)

matplot(t(birth), type='l')

##Ora creo un altro dataset col numero di residenti

res <- read_csv("Residenti storico.csv", 
                col_types = cols(ITTER107 = col_skip(), TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
                                 ETA1 = col_skip(), `Classe di età` = col_skip(),SEXISTAT1 = col_skip(), Sesso = col_skip(), 
                                 CITTADINANZA = col_skip(), Cittadinanza = col_skip(), `Seleziona periodo` = col_skip(), 
                                 `Flag Codes` = col_skip(), Flags = col_skip()))

popolazione <- matrix(rep(0, length(years)*length(prov)), nrow = length(prov), ncol = length(years))
for(p in 1:length(prov))
{
  for(y in 1:length(years))
  {
    popolazione[p,y] <- res$Value[which(res$Territorio == prov[p] & res$TIME == years[y])]
  }
}

nati <- birth/popolazione
rm(list = c('birth', 'res', 'popolazione'))
matplot(t(nati), type='l', x = years)

temp = nati[,1] - nati[,18]
which.min(temp)

### Possiamo notare che per ogni singola provincia delta(2018,2002) sia > 0 

plot(temp, ylim= c(-0.0005, 0.005))
abline(h = 0, col = 2)

###Questo è il nostro dataset con cui iniziare l'analisi.

View(as.matrix(nati[, 1]))
View(as.matrix(nati[, 18]))

#Analisi funzionale (depth measure) sulla fda e magari derivate?
#Altro su indici specifici? 
#->Flourish per grafici fighi

d1 = nati[,1:9]
d2 = nati[,10:18]

m1  = colMeans(t(d1))
m2  = colMeans(t(d2))

plot( m1 , col = 2, pch = 16)
points(m2 , col = 3, pch = 16)

diff = m1-m2

which.min(diff)

ancona = nati[3,]

anconaDiff = diff(as.vector(ancona))
i = 0
perc = rep(1,16)
for(i in 1:16){
  perc[i] = 100*anconaDiff[i+1]/ancona[i]
  }
  


### leaflet


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





# MEI


library(roahd)

data = fData(grid = years, values = nati)


band_depth = BD(Data = nati)
median_curve_manual <- data[which.max(band_depth),]
grid_ecg <- seq(median_curve_manual$t0,median_curve_manual$tP,by=median_curve_manual$h)

plot(data)
lines(grid_ecg,median_curve_manual$values, lw = 2)

median.mbd =  median_fData(fData = data, type = "MBD")

library(DepthProc)


tukey.depth=depth(u=data$values,method='Tukey')
tukey.deepest.idx = which(tukey.depth==max(tukey.depth))
lines(years, data$values[tukey.deepest.idx[1],], col="red", lwd = 2)

plot(data)
mei.data= MEI(data)
which.max(mei.data)
which.min(mei.data)
lines(years, data$values[15,], col="green", lwd = 2)
lines(years, data$values[61,], col="blue", lwd = 2)
lines(years, data$values[tukey.deepest.idx[1],], col="red", lwd = 2)













