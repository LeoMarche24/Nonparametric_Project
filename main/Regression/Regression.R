# regression on maximum values of data
load("Datasets/data")
load("Datasets/env")
library(mgcv)
library(splines)

color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))

# taking the QoI 
maxima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
new <- data.frame(x=seq(17,50,length=1000))
for (i in 1:length(years))
{
  for (j in 1:length(prov))
  {
    temp <- data.frame(x=17:50, y=prov_list[[j]][i ,])
    model <- with(temp, smooth.spline(x,y,df = 9))
    grid <- predict(model, new)$y
    maxima[((i-1)*length(prov)+j),1] <- prov[j]
    maxima[((i-1)*length(prov)+j),2] <- years[i]
    maxima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.max(grid$x)])
    maxima[((i-1)*length(prov)+j),4] <- as.numeric(max(grid))
  }
}
maxima <- data.frame(maxima)
names(maxima) <- c("Province", "Year", "MaxDomain", "Max")

data_max <- data.frame(x=as.numeric(maxima$MaxDomain),y=as.numeric(maxima$Max))
plot(data_max, col = color_gray)

# create columns for regions in maxima
associa_regione <- function(provincia) {
  regioni <- list(
    Abruzzo = c("L'Aquila", "Chieti", "Pescara", "Teramo"),
    Basilicata = c("Matera", "Potenza"),
    Calabria = c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia"),
    Campania = c("Avellino", "Benevento", "Caserta", "Napoli", "Salerno"),
    Emilia_Romagna = c("Bologna", "Ferrara", "Forlì-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini"),
    Friuli_Venezia_Giulia = c("Gorizia", "Pordenone", "Trieste", "Udine"),
    Lazio = c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo"),
    Liguria = c("Genova", "Imperia", "La Spezia", "Savona"),
    Lombardia = c("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese"),
    Marche = c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino"),
    Molise = c("Campobasso", "Isernia"),
    Piemonte = c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli"),
    Puglia = c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto"),
    Sardegna = c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna"),
    Sicilia = c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani"),
    Toscana = c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa-Carrara", "Pisa", "Pistoia", "Prato", "Siena"),
    Trentino_Alto_Adige = c("Bolzano / Bozen", "Trento"),
    Umbria = c("Perugia", "Terni"),
    Valle_d_Aosta = c("Valle d'Aosta / Vallée d'Aoste"),
    Veneto = c("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")
  )
  
  for(regione in names(regioni)) {
    if(provincia %in% regioni[[regione]]) {
      return(regione)
    }
  }
  return(NA) 
}

# create columns for regions in maxima
associa_Area <- function(regione) {
  zone <- list(
    Nord = c('Friuli_Venezia_Giulia', 'Liguria', 'Lombardia', 'Piemonte', 'Trentino_Alto_Adige', 'Valle_d_Aosta', 'Veneto', "Friuli-Venezia Giulia", "Trentino Alto Adige / Südtirol", "Valle d'Aosta / Vallée d'Aoste"),
    Centro = c('Emilia_Romagna', 'Lazio', 'Marche', 'Molise', 'Toscana', 'Umbria', "Emilia-Romagna"),
    Sud = c('Abruzzo', 'Basilicata', 'Calabria', 'Campania', 'Puglia', 'Sardegna', 'Sicilia')
  )
  
  for(Area in names(zone)) {
    if(regione %in% zone[[Area]]) {
      return(Area)
    }
  }
  return(NA) 
}


maxima$Region <- sapply(maxima$Province, associa_regione)
maxima$Area <- sapply(maxima$Region, associa_Area)

# mean of max values per region and area
maxima$MaxDomain <- as.numeric(maxima$MaxDomain)
maxima$Max <- as.numeric(maxima$Max)

media.regione.max <- aggregate(Max ~ Region + Year, data = maxima, FUN = function(x) mean(x, trim = 0.1))
media.regione.maxdom <- aggregate(MaxDomain ~ Region + Year, data = maxima, FUN = function(x) mean(x, trim = 0.1))

media.area.max <- aggregate(Max ~ Area + Year, data = maxima, FUN = function(x) mean(x, trim = 0.1))
media.area.maxdom <- aggregate(MaxDomain ~ Area + Year, data = maxima, FUN = mean)

# dataset with maximum values for regions 
maxima.region <- merge(media.regione.max, media.regione.maxdom, by = c('Year', 'Region'), all.x = T)
maxima.area <- merge(media.area.max, media.area.maxdom, by = c('Year', 'Area'), all.x = T)

plot(maxima.region$MaxDomain, maxima.region$Max, col = color_gray)
plot(maxima.area$MaxDomain, maxima.area$Max, col = factor(maxima.area$Area), pch=19)


# load covariates 
uni <- read.csv('Regression/Dati_clean/dati_uni.txt', header = T)
pop <- read.csv('Regression/Dati_clean/dati_immigrazioni_emigrazioni.txt', header = T)
occ <- read.csv('Regression/Dati_clean/dati_inattivita_occupazione.txt', header = T)
grav <- read.csv('Regression/Dati_clean/dati_interruzioni_gravidanze.txt', header = T)

names(uni)[names(uni) == "Rinunce"] <- "Dropouts"

grav.split <- split(grav, factor(grav$Età))

grav <- cbind(grav.split$`25-29 anni`, grav.split$`30-34 anni`)[,c(1,2,4,8)]
names(grav)[names(grav) == "Abortions"] <- "Abortions.2529"
names(grav)[names(grav) == "Abortions.1"] <- "Abortions.3034"

# aggregate covariate and response in one dataset
ds_reg <- merge(pop, uni,  by = c("Year", "Region"), all.x = T)
ds_reg <- merge(ds_reg, occ, by=c("Year", "Region"), all.x = T)
ds_reg <- merge(ds_reg, grav, by=c("Year", "Region"), all.x = T)
ds_reg$Area <- sapply(ds_reg$Region, associa_Area)

ds_area1 <- aggregate(cbind(Emigrations, Immigrations, Employment.rate, Unemployment.rate) ~ Area + Year, data = ds_reg, FUN = function(x) mean(x, trim = 0.1))
ds_area2 <- aggregate(cbind(Abortions.2529, Abortions.3034) ~ Area + Year, data = ds_reg, FUN = function(x) mean(x, trim = 0.1))
ds_area3 <- aggregate(Dropouts ~ Area + Year, data = ds_reg, FUN = function(x) mean(x, trim = 0.1))
ds_area4 <- aggregate(Women.enrolled ~ Area + Year, data = ds_reg, FUN = function(x) mean(x, trim = 0.1))
ds_area <- merge(ds_area1, ds_area2, by=c("Year", "Area"), all.x = T)
ds_area <- merge(ds_area, ds_area3, by=c("Year", "Area"), all.x = T)
ds_area <- merge(ds_area, ds_area4, by=c("Year", "Area"), all.x = T)

rm(uni, pop, occ, grav, ds_area1, ds_area2, ds_area3, ds_area4)

ds_area <- merge(maxima.area, ds_area, by = c("Year", "Area"), all.x = T)
ds_area[,c(5,6,12)] <- ds_area[,c(5,6,12)]*100
  

## regression on all italy

# model1: we consider employment, unemployment, immigrations and emigrations 
# as covariates x 2002:2021
model1.bs <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Immigrations, bs = 'cr')
              + s(Employment.rate, bs = 'cr') 
              + s(Unemployment.rate, bs = 'cr'), data = ds_area)
summary(model1)

model2 <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Immigrations, bs = 'cr')
              + s(Employment.rate, bs = 'cr') 
              , data = ds_area)
summary(model2)

model3 <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Employment.rate, bs = 'cr') 
              , data = ds_area)
summary(model3)

model_gam <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
                 , data = ds_area)
model_gam <- gam(MaxDomain ~ s(Employment.rate, bs = 'cr'),
                 data = ds_area)

summary(model_gam)
# plot

## regression on areas
# split area
ds.split <- split(ds_area, factor(ds_area$Area))
# model1: we consider employment, unemployment, Immigrations and emigrations 
# as covariates x 2002:2021 on areas
model1 <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Immigrations, bs = 'cr')
              + s(Employment.rate, bs = 'cr') 
              + s(Unemployment.rate, bs = 'cr'), data = ds_area)
summary(model1)


## maxima: 2002-2021 x regioni
# uni: 2004- 2020 x regioni
# inizierei considerando solamente le iscrizioni all'università delle donne

# vedo due possibili modi di procedere:
# -> fare una regressione dove maxima è cumulativo sulle regioni [20 righe -> un dato per regione]
# -> fare una regressione dove maxima è cumulativo sugli anni [20 righe -> un dato per anno]
# per poi trattare allo stesso modo il dato della covariata

# una prima prova stupida
# -> sommo tutti i dati degli stessi anni

# -> stesso per uni


#split











