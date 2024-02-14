#Test su derivate seconde
load("./Datasets/data")
load("./Datasets/env")
library(splines)
library(roahd)
library(fdANOVA)
library(progress)
library(tidyr)
library(tidyverse)
library(robustbase)
library(mgcv)
library(fda)
### covariates and QoI ###
load("Datasets/data")
load("Datasets/env")
library(mgcv)
library(splines)
library(rgl)
library(robustbase)

total_curves <- matrix(0, nrow = length(17:50), ncol=length(prov)*length(years))
basis <- create.bspline.basis(rangeval=c(17,50), nbasis=9, norder=5)
lowercut <- 31
uppercut <- 302
for (i in 1:length(prov))
{
  for (j in 1:length(years))
  {
    Xsp <- smooth.basis(argvals=17:50, y=prov_list[[i]][j ,], fdParobj=basis)
    temp <-  eval.fd(17:50, Xsp$fd, Lfd=2)
    total_curves[, (j-1)*length(prov)+i] <- eval.fd(17:50, Xsp$fd, Lfd=2)
  }
}
total_curves <- data.frame(total_curves)

maxima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
new <- data.frame(x=seq(17,50,length=1000))
for (i in 1:length(years))
{
  for (j in 1:length(prov))
  {
    Xsp <- smooth.basis(argvals=17:50, y=prov_list[[j]][i ,], fdParobj=basis)
    temp <-  eval.fd(new$x, Xsp$fd, Lfd=2)
    maxima[((i-1)*length(prov)+j),1] <- prov[j]
    maxima[((i-1)*length(prov)+j),2] <- years[i]
    maxima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.max(temp)])
    maxima[((i-1)*length(prov)+j),4] <- as.numeric(max(temp))
  }
}
maxima <- data.frame(maxima)
names(maxima) <- c("Province", "Year", "MaxDomain", "Max")

color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))

# # define quantities of interest 
# maxima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
# new <- data.frame(x=seq(17,50,length=1000))
# for (i in 1:length(years))
# {
#   for (j in 1:length(prov))
#   {
#     temp <- data.frame(x=17:50, y=prov_list[[j]][i ,])
#     model <- with(temp, smooth.spline(x,y,df = 9))
#     grid <- predict(model, new)$y
#     maxima[((i-1)*length(prov)+j),1] <- prov[j]
#     maxima[((i-1)*length(prov)+j),2] <- years[i]
#     maxima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.max(grid$x)])
#     maxima[((i-1)*length(prov)+j),4] <- as.numeric(max(grid))
#   }
# }
# maxima <- data.frame(maxima)
# names(maxima) <- c("Province", "Year", "MaxDomain", "Max")

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

# dataset with maximum values for regions 
maxima.region <- merge(media.regione.max, media.regione.maxdom, by = c('Year', 'Region'), all.x = T)

plot(maxima.region$MaxDomain, maxima.region$Max, col = color_gray)

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

ds_reg[,c(3,4,5)] <- ds_reg[,c(3,4,5)]*100

# changing names
ds_reg[] <- lapply(ds_reg, function(x) gsub("Friuli-Venezia Giulia", 'Friuli_Venezia_Giulia', x))
ds_reg[] <- lapply(ds_reg, function(x) gsub('Emilia-Romagna', 'Emilia_Romagna', x))
ds_reg[] <- lapply(ds_reg, function(x) gsub("Trentino Alto Adige / Südtirol", 'Trentino_Alto_Adige', x))
ds_reg[] <- lapply(ds_reg, function(x) gsub("Valle d'Aosta / Vallée d'Aoste", 'Valle_d_Aosta', x))

ds_reg <- merge(maxima.region, ds_reg, by = c("Year", "Region"), all.x = T)

for(i in 3:12){
  ds_reg[,i] <- as.numeric(ds_reg[,i])
}

ds_reg$Area <- sapply(ds_reg$Region, associa_Area)

# write.csv(ds_reg, 'data_regression.csv', row.names = F)

rm(uni, pop, occ, grav)


### regression ###

## regression on MaxDomain in 2002:2021
# we consider emigrations, immigrations, unemployment and employment
model1.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                     + s(Immigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr') 
                     + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model1.maxdom)

# drop immigration
model2.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr') 
                     + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model2.maxdom)

# drop unemployment
model3.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model3.maxdom)


## regression on MaxDomain in 2008:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model11.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Dropouts, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model11.maxdom)


## regression on MaxDomain in 2010:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model21.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Dropouts, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model21.maxdom)

