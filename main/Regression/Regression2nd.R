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
library(rgl)

### covariates and QoI ###

maxima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
new <- data.frame(x=seq(20,30,length=1000))
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


### regression second derivative ###

# MAX DOMAIN

## regression on MaxDomain in 2002:2021
# we consider emigrations, immigrations, unemployment and employment
model1.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                     + s(Immigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr') 
                     + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model1.maxdom) # R-sq.(adj) =  0.283

# drop immigration
model2.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr') 
                     + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model2.maxdom) # R-sq.(adj) =  0.283

# drop unemployment
model3.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model3.maxdom) #R-sq.(adj) = 0.212
 

## regression on MaxDomain in 2008:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model11.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Dropouts, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model11.maxdom) # R-sq.(adj) =  0.482

# drop dropouts and moving to 2008:2021
model12.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model12.maxdom) # R-sq.(adj) =  0.413

# drop emigrations
model13.maxdom <- gam(MaxDomain ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model13.maxdom) # R-sq.(adj) =   0.39

# trying to drop unemployment
model14.maxdom <- gam(MaxDomain ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model14.maxdom) # R-sq.(adj) =   0.324


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
summary(model21.maxdom) # R-sq.(adj) =  0.535

# drop dropouts and move to 2010:2021
model22.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg)
summary(model22.maxdom) # R-sq.(adj) =  0.454

# drop abortion 3034 (make sense as 20 < MaxDom < 30) 
model23.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr'), data = ds_reg)
summary(model23.maxdom) # R-sq.(adj) =  0.456 <- in fact pvalue increases

# drop Unemployment.rate
model24.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr'), data = ds_reg)
summary(model24.maxdom) # R-sq.(adj) =  0.43 

# drop Abortions.2529
model25.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model25.maxdom) # R-sq.(adj) =  0.354 

# drop Emigrations
model25.maxdom <- gam(MaxDomain ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model25.maxdom) # R-sq.(adj) =  0.354 


# MAX 

## regression on MaxDomain in 2002:2021
# we consider emigrations, immigrations, unemployment and employment
model1.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                     + s(Immigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr') 
                     + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model1.max) # R-sq.(adj) =  0.388

# drop unemployment.rate
model1.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                     + s(Immigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model1.max) # R-sq.(adj) =  0.313


## regression on MaxDomain in 2008:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model11.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Dropouts, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model11.max) # R-sq.(adj) =  0.482

# drop droputs and unemployment rate

model12.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model12.max) # R-sq.(adj) =  0.607

# drop droputs and 
model13.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model13.max) # R-sq.(adj) =  0.527


## regression on MaxDomain in 2010:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model21.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Dropouts, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model21.max) # R-sq.(adj) =  0.696

# drop Dropouts
model22.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model22.max) # R-sq.(adj) =  0.683

# drop Dropouts
model23.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model23.max) # R-sq.(adj) =  0.555

# drop abortion
model24.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model24.max) # R-sq.(adj) =  0.544

# linearizing

model25.max <- gam(Max ~ Immigrations
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model25.max) # R-sq.(adj) =  0.486



# final models

# MAX DOMAIN
model25.maxdom <- gam(MaxDomain ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model25.maxdom) # R-sq.(adj) =  0.324 

model25.maxdom.lin <- gam(MaxDomain ~ Immigrations
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model25.maxdom.lin) # R-sq.(adj) =  0.285

anova(model25.maxdom.lin, model25.maxdom, test = "F") 
# p-value = 0.01168 < alpha => the linear model is better

# MAX
model24.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model24.max) # R-sq.(adj) =  0.489

model24.max.lin <- gam(Max ~ Immigrations
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model24.max.lin) # R-sq.(adj) =  0.434

anova(model24.max, model24.max.lin, test = "F") 
# p-value = 0.0001094 < alpha => the linear model is better

# outlier
data_regression <- ds_reg[which(ds_reg$Year %in% 2008:2021),]
fit_MCD <- covMcd(x = data_regression[,c("Immigrations", "Employment.rate","Women.enrolled")], alpha = 0.95, nsamp = 1000)
fit_MCD$raw.center
fit_MCD$raw.cov
ind_best_subset <- fit_MCD$best
N <- nrow(data_regression[,c("Immigrations", "Employment.rate","Women.enrolled")])

# outlier indeces
ind_out_MCD <- setdiff(1:N,fit_MCD$best)

data.no.out.2nd <- data_regression[-ind_out_MCD,]
data_regression[ind_out_MCD,c("Year","Region")] # tolgo solo la valle d'aosta

# FINAL MODELS 4L

# MAX DOMAIN
model.fin.maxdom.no.out.lin <- gam(MaxDomain ~ Immigrations
                                   + s(Employment.rate, bs = 'cr') 
                                   + s(Women.enrolled, bs = 'cr'), data = data.no.out.2nd)
summary(model.fin.maxdom.no.out.lin) # R-sq.(adj) =  0.312

# MAX 
model.fin.max.no.out.lin <- gam(Max ~ Immigrations
                       + s(Employment.rate, bs = 'cr') 
                       + s(Women.enrolled, bs = 'cr'), data = data.no.out.2nd)
summary(model.fin.max.no.out.lin) # R-sq.(adj) =  0.479


## plots MaxDomain ## 
# plot 1
new_data_seq <- seq(min(data.no.out.2nd$Employment.rate), max(data.no.out.2nd$Employment.rate), length.out = 100)
new_data_seq1 <- rep(median(data.no.out.2nd$Immigrations), length = 100)
new_data_seq2 <- rep(median(data.no.out.2nd$Women.enrolled), length = 100)

preds <- predict(model.fin.maxdom.no.out.lin, newdata = list(Employment.rate = new_data_seq,
                                                              Immigrations = new_data_seq1,
                                                              Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out.2nd$Employment.rate, data.no.out.2nd$MaxDomain, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Employment rate', ylab = 'Max domain')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 2
new_data_seq <- seq(min(data.no.out.2nd$Immigrations), max(data.no.out.2nd$Immigrations), length.out = 100)
new_data_seq1 <- rep(median(data.no.out.2nd$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out.2nd$Women.enrolled), length = 100)

preds <- predict(model.fin.maxdom.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                             Immigrations = new_data_seq,
                                                             Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out.2nd$Immigrations, data.no.out.2nd$MaxDomain, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Immigrations', ylab = 'Max domain')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 3
new_data_seq <- seq(min(data.no.out.2nd$Women.enrolled), max(data.no.out.2nd$Women.enrolled), length.out = 100)
new_data_seq1 <- rep(median(data.no.out.2nd$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out.2nd$Immigrations), length = 100)

preds <- predict(model.fin.maxdom.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                             Immigrations = new_data_seq2,
                                                             Women.enrolled = new_data_seq), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out.2nd$Women.enrolled, data.no.out.2nd$MaxDomain, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Women enrolled', ylab = 'Max domain')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)


## plots Max ## 
# plot 1
new_data_seq <- seq(min(data.no.out.2nd$Employment.rate), max(data.no.out.2nd$Employment.rate), length.out = 100)
new_data_seq1 <- rep(median(data.no.out.2nd$Immigrations), length = 100)
new_data_seq2 <- rep(median(data.no.out.2nd$Women.enrolled), length = 100)

preds <- predict(model.fin.max.no.out.lin, newdata = list(Employment.rate = new_data_seq, 
                                                          Immigrations = new_data_seq1,
                                                          Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out.2nd$Employment.rate, data.no.out.2nd$Max, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Employment rate', ylab = 'Max')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 2
new_data_seq <- seq(min(data.no.out.2nd$Immigrations), max(data.no.out.2nd$Immigrations), length.out = 100)
new_data_seq1 <- rep(median(data.no.out.2nd$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out.2nd$Women.enrolled), length = 100)

preds <- predict(model.fin.max.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                          Immigrations = new_data_seq,
                                                          Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out.2nd$Immigrations, data.no.out.2nd$Max, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Immigrations', ylab = 'Max')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 3
new_data_seq <- seq(min(data.no.out.2nd$Women.enrolled), max(data.no.out.2nd$Women.enrolled), length.out = 100)
new_data_seq1 <- rep(median(data.no.out.2nd$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out.2nd$Immigrations), length = 100)

preds <- predict(model.fin.max.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                          Immigrations = new_data_seq2,
                                                          Women.enrolled = new_data_seq), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out.2nd$Women.enrolled, data.no.out.2nd$Max, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Women enrolled', ylab = 'Max')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# write.csv(data.no.out.2nd, 'data_no_out2nd.csv', row.names = F)
