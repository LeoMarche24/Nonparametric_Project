# regression on maximum values of data
load("Datasets/data")
load("Datasets/env")
library(mgcv)
library(splines)
library(rgl)

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

ds_reg[,c(3,4,6)] <- ds_reg[,c(3,4,6)]*100
ds_reg[] <- lapply(ds_reg, function(x) gsub("Friuli-Venezia Giulia", 'Friuli_Venezia_Giulia', x))
ds_reg[] <- lapply(ds_reg, function(x) gsub('Emilia-Romagna', 'Emilia_Romagna', x))
ds_reg[] <- lapply(ds_reg, function(x) gsub("Trentino Alto Adige / Südtirol", 'Trentino_Alto_Adige', x))
ds_reg[] <- lapply(ds_reg, function(x) gsub("Valle d'Aosta / Vallée d'Aoste", 'Valle_d_Aosta', x))

ds_reg <- merge(maxima.region, ds_reg, by = c("Year", "Region"), all.x = T)

for(i in 3:12){
  ds_reg[,i] <- as.numeric(ds_reg[,i])
}

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


## regression on all italy per region ##

## regression on MaxDomain in 2002:2021
# we have data for emigrations, immigrations, unemployment and employment
model1.reg <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Immigrations, bs = 'cr')
              + s(Employment.rate, bs = 'cr') 
              + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model1.reg)

model2.reg <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                  + s(Employment.rate, bs = 'cr') 
                  + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model2.reg)

model3.reg <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model3.reg)

colors <- ifelse(ds_reg$Area == 'Nord', color_pal(3)[1], ifelse(ds_reg$Area == 'Centro', color_pal(3)[2], color_pal(3)[3]))
plot(ds_reg$Emigrations, ds_reg$Employment.rate, col = colors, pch = 19)
legend("bottomright", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

# regression without outliers
ds.no.outlier <- ds_reg[which(ds_reg$Emigrations < 25),] # to be changed
colors1 <- ifelse(ds.no.outlier$Area == 'Nord', color_pal(3)[1], ifelse(ds.no.outlier$Area == 'Centro', color_pal(3)[2], color_pal(3)[3]))
plot(ds.no.outlier$Emigrations, ds.no.outlier$Employment.rate, col = colors1, pch = 19)
legend("bottomright", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

model3.reg.no.out <- gam(MaxDomain ~ Emigrations 
                  + s(Employment.rate, bs = 'cr'), data = ds.no.outlier)
summary(model3.reg.no.out)

# plot 3d of this model 
emigrations.grid <- seq(range(ds.no.outlier$Emigrations)[1], range(ds.no.outlier$Emigrations)[2], length.out = 100)
employment.grid <- seq(range(ds.no.outlier$Employment.rate)[1], range(ds.no.outlier$Employment.rate)[2], length.out = 100)
grid <- expand.grid(emigrations.grid, employment.grid)
names(grid) <- c('Emigrations','Employment.rate')
pred <- predict(model3.reg.no.out, newdata = grid) 
persp3d(emigrations.grid, employment.grid, pred, col = 'grey30', border = "black", lwd=0.3)
points3d(ds.no.outlier$Emigrations, ds.no.outlier$Employment.rate, ds.no.outlier$MaxDomain, col = colors1, size=5)

# considering only employment.rate
model4.reg.no.out <- gam(MaxDomain ~ s(Employment.rate, bs = 'cr'), data = ds.no.outlier)
summary(model4.reg.no.out)

new_data_seq <- seq(min(ds.no.outlier$Employment.rate),
                    max(ds.no.outlier$Employment.rate), length.out = 100)
preds <- predict(model4.reg.no.out, newdata = list(Employment.rate = new_data_seq), se = T) 
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(ds.no.outlier$Employment.rate, ds.no.outlier$MaxDomain, col = color_gray)
lines(new_data_seq,preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)


## regression on MaxDomain in 2008:2017
# we have data for emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model11.reg <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
                  + s(Immigrations, bs = 'cr')
                  + s(Employment.rate, bs = 'cr') 
                  + s(Unemployment.rate, bs = 'cr')
                  + s(Women.enrolled, bs = 'cr')
                  + s(Dropouts, bs = 'cr'), data = ds_reg)
summary(model11.reg)

model12.reg <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr')
                   + s(Dropouts, bs = 'cr'), data = ds_reg)
summary(model12.reg)

model13.reg <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model13.reg)

model14.reg <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
                   + s(Employment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model14.reg)

model15.reg <- gam(MaxDomain ~ s(Employment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model15.reg)


women.grid <- seq(range(ds_reg[which(ds_reg$Year %in% 2008:2017),]$Women.enrolled)[1], range(ds_reg[which(ds_reg$Year %in% 2008:2017),]$Women.enrolled)[2], length.out = 100)
employment.grid <- seq(range(ds_reg[which(ds_reg$Year %in% 2008:2017),]$Employment.rate)[1], range(ds_reg[which(ds_reg$Year %in% 2008:2017),]$Employment.rate)[2], length.out = 100)
grid <- expand.grid(women.grid, employment.grid)
names(grid) <- c('Women.enrolled','Employment.rate')
pred <- predict(model15.reg, newdata = grid) 
persp3d(women.grid, employment.grid, pred, col = 'grey30', border = "black", lwd=0.3)
points3d(ds_reg[which(ds_reg$Year %in% 2008:2017),]$Women.enrolled, ds_reg[which(ds_reg$Year %in% 2008:2017),]$Employment.rate, ds_reg[which(ds_reg$Year %in% 2008:2017),]$MaxDomain, col = colors1, size=5)

# maybe do a plot separeting covariates to show the regression


## regression on Max in 2002:2021
model1.reg.max <- gam(Max ~ s(Emigrations, bs = 'cr') 
                  + s(Immigrations, bs = 'cr')
                  + s(Employment.rate, bs = 'cr') 
                  + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model1.reg.max)

model2.reg.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model2.reg.max)

model3.reg.max <- gam(Max ~ s(Immigrations, bs = 'cr') 
                  + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model3.reg.max)

plot(ds_reg$Unemployment.rate, ds_reg$Employment.rate, col = colors, pch = 19)
legend("topright", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))
# the two covariates are linear with each other

# drop unemployment
model4.reg.max <- gam(Max ~ s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model4.reg.max)

new_data_seq <- seq(min(ds_reg$Employment.rate),
                    max(ds_reg$Employment.rate), length.out = 100)
preds <- predict(model4.reg.max, newdata = list(Employment.rate = new_data_seq), se = T) 
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(ds_reg$Employment.rate, ds_reg$Max, col = color_gray)
lines(new_data_seq,preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)


## regression on Max in in 2008:2017
# we have data for emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model11.reg <- gam(Max ~ s(Emigrations, bs = 'cr') 
                   + s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr')
                   + s(Dropouts, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2017),])
summary(model11.reg)

model12.reg <- gam(Max ~ s(Emigrations, bs = 'cr') 
                   + s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2017),])
summary(model12.reg)

model13.reg <- gam(Max ~ s(Emigrations, bs = 'cr') 
                   + s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2017),])
summary(model13.reg)

model14.reg <- gam(Max ~ s(Emigrations, bs = 'cr') 
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2017),])
summary(model14.reg)

model15.reg <- gam(Max ~ s(Employment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg)
summary(model15.reg)


## regression on all italy per area

# model1: we consider employment, unemployment, immigrations and emigrations 
# as covariates x 2002:2021
model1 <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Immigrations, bs = 'cr')
              + s(Employment.rate, bs = 'cr') 
              + s(Unemployment.rate, bs = 'cr'), data = ds_area)
summary(model1)

model2 <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Immigrations, bs = 'cr')
              + s(Employment.rate, bs = 'cr'), data = ds_area)
summary(model2)

model3 <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
              + s(Employment.rate, bs = 'cr'), data = ds_area)
summary(model3)

plot(ds_area$Emigrations, ds_area$Employment.rate, col = factor(ds_area$Area))

emigrations.grid <- seq(range(ds_area$Emigrations)[1], range(ds_area$Emigrations)[2], length.out = 100)
employment.grid <- seq(range(ds_area$Employment.rate)[1], range(ds_area$Employment.rate)[2], length.out = 100)
grid <- expand.grid(emigrations.grid, employment.grid)
names(grid) <- c('Emigrations','Employment.rate')
pred <- predict(model3, newdata = grid) 
persp3d(emigrations.grid, employment.grid, pred, col = 'grey30', border = "black", lwd=0.3)
points3d(ds_area$Emigrations, ds_area$Employment.rate, ds_area$MaxDomain, col = color_pal(1), size=5)

model4 <- gam(MaxDomain ~ s(Emigrations, bs = 'cr'), data = ds_area)
summary(model4)
plot(model4)

model5 <- gam(MaxDomain ~ s(Employment.rate, bs = 'cr'),data = ds_area)
summary(model5)
plot(model5)

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











