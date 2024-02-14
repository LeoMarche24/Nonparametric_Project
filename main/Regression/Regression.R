### covariates and QoI ###
load("Datasets/data")
load("Datasets/env")
library(mgcv)
library(splines)
library(rgl)
library(robustbase)

color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))

# define quantities of interest 
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

# drop Immigrations
model2.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr')
                     + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model2.maxdom)

# collinearity between Employment and Unemployment rates
colors <- ifelse(ds_reg$Area == 'Nord', color_pal(3)[1], ifelse(ds_reg$Area == 'Centro', color_pal(3)[2], color_pal(3)[3]))
plot(ds_reg$Unemployment.rate, ds_reg$Employment.rate, col = colors, pch = 19, xlab = 'Unemployment rate', ylab = 'Employment rate')
legend("topright", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

plot(ds_reg$Immigrations, ds_reg$Emigrations, col = colors, pch = 19, xlab = 'Immigrations', ylab = 'Emigrations')
legend("bottomright", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

# drop Unemployment rate
model3.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr') 
                     + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model3.maxdom) # R2 = 0.343 

# linear effect on Emigrations
model5.maxdom <- gam(MaxDomain ~ Emigrations 
                     + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model5.maxdom) # R2 = 0.324


## regression on MaxDomain in 2008:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model11.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Dropouts, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model11.maxdom)
# Dropouts is not significant, hence we drop it and extend the model til 2021

# drop Dropouts
model12.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model12.maxdom)

# drop Emigrations
model13.maxdom <- gam(MaxDomain ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model13.maxdom)

# collinearity between Unemployment and Employment rates
plot(ds_reg[which(ds_reg$Year %in% 2008:2021),]$Unemployment.rate, ds_reg[which(ds_reg$Year %in% 2008:2021),]$Employment.rate, col = colors, pch = 19)
legend("bottomright", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

# drop Unemployment rate 
model14.maxdom <- gam(MaxDomain ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model14.maxdom) # R2 =  0.586


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
# Dropouts is not significant, hence we drop it and extend the model til 2021

# drop Dropouts
model22.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model22.maxdom)

# drop Abortions.2529
model23.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model23.maxdom)

# drop Immigrations
model23.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model23.maxdom)

# drop Unemployment.rate
model24.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Women.enrolled, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model24.maxdom)

# drop Women.enrolled
model24.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model24.maxdom) # R2 = 0.591

# drop Abortions30.34
model24.maxdom <- gam(MaxDomain ~ s(Emigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model24.maxdom) # R2 = 0.343


## regression on Max in 2002:2021
# we consider emigrations, immigrations, unemployment and employment
model1.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                  + s(Immigrations, bs = 'cr')
                  + s(Employment.rate, bs = 'cr') 
                  + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model1.max)

# drop Emigrations
model2.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                  + s(Employment.rate, bs = 'cr') 
                  + s(Unemployment.rate, bs = 'cr'), data = ds_reg)
summary(model2.max)

# drop Unemployment rate (due to collinearity with Employment)
model3.max <- gam(Max ~ s(Immigrations, bs = 'cr') 
                  + s(Employment.rate, bs = 'cr'), data = ds_reg)
summary(model3.max) # R2 = 0.439


## regression on Max in in 2008:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model11.max <- gam(Max ~ s(Emigrations, bs = 'cr') 
                  + s(Immigrations, bs = 'cr')
                  + s(Employment.rate, bs = 'cr') 
                  + s(Unemployment.rate, bs = 'cr')
                  + s(Women.enrolled, bs = 'cr')
                  + s(Dropouts, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model11.max)

# drop Emigrations
model12.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr')
                   + s(Dropouts, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2020),])
summary(model12.max)
# Dropouts is not significant, hence we drop it and extend the model til 2021

# drop Dropouts
model13.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model13.max)

# drop Unemployment.rate
model14.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model14.max) # R2 = 0.62


## regression on Max in 2010:2020
# we consider emigrations, immigrations, unemployment, employment, women.enrolled and dropouts
model21.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                      + s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr') 
                      + s(Unemployment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr')
                      + s(Dropouts, bs = 'cr')
                      + s(Abortions.2529, bs = 'cr')
                      + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model21.max)

# drop Abortions.2529
model22.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                   + s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr')
                   + s(Dropouts, bs = 'cr')
                   + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2020),])
summary(model22.max)
# Dropouts is not significant, hence we drop it and extend the model til 2021

# drop Dropouts
model23.max <- gam(Max ~ s(Emigrations, bs = 'cr')
                   + s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr')
                   + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model23.max)

# drop Emigrations
model24.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Unemployment.rate, bs = 'cr')
                   + s(Women.enrolled, bs = 'cr')
                   + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model24.max)

# drop Unemployment.rate
model25.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr')
                   + s(Abortions.3034, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model25.max)

# drop Abortions.3034
model26.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                   + s(Employment.rate, bs = 'cr') 
                   + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2010:2021),])
summary(model26.max) # R2 = 0.621


### final models ###

## models ##
## MaxDomain (2008:2021)
model.fin.maxdom <- gam(MaxDomain ~ s(Immigrations, bs = 'cr')
                      + s(Employment.rate, bs = 'cr')
                      + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model.fin.maxdom) # R2 =  0.586


model.fin.maxdom.lin <- gam(MaxDomain ~ Immigrations 
                        + s(Employment.rate, bs = 'cr')
                        + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model.fin.maxdom.lin) # R2 = 0.573

# comparing models
qqnorm(model.fin.maxdom.lin$residuals, main = '')
qqline(model.fin.maxdom.lin$residuals, col = color_pal(2)[2], lwd = 2)
qqnorm(model.fin.maxdom$residuals, main = '')
qqline(model.fin.maxdom$residuals, col = color_pal(2)[2], lwd = 2)

anova(model.fin.maxdom.lin, model.fin.maxdom, test = "F") 
# p-value = 0.01721 < alpha => the linear model is better


## Max (2008:2021)
model.fin.max <- gam(Max ~ s(Immigrations, bs = 'cr')
                     + s(Employment.rate, bs = 'cr') 
                     + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model.fin.max) # R2 = 0.62

model.fin.max.lin <- gam(Max ~ Immigrations
                     + s(Employment.rate, bs = 'cr') 
                     + s(Women.enrolled, bs = 'cr'), data = ds_reg[which(ds_reg$Year %in% 2008:2021),])
summary(model.fin.max.lin) # R2 = 0.582

# comparing models
qqnorm(model.fin.max.lin$residuals, main = '')
qqline(model.fin.max.lin$residuals, col = color_pal(2)[2], lwd = 2)
qqnorm(model.fin.max$residuals, main = '')
qqline(model.fin.max$residuals, col = color_pal(2)[2], lwd = 2)

anova(model.fin.max.lin, model.fin.max, test = "F") 
# p-value = 8.152e-05 < alpha => the reduced model is better


## outlier detection ##
# visually we identify the following areas as possible outliers
data_regression <- ds_reg[which(ds_reg$Year %in% 2008:2021),]
colors.05 <- ifelse(data_regression$Area == 'Nord', color_pal(3)[1], ifelse(data_regression$Area == 'Centro', color_pal(3)[2], color_pal(3)[3]))

plot(data_regression$Employment.rate, data_regression$Immigrations, col = colors.05, pch = 19, xlab = 'Employment rate', ylab = 'Immigrations')
legend("topleft", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

plot(data_regression$Employment.rate, data_regression$Emigrations, col = colors.05, pch = 19, xlab = 'Employment rate', ylab = 'Emigrations')
legend("topleft", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

ds.no.outlier <- ds_reg[which(ds_reg$Immigrations < 25),] 
colors1 <- ifelse(ds.no.outlier$Area == 'Nord', color_pal(3)[1], ifelse(ds.no.outlier$Area == 'Centro', color_pal(3)[2], color_pal(3)[3]))

plot(data_regression[which(data_regression$Region != 'Valle_d_Aosta'),]$Women.enrolled, data_regression[which(data_regression$Region != 'Valle_d_Aosta'),]$Immigrations,  col = colors1, pch = 19)
legend("topright", legend = c('Nord', 'Centro', 'Sud'), fill = c(color_pal(3)[1], color_pal(3)[2], color_pal(3)[3]))

visual.out <- data_regression[which(data_regression$Immigrations > 25 ),c("Region","Emigrations", "Employment.rate")]
visual.alpha <- 1 - nrow(visual.out)/nrow(data_regression) # 0.95
# then we round our alpha to 0.95

fit_MCD1 <- covMcd(x = data_regression[,c("Emigrations", "Employment.rate","Women.enrolled")], alpha = 0.95, nsamp = 1000)
fit_MCD1$raw.center
fit_MCD1$raw.cov
ind_best_subset1 <- fit_MCD1$best
N1 <- nrow(data_regression[,c("Emigrations", "Employment.rate","Women.enrolled")])

fit_MCD2 <- covMcd(x = data_regression[,c("Immigrations", "Employment.rate","Women.enrolled")], alpha = 0.95, nsamp = 1000)
fit_MCD2$raw.center
fit_MCD2$raw.cov
ind_best_subset2 <- fit_MCD2$best
N2 <- nrow(data_regression[,c("Immigrations", "Employment.rate","Women.enrolled")])

# outlier indeces
ind_out_MCD1 <- setdiff(1:N1,fit_MCD1$best)
ind_out_MCD2 <- setdiff(1:N2,fit_MCD2$best)

ind_out_MCD1 == ind_out_MCD2 # same outliers 
data.no.out <- data_regression[-ind_out_MCD1,]
data_regression[ind_out_MCD1,c("Year","Region")]

# write.csv(data.no.out, 'data_no_out.csv', row.names = F)

## models without outliers ##
## MaxDomain (2008:2021)
model.fin.maxdom.no.out.lin <- gam(MaxDomain ~ Immigrations
                               + s(Employment.rate, bs = 'cr')
                               + s(Women.enrolled, bs = 'cr'), data = data.no.out)
summary(model.fin.maxdom.no.out.lin) # R2 = 0.556

# check for normality of residuals
hist(model.fin.maxdom.no.out.lin$residuals)
qqnorm(model.fin.maxdom.no.out.lin$residuals, main = '')
qqline(model.fin.maxdom.no.out.lin$residuals, col = color_pal(2)[2], lwd = 2)


## Max (2008:2021)
model.fin.max.no.out.lin <- gam(Max ~ Immigrations
                            + s(Employment.rate, bs = 'cr')
                            + s(Women.enrolled, bs = 'cr'), data = data.no.out)
summary(model.fin.max.no.out.lin) # R2 = 0.56

# check for normality of residuals
hist(model.fin.max.no.out.lin$residuals)
qqnorm(model.fin.max.no.out.lin$residuals, main = '')
qqline(model.fin.max.no.out.lin$residuals, col = color_pal(2)[2], lwd = 2)


## plots MaxDomain ## 
# plot 1
new_data_seq <- seq(min(data.no.out$Employment.rate), max(data.no.out$Employment.rate), length.out = 100)
new_data_seq1 <- rep(median(data.no.out$Immigrations), length = 100)
new_data_seq2 <- rep(median(data.no.out$Women.enrolled), length = 100)

preds <- predict(model.fin.maxdom.no.out.lin2, newdata = list(Employment.rate = new_data_seq,
                                                              Immigration = new_data_seq1,
                                                              Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out$Employment.rate, data.no.out$MaxDomain, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Employment rate', ylab = 'Max domain')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 2
new_data_seq <- seq(min(data.no.out$Immigrations), max(data.no.out$Immigrations), length.out = 100)
new_data_seq1 <- rep(median(data.no.out$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out$Women.enrolled), length = 100)

preds <- predict(model.fin.maxdom.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                      Immigrations = new_data_seq,
                                                      Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out$Immigrations, data.no.out$MaxDomain, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Immigrations', ylab = 'Max domain')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 3
new_data_seq <- seq(min(data.no.out$Women.enrolled), max(data.no.out$Women.enrolled), length.out = 100)
new_data_seq1 <- rep(median(data.no.out$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out$Immigrations), length = 100)

preds <- predict(model.fin.maxdom.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                      Immigrations = new_data_seq2,
                                                      Women.enrolled = new_data_seq), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out$Women.enrolled, data.no.out$MaxDomain, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Women enrolled', ylab = 'Max domain')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)


## plots Max ## 
# plot 1
new_data_seq <- seq(min(data.no.out$Employment.rate), max(data.no.out$Employment.rate), length.out = 100)
new_data_seq1 <- rep(median(data.no.out$Immigrations), length = 100)
new_data_seq2 <- rep(median(data.no.out$Women.enrolled), length = 100)

preds <- predict(model.fin.max.no.out.lin, newdata = list(Employment.rate = new_data_seq, 
                                                      Immigrations = new_data_seq1,
                                                      Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out$Employment.rate, data.no.out$Max, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Employment rate', ylab = 'Max')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 2
new_data_seq <- seq(min(data.no.out$Immigrations), max(data.no.out$Immigrations), length.out = 100)
new_data_seq1 <- rep(median(data.no.out$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out$Women.enrolled), length = 100)

preds <- predict(model.fin.max.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                      Immigrations = new_data_seq,
                                                      Women.enrolled = new_data_seq2), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out$Immigrations, data.no.out$Max, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Immigrations', ylab = 'Max')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# plot 3
new_data_seq <- seq(min(data.no.out$Women.enrolled), max(data.no.out$Women.enrolled), length.out = 100)
new_data_seq1 <- rep(median(data.no.out$Employment.rate), length = 100)
new_data_seq2 <- rep(median(data.no.out$Immigrations), length = 100)

preds <- predict(model.fin.max.no.out.lin, newdata = list(Employment.rate = new_data_seq1, 
                                                      Immigrations = new_data_seq2,
                                                      Women.enrolled = new_data_seq), se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(data.no.out$Women.enrolled, data.no.out$Max, xlim = range(new_data_seq), cex = .5, col = color_gray,
     xlab = 'Women enrolled', ylab = 'Max')
lines(new_data_seq, preds$fit, lwd = 2, col = color_pal(2)[2])
matlines(new_data_seq, se.bands, lwd = 1, col = color_pal(2)[2], lty = 3)

# we use these models for conformal prediction
