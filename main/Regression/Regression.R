# regression on maximum values of data
load("Datasets/data")
load("Datasets/env")
library(mgcv)


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
  return(NA) # Se la provincia non è stata trovata
}

# create columns for regions in maxima
associa_zona <- function(regione) {
  zone <- list(
    Nord = c('Friuli_Venezia_Giulia', 'Liguria', 'Lombardia', 'Piemonte', 'Trentino_Alto_Adige', 'Valle_d_Aosta', 'Veneto'),
    Centro = c('Emilia_Romagna', 'Lazio', 'Marche', 'Molise', 'Toscana', 'Umbria'),
    Sud = c('Abruzzo', 'Basilicata', 'Calabria', 'Campania', 'Puglia', 'Sardegna', 'Sicilia')
  )
  
  for(zona in names(zone)) {
    if(regione %in% zone[[zona]]) {
      return(zona)
    }
  }
  return(NA) # Se la provincia non è stata trovata
}


maxima$Region <- sapply(maxima$Province, associa_regione)
maxima$Zona <- sapply(maxima$Region, associa_zona)

# mean of max values per region
maxima$MaxDomain <- as.numeric(maxima$MaxDomain)
maxima$Max <- as.numeric(maxima$Max)
media.regione.max <- aggregate(Max ~ Region + Zona + Year, data = maxima, FUN = mean)
media.regione.maxdom <- aggregate(MaxDomain ~ Region + Zona + Year, data = maxima, FUN = mean)

# dataset with maximum values for regions 
maxima.region <- merge(media.regione.max, media.regione.maxdom, by = c('Year', 'Region', 'Zona'), all.x = T)

plot(maxima.region$MaxDomain, maxima.region$Max, col = color_gray)
# plot(maxima.region$MaxDomain, maxima.region$Max, col = factor(maxima.region$Region), pch=19)


# load covariates 
uni <- read.csv('Regression/Dati_clean/dati_uni.txt', header = T)
excluded_categories <- c('Trento', 'Bolzano / Bozen')
uni <- subset(uni, !(Territorio %in% excluded_categories)) # sistemare l'errore in construction_data
colnames(uni)[colnames(uni) == 'TIME'] <- 'Year'
colnames(uni)[colnames(uni) == 'Territorio'] <- 'Region'


pop <- read.csv('Regression/Dati_clean/dati_immigrazioni_emigrazioni.txt', header = T)
occ <- read.csv('Regression/Dati_clean/dati_inattivita_occupazione.txt', header = T)
grav <- read.csv('Regression/Dati_clean/dati_interruzioni_gravidanze.txt', header = T)

# maxima: 2002-2021 x regioni
# uni: 2004- 2020 x regioni
# inizierei considerando solamente le iscrizioni all'università delle donne
maxima.04 <- subset(maxima.region, Year != 2002 & Year != 2003 & Year != 2021)
uni_donna <- uni[which(uni$Sesso=='femmine'),]
uni_uomo <- uni[which(uni$Sesso=='maschi'),]

# vedo due possibili modi di procedere:
# -> fare una regressione dove maxima è cumulativo sulle regioni [20 righe -> un dato per regione]
# -> fare una regressione dove maxima è cumulativo sugli anni [20 righe -> un dato per anno]
# per poi trattare allo stesso modo il dato della covariata

# una prima prova stupida
# -> sommo tutti i dati degli stessi anni
sum_max_by_year <- aggregate(Max ~ Year, data = maxima.region, FUN = sum)
sum_max_by_year <- sum_max_by_year[3:19,]

# -> stesso per uni
sum_rinuncie_by_year <- aggregate(X..rinunce ~ TIME, data = uni_donna, FUN = sum)
colnames(sum_rinuncie_by_year)[colnames(sum_rinuncie_by_year) == 'TIME'] <- 'Year'


ds_reg <- merge(sum_max_by_year, sum_rinuncie_by_year, by=c("Year"), all.x = T) #oppure prima merge e poi sum

model_gam = gam(Max ~ 1 + s(X..rinunce,bs='cr'),data = ds_reg)
summary(model_gam)

attach(ds_reg)
new_data_seq <- seq(min(X..rinunce),
                    max(X..rinunce), length.out = 100)

#                                               change X..rinunce with the name of the abscissa
preds=predict(model_gam,newdata = list(X..rinunce=new_data_seq),se=T) 
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

plot(X..rinunce , Max ,xlim=range(X..rinunce) ,cex =.5, col =" darkgrey " )
lines(new_data_seq,preds$fit ,lwd =2, col =" blue")
matlines(new_data_seq, se.bands ,lwd =1, col =" blue",lty =3)


# Idea: creare 3 modelli, uno per zona

ds_reg <- merge(maxima.region, uni_uomo,  by=c("Year", "Region"), all.x = T)
ds_reg <- merge(ds_reg, uni_donna, by=c("Year", "Region"), all.x = T)
# così non va le covariate devono essere prima aggregate

#split

ds.split <- split(ds_reg, factor(ds_reg$Zona))

sum_centro <- aggregate(cbind(Max, X..rinunce.x, X..rinunce.y) ~ Year, data = ds.split$Centro, FUN = sum)
model_gam = gam(Max ~ X..rinunce.x + s(X..rinunce.y,bs='cr'),data = sum_centro)
summary(model_gam)

help("aggregate")









