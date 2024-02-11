library(readr)
color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))
nati <- read_csv("Analisi Introduttiva/Nati totali.csv", 
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

## Dataset with the residents

res <- read_csv("Analisi Introduttiva/Residenti storico.csv", 
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
x11()
matplot(t(nati), type='l', x = years, main = 'Total newborns for province', 
        xlab = 'Year', ylab = 'Birth per person', col=color_pal(dim(nati)[1]))

# Difference overall 2002/2019
temp = nati[,1] - nati[,18]
which.min(temp)

### MEI & MHI 
library(roahd)
library(DepthProc)

data = fData(grid = years, values = nati)

band_depth = BD(Data = nati)
median_curve_manual <- data[which.max(band_depth),]
grid_ecg <- seq(median_curve_manual$t0,median_curve_manual$tP,by=median_curve_manual$h)

plot(data, col=color_gray)
lines(grid_ecg,median_curve_manual$values, lw = 2, col=color_pal(2)[1])

median.mbd =  median_fData(fData = data, type = "MBD")

### PERMUTATION TEST

## year 2019 -> test between nord / centro / sud 
nati_data = data.frame(nati=nati,prov = prov)
geo <- NULL

prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
nati_nord = nati_data[nati_data$prov %in% prov_nord, ]

prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
nati_centro = nati_data[nati_data$prov %in% prov_centro, ]

prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
nati_sud = nati_data[nati_data$prov %in% prov_sud, ]

geo <- ifelse(nati_data$prov %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(nati_data[which(is.na(geo)) ,]$prov %in% prov_sud, 'sud', 'centro')

nati_data <- data.frame(nati_data, geo=geo)

nati_2019 = nati_data[,18:20]
nati_2012 = nati_data[, c(11,19,20)]
rm(list = c('prov_nord', 'prov_sud', 'prov_centro'))

# one-way anova 2019
g <- nlevels(as.factor(nati_2019$geo))
n <- dim(nati_2019)[1]

# H0: the distributions belong to the same population
# H1: (H0)^c

# Parametric test:
fit <- aov(as.numeric(nati_2019$nati.2019) ~ as.factor(nati_2019$geo))
summary(fit)

# Permutation test:
# Test statistic: F stat
T0 <- summary(fit)[[1]][1,4]
T0

plot(as.factor(nati_2019$geo), as.numeric(nati_2019$nati.2019)*1000, 
     xlab='position',col=color_pal(g),main='Original Data - Plot intro A' )

# CMC to estimate the p-value
B <- 1000 # Number of permutations
T_stat <- numeric(B)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  X2019_perm <- as.numeric(nati_2019$nati.2019)[permutation]
  fit_perm <- aov(X2019_perm ~ as.factor(nati_2019$geo))
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=color_gray)
abline(v=T0,lwd=2, col=color_pal(2)[1]) 

## PEMRUTATION PER CURVA DI PVALUE SU TUTTI GLI ANNI
p.value <- NULL
for (i in 1:length(years)){
  fit <- aov(as.numeric(nati_data[,i]) ~ as.factor(geo))
  
  # Permutation test:
  # Test statistic: F stat
  T0 <- summary(fit)[[1]][1,4]
  T0
  
  B <- 1000 
  T_stat <- numeric(B)
  
  for(perm in 1:B){
    permutation <- sample(1:n)
    temp <- nati_data[,i]
    temp_perm <- as.numeric(temp)[permutation]
    fit_perm <- aov(temp_perm ~ as.factor(geo))
    
    T_stat[perm] <- summary(fit_perm)[[1]][1,4]
  }
  
  p.value[i] <- sum(T_stat>=T0)/B
}
p.value <- p.adjust(p.value, method="BH")
plot(years, p.value, type ='l', main = 'Adjusted p-value function', lwd=2, xlab = 'Years', col=color_pal(2)[2])
abline(h=0.05, col ='darkred')

## PERMUTATON TEST ON 2002/2019 WITH GEO AS PERMUTATION

data = fData(grid = years, values = nati_data[,-((ncol(nati_data)-1):ncol(nati_data))])

data_nord <- data[which(geo=='nord') ,]
data_centro <- data[which(geo=='centro') ,]
data_sud <- data[which(geo=='sud') ,]

band_depth_nord = BD(Data = data_nord)
band_depth_centro = BD(Data = data_centro)
band_depth_sud = BD(Data = data_sud)

mn <- as.numeric(data_nord[which.max(band_depth_nord),]$values)
mc <- as.numeric(data_centro[which.max(band_depth_centro),]$values)
ms <- as.numeric(data_sud[which.max(band_depth_sud),]$values)

plot(data, main = 'Plot intro A [7]', xlab = 'Years', col=color_gray)
lines(years, mn, col=color_pal(3)[1], lwd=3)
lines(years, mc, col=color_pal(3)[2], lwd=3)
lines(years, ms, col=color_pal(3)[3], lwd=3)

