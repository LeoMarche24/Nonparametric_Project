#Fermo il tempo e guardo 107 linee che rappresentano le età al parto

library(readr)
province <- read_csv("Datasets/Fecondita_Eta_province.csv", 
                      col_types = cols(ITTER107 = col_skip(), 
                                       TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
                                       ETA1 = col_skip(), `Seleziona periodo` = col_skip(), 
                                       `Flag Codes` = col_skip(), Flags = col_skip()))

years <- sort(unique(province$TIME))
eta <- unique(province$`Età della madre`)
prov <- sort(unique(province$Territorio))
years <- years[4:23]
eta <- eta[-34]

fertility <- matrix(rep(0, length(eta)*length(prov)), nrow=length(prov), ncol = length(eta))
for (i in 1:length(prov))
{
  for (j in 1:length(eta))
  {
    row2019 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2019)
    row2020 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2020)
    row2021 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2021)
    
    fertility[i, j] <- (1/6)*province$Value[row2019]+(1/3)*province$Value[row2020]+(1/2)*province$Value[row2021]
  }
}
rm(province)

matplot(t(fertility), type='l', main = 'Plot intro C [1]')

#MEI & MHI
library(roahd)
library(DepthProc)
eta <- 17:50
fertility <- data.frame(fertility)
rownames(fertility) <- prov
colnames(fertility) <- eta

data <- fData(grid = eta, values = fertility)

band_depth <- BD(Data = data)
median_curve_manual <- data[which.max(band_depth),]
grid_ecg <- seq(median_curve_manual$t0,median_curve_manual$tP,by=median_curve_manual$h)

plot(data)
lines(grid_ecg,median_curve_manual$values, lw = 2)

median.mbd =  median_fData(fData = data, type = "MBD")

tukey.depth=depth(u=data$values,method='Tukey')
tukey.deepest.idx = which(tukey.depth==max(tukey.depth))
lines(eta, data$values[tukey.deepest.idx[1],], col="red", lwd = 2)  #Tuckey meglio

plot(data, main = 'Plot intro C [2]')
mei.data= MEI(data)
which.max(mei.data)
which.min(mei.data)
lines(eta, data$values[which.max(mei.data) ,], col="green", lwd = 2)
lines(eta, data$values[which.min(mei.data),], col="blue", lwd = 2)
lines(eta, data$values[tukey.deepest.idx[1],], col="red", lwd = 2)


####Permutation test####
geo <- NULL
prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')

geo <- ifelse(rownames(fertility) %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(row.names(fertility)[which(is.na(geo))] %in% prov_sud, 'sud', 'centro')
fertility <- data.frame(fertility, geo=geo)

data = fData(grid = eta, values = fertility[,-ncol(fertility)])

data_nord <- data[which(geo=='nord') ,]
data_centro <- data[which(geo=='centro') ,]
data_sud <- data[which(geo=='sud') ,]

tuckey_depth_nord = depth(u = data_nord$values, method = 'Tukey')
tuckey_depth_centro = depth(u = data_centro$values, method = 'Tukey')
tuckey_depth_sud = depth(u = data_sud$values, method = 'Tukey')

mn <- as.numeric(data_nord[which.max(tuckey_depth_nord),]$values)
mc <- as.numeric(data_centro[which.max(tuckey_depth_centro),]$values)
ms <- as.numeric(data_sud[which.max(tuckey_depth_sud),]$values)

plot(data, col='black')
lines(eta, mn, col='blue', lwd=5)
lines(eta, mc, col='red', lwd=5)
lines(eta, ms, col='green', lwd=5)

T0 <- sum(abs(mn-mc))+sum(abs(mn-ms))+sum(abs(mc-ms))

B <- 1000 # Number of permutations
T_stat <- numeric(B)
##Ci mette 10, min
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:length(prov))
  geo_perm <- geo[permutation]
  data_nord <- data[which(geo_perm=='nord') ,]
  data_centro <- data[which(geo_perm=='centro') ,]
  data_sud <- data[which(geo_perm=='sud') ,]
  
  tuckey_depth_nord = depth(u = data_nord$values, method = 'Tukey')
  tuckey_depth_centro = depth(u = data_centro$values, method = 'Tukey')
  tuckey_depth_sud = depth(u = data_sud$values, method = 'Tukey')
  
  mn <- as.numeric(data_nord[which.max(tuckey_depth_nord),]$values)
  mc <- as.numeric(data_centro[which.max(tuckey_depth_centro),]$values)
  ms <- as.numeric(data_sud[which.max(tuckey_depth_sud),]$values)
  # Test statistic:
  T_stat[perm] <- sum(abs(mn-mc))+sum(abs(mn-ms))+sum(abs(mc-ms))
}

layout(1)
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
#Rifiuto
plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=2)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val  #p.value è 0.6


####Proviamo l'approccio functional e vediamo che succede - smoothing####
library(fda)
abscissa <- 1:length(eta)
observations <- t(fertility[, -dim(fertility)[2]])

orders <- c(5, 7)
gcv <- matrix(rep(0, length(orders)*length(abscissa)), nrow = length(orders), ncol = (length(abscissa)))
row.names(gcv) <- orders
colnames(gcv) <- abscissa
for (m in 1:length(orders))
{
  grid <- (orders[m]+1):(length(abscissa)-1)
  for (i in grid)
  {
    basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=i, norder = m)
    functionalPar <- fdPar(fdobj=basis) 
    for (j in 1:length(years))
    { gcv[m-1, i] <- gcv[m-1, i] + smooth.basis(1:34, observations[, j], functionalPar)$gcv}
  }
}
View(gcv)
min <- min(gcv[-which(gcv==0)])
which(gcv == min, arr.ind=TRUE)

##Optimal : m = 5, nbasis = 10

basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=10, norder = 5)
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, observations, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef
matplot(Xsp0, type='l')

####Derivatives####

functionalPar <- fdPar(fdobj=basis)
Xss <- smooth.basis(abscissa, observations, functionalPar)
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)
matplot(Xss0, type='l')
matplot(Xss1, type='l')
matplot(Xss2, type='l')

##Permutation test on derivatives

eta <- eta[-c(1, length(eta))]
data <- t(Xss1)[, -c(1,length(eta))]
data = fData(grid = eta, values = data)

med <- data[which.max(depth(u = data$values, method = 'Tukey'))]
plot(data)
lines(eta, med$values)
med <- data[which.max(MBD(data$values))]
plot(data)
lines(eta, med$values)

data_nord <- data[which(geo=='nord') ,]
data_centro <- data[which(geo=='centro') ,]
data_sud <- data[which(geo=='sud') ,]

band_depth_nord = BD(Data = data_nord)
band_depth_centro = BD(Data = data_centro)
band_depth_sud = BD(Data = data_sud)

mn <- as.numeric(data_nord[which.max(band_depth_nord),]$values)
mc <- as.numeric(data_centro[which.max(band_depth_centro),]$values)
ms <- as.numeric(data_sud[which.max(band_depth_sud),]$values)

plot(data, col='black', main = 'Plot intro C [3]')
lines(eta, mn, col='blue', lwd=5)
lines(eta, mc, col='red', lwd=5)
lines(eta, ms, col='green', lwd=5)

T0 <- as.numeric((mn-mc)%*%(mn-mc)+((mn-ms)%*%(mn-ms))+((mc-ms)%*%(mc-ms)))

B <- 1000 # Number of permutations
T_stat <- numeric(B)
##Ci mette 10, min
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:length(prov))
  geo_perm <- geo[permutation]
  data_nord <- data[which(geo_perm=='nord') ,]
  data_centro <- data[which(geo_perm=='centro') ,]
  data_sud <- data[which(geo_perm=='sud') ,]
  
  band_depth_nord = BD(Data = data_nord)
  band_depth_centro = BD(Data = data_centro)
  band_depth_sud = BD(Data = data_sud)
  
  mn <- as.numeric(data_nord[which.max(band_depth_nord),]$values)
  mc <- as.numeric(data_centro[which.max(band_depth_centro),]$values)
  ms <- as.numeric(data_sud[which.max(band_depth_sud),]$values)
  # Test statistic:
  T_stat[perm] <- ((mn-mc)%*%(mn-mc))+((mn-ms)%*%(mn-ms))+((mc-ms)%*%(mc-ms))
}

layout(rbind(2,1))
plot(ecdf(T_stat), main = 'Plot intro C [4]')
abline(v=T0,col=3,lwd=2)
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
#accetto p-val = 0.823


# p-value
p_val <- sum(T_stat>=T0)/B
p_val


#Derivata seconda
eta <- 17:50
data <- t(Xss2)[, -c(1,2,3,length(eta)-2, length(eta)-1, length(eta))]
eta <- 20:47
data = fData(grid = eta, values = data)

layout(c(1,1))
med <- data[which.max(depth(u = data$values, method = 'Tukey'))]
plot(data)
lines(eta, med$values)
med <- data[which.max(MBD(data$values))]
plot(data)
lines(eta, med$values)

data_nord <- data[which(geo=='nord') ,]
data_centro <- data[which(geo=='centro') ,]
data_sud <- data[which(geo=='sud') ,]

band_depth_nord = BD(Data = data_nord)
band_depth_centro = BD(Data = data_centro)
band_depth_sud = BD(Data = data_sud)

mn <- as.numeric(data_nord[which.max(band_depth_nord),]$values)
mc <- as.numeric(data_centro[which.max(band_depth_centro),]$values)
ms <- as.numeric(data_sud[which.max(band_depth_sud),]$values)

plot(data, col='black')
lines(eta, mn, col='blue', lwd=5)
lines(eta, mc, col='red', lwd=5)
lines(eta, ms, col='green', lwd=5)

T0 <- ((mn-mc)%*%(mn-mc))+((mn-ms)%*%(mn-ms))+((mc-ms)%*%(mc-ms))

B <- 1000 # Number of permutations
T_stat <- numeric(B)
##Ci mette 10, min
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:length(prov))
  geo_perm <- geo[permutation]
  data_nord <- data[which(geo_perm=='nord') ,]
  data_centro <- data[which(geo_perm=='centro') ,]
  data_sud <- data[which(geo_perm=='sud') ,]
  
  band_depth_nord = BD(Data = data_nord)
  band_depth_centro = BD(Data = data_centro)
  band_depth_sud = BD(Data = data_sud)
  
  mn <- as.numeric(data_nord[which.max(band_depth_nord),]$values)
  mc <- as.numeric(data_centro[which.max(band_depth_centro),]$values)
  ms <- as.numeric(data_sud[which.max(band_depth_sud),]$values)
  # Test statistic:
  T_stat[perm] <- ((mn-mc)%*%(mn-mc))+((mn-ms)%*%(mn-ms))+((mc-ms)%*%(mc-ms))
}

layout(1)
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=2)

# p-value
p_val <- sum(T_stat>=as.numeric(T0))/B
p_val

