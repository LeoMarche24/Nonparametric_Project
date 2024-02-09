#Fermo il tempo e guardo 107 linee che rappresentano le età al parto
library(progress)
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
pb = progress_bar$new(total=length(prov)*length(eta))
pb$tick(0)
for (i in 1:length(prov))
{
  for (j in 1:length(eta))
  {
    row2017 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2017)
    row2018 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2018)
    row2019 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2019)
    row2020 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2020)
    row2021 <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[j] & province$TIME==2021)
    
    temp <- province$Value[c(row2017, row2018, row2019, row2020, row2021)]
    
    fertility[i, j] <- mean(temp, trim = .4)
    pb$tick()

  }
}
rm(province)

matplot(t(fertility), type='l', main = 'Trimmed mean for provinces', x = 17:50, xlab="Mother's age"
        , ylab="Fertility rate")

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

mei.data= MEI(data)
which.max(mei.data)
which.min(mei.data)
lines(eta, data$values[which.max(mei.data) ,], col="green", lwd = 2)
lines(eta, data$values[which.min(mei.data),], col="blue", lwd = 2)
lines(eta, data$values[tukey.deepest.idx[1],], col="red", lwd = 2)

####Permutation test####
geo <- NULL
prov_nord_ovest = c('Alessandria', 'Asti', 'Bergamo', 'Biella','Brescia', 'Como', 'Cremona', 'Cuneo', 'Genova', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano', 'Monza e della Brianza', 'Novara', 'Pavia', 'Savona', 'Sondrio', 'Torino', 'Varese',  "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli')
prov_nord_est = c('Belluno', 'Bolzano / Bozen', 'Ferrara', 'Forlì-Cesena', 'Gorizia', 'Padova','Pordenone','Rovigo','Trento','Venezia','Treviso','Verona','Vicenza','Trieste', 'Udine')
prov_centro_ovest = c('Arezzo',  'Bologna',  'Chieti',  'Firenze', 'Frosinone', 'Grosseto', 'Latina','Livorno','Lucca', 'Massa-Carrara', 'Modena','Parma', 'Perugia','Piacenza','Pisa', 'Pistoia', 'Prato','Ravenna',"Reggio nell'Emilia",'Rieti', 'Rimini','Roma', 'Siena', 'Viterbo')
prov_centro_est = c('Ancona', 'Ascoli Piceno','Campobasso','Fermo', 'Isernia', "L'Aquila",'Macerata','Pesaro e Urbino', 'Pescara', 'Teramo', 'Terni')
prov_sud = c( 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi',  'Caserta', 'Catanzaro', 'Cosenza', 'Foggia', 'Lecce', 'Matera', 'Napoli', 'Potenza', 'Reggio di Calabria', 'Salerno', 'Taranto', 'Vibo Valentia')
prov_isole = c('Catania', 'Agrigento','Cagliari', 'Caltanissetta', 'Crotone', 'Enna', 'Messina', 'Nuoro', 'Oristano', 'Palermo', 'Ragusa', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Trapani')

geo <- rep(0, 107)
lab <- list(prov_nord_est, prov_nord_ovest, prov_centro_est, prov_centro_ovest, prov_sud, prov_isole)
nomi <- c("prov_nord_est", "prov_nord_ovest", "prov_centro_est", "prov_centro_ovest", "prov_sud", "prov_isole")
for (i in 1:length(prov))
{
  for (j in 1:length(lab))
  {
    if (prov[i] %in% lab[[j]])
      geo[i] <- nomi[j]
  }
}
fertility$geo = geo

data = fData(grid = eta, values = fertility[,-ncol(fertility)])

x11()
plot(data, col='black')
for (i in 1:length(lab))
{
  med <- median_fData(fData = data[which(geo==nomi[i])], type = "MBD")
  lines(17:50, med$values, col=rainbow(length(lab))[i], lwd=2)
}
legend(x = 'topright',fill=rainbow(length(lab)), legend = nomi)

## l2 norm for medians
norms <- NULL
for (i in 1:length(lab))
{
  med <- median_fData(fData = data[which(geo==nomi[i])], type = "MBD")
  norm_i <- norm(med$values, type = '2')
  norms[i] <- norm_i
}
norms_stand <- data.frame(geo = c("prov_nord_est", "prov_nord_ovest", "prov_centro_est", "prov_centro_ovest", "prov_sud", "prov_isole"), 
                          norms = norms/max(data$values)*34)

plot <- ggplot(norms_stand, aes(x = geo, y = norms)) +
  geom_point(size = 4, col = rainbow(length(lab))) +
  labs(title = "Standardized norms of medians",
       x = "Geographic position", y = "Norms") +
  scale_color_manual(values = c("prov_nord_est" = "red", "prov_sud" = "blue", "prov_centro_est" = "green", "prov_nord_ovest" = "yellow", "prov_isole" = "purple", "prov_centro_ovest" = "cyan")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot)


library(fdANOVA)
test <- fanova.tests(x=t(fertility[, -ncol(fertility)]), group.label = fertility[, ncol(fertility)], test = "L2N")
T0 <- test$L2N$statL2

B <- 1000 # Number of permutations
T_stat <- numeric(B)
pb=progress_bar$new(total=B)
pb$tick(0)
for(perm in 1:B){
  permutation <- sample(1:length(prov))
  geo_perm <- geo[permutation]
  
  test <- fanova.tests(x=t(fertility[, -ncol(fertility)]), group.label = geo_perm, test = "L2N")
  T_stat[perm] <- test$L2N$statL2
  
  pb$tick()

}

layout(1)
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
#Rifiuto
plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=2)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val

#P-value function
p_val_fun <- rep(0, length(eta))
pb=progress_bar$new(total=length(eta))
pb$tick(0)
for (i in 1:length(eta))
{
  data_univ <- fertility[, i]
  geo_test <- as.factor(geo)
  T0 <- summary(aov(data_univ ~ geo_test))[[1]][1,4]
  
  B <- 1000 # Number of permutations
  T_stat <- numeric(B)

  for(perm in 1:B){
    permutation <- sample(1:length(prov))
    geo_perm <- geo_test[permutation]
    
    T_stat[perm] <- summary(aov(data_univ ~ geo_perm))[[1]][1,4]
    
  }

  # p-value
  p_val[i] <- sum(T_stat>=T0)/B

  pb$tick()
}

matplot(p_val_fun, type='l')






