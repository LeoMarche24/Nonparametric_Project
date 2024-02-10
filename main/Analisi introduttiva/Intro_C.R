library(progress)
library(readr)
color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))
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
# Create a dataset with the trimmed mean of the last 5 years in order to see the last trend on the 
# fertility rates

matplot(t(fertility), type='l', main = 'Trimmed mean for provinces', x = 17:50, xlab="Mother's age"
        , ylab="Fertility rate", col=color_pal(dim(fertility)[1]))

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

plot(data, col=color_gray)
lines(grid_ecg,median_curve_manual$values, col=color_pal(2)[1], lw = 3)

median.mbd =  median_fData(fData = data, type = "MBD")

tukey.depth=depth(u=data$values,method='Tukey')
tukey.deepest.idx = which(tukey.depth==max(tukey.depth))
lines(eta, data$values[tukey.deepest.idx[1],], col=color_pal(2)[1], lwd = 2)  #Tuckey meglio

mei.data= MEI(data)
which.max(mei.data)
which.min(mei.data)

####Permutation test####
# Creating a factor variable with the macroregion of belonging for each data
prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- case_when(
  prov %in% prov_nord ~ "1",
  prov %in% prov_centro ~ "2",
  prov %in% prov_sud ~ "3",
  TRUE ~ NA_character_
)
geo <- factor(geo)
geo_names <- c("Nord", "Centro", "Sud")

x11()
plot(data, col=color_gray)
for (i in 1:length(levels(geo)))
{
  med <- median_fData(fData = data[which(geo==levels(geo)[i])], type = "MBD")
  lines(17:50, med$values, col=color_pal(length(levels(geo)))[i], lwd=2)
}
legend(x = 'topright',fill=color_pal(length(levels(geo))), legend = geo_names)

## l2 norm for medians
norms <- NULL
for (i in 1:length(levels(geo)))
{
  med <- median_fData(fData = data[which(geo==levels(geo)[i])], type = "MBD")
  norm_i <- norm(med$values, type = '2')
  norms[i] <- norm_i
}
norms_stand <- data.frame(zone = levels(geo),
                          names = geo_names,
                          norms = norms/max(data$values)*34)

plot <- ggplot(data=norms_stand, aes(x = factor(zone), y = norms)) +
  geom_point(size = 4, col = color_pal(length(levels(geo)))) +
  labs(title = "Standardized norms of medians",
       x = "Geographic position", y = "Norms") +
  scale_color_manual(values = color_pal(length(levels(geo)))) +
  scale_x_discrete(breaks = factor(norms_stand$zone), labels = norms_stand$names) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot)

library(fdANOVA)
test <- fanova.tests(x = t(fertility), group.label = geo, test = "L2N")
T0 <- test$L2N$statL2

B <- 1000 # Number of permutations
T_stat <- numeric(B)
pb=progress_bar$new(total=B)
pb$tick(0)
for(perm in 1:B){
  permutation <- sample(1:length(prov))
  geo_perm <- geo[permutation]
  
  test <- fanova.tests(x=t(fertility), group.label = geo_perm, test = "L2N")
  T_stat[perm] <- test$L2N$statL2
  
  pb$tick()

}

layout(1)
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=color_gray)
abline(v=T0,lwd=2, col=color_pal(2)[1])

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

# Reject the hypothesis everywhere, namely the difference in the fertility rates along the 
# nation are significantly different
