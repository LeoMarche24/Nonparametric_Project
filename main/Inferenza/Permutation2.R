###Inferenza su derivate seconde
load("data")
load("env")
total_curves <- NULL
for (i in 1:length(prov))
{
  for (j in 1:length(years))
  {
    total_curves <- cbind(total_curves, prov_smooth[[i]][[3]][, j])
  }
}
total_curves <- data.frame(total_curves)
names(total_curves) <- rep(prov, each=20)
prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- ifelse(prov %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(prov[which(is.na(geo))] %in% prov_sud, 'sud', 'centro')
geo <- rep(geo, each=20)
geo <- as.factor(geo)

####Test 1####
first_max <- function(curve)
{
  return(which.max(curve[3:10]))
}

values <- NULL
k <- 1
for (i in 1:length(prov))
{
  for (j in 1:length(years))
  {
    values <- rbind(values, c(first_max(t(total_curves)[k ,]), geo[k]))
    k <- k+1
  }
}

fit <- aov(as.numeric(values[, 1]) ~ as.factor(values[, 2]))
summary(fit)
T0 <- summary(fit)[[1]][1,4]
T0
B <- 1000 # Number of permutations
T_stat <- numeric(B)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:length(geo))
  values_perm <- as.numeric(values[, 1])[permutation]
  fit_perm <- aov(values_perm ~ as.factor(values[, 2]))
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

plot(ecdf(T_stat), main = 'Plot intro A [5]')
abline(v=T0,col=3,lwd=2)


