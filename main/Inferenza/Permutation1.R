#Test su curve base
load("data")
load("env")

####Test 1####
total_curves <- NULL
for (i in 1:length(prov))
{
  for (j in 1:length(years))
  {
    total_curves <- cbind(total_curves, prov_smooth[[i]][[1]][, j])
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

values <- 0
for (i in 1:100)
{
  set.seed(534*i)
  tr <- runif(50, 0, length(geo))
  ind <- round(runif(1, 1, 34))

  sum <- summary(aov(t(as.matrix(total_curves))[tr, ind] ~ geo[tr]))
  if (sum[[1]][["Pr(>F)"]][1]<0.05)
    values <- values+1
}
T0 <- values/100

B <- 500
T_stat <- numeric(B)
#Ci mette troppo non farlo
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  values <- 0
  for (i in 1:100)
  {
    set.seed(534*i)
    tr <- runif(50, 0, length(geo))
    ind <- round(runif(1, 1, 34))
    
    sum <- summary(aov(t(as.matrix(total_curves))[tr, ind] ~ geo[tr]))
    if (sum[[1]][["Pr(>F)"]][1]<0.05)
      values <- values+1
  }  
  # Test statistic:
  T_stat[perm] <- values/100
}

layout(rbind(1,2))
plot(ecdf(T_stat), main = 'Plot intro A [5]')
abline(v=T0,col=3,lwd=2)





