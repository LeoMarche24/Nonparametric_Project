#Test su curve base
load("Datasets/data")
load("Datasets/env")
library(fda)
library(roahd)

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

####Prova con la media####
#basis <- bs(x = 17:50, degree = 5, df=9)
#try <- total_curves[, 1]
#observations <- prov_list[[1]][1 ,]

#eval <- colMeans(try*t(basis))*9    #Questo dovrebbe essere il codice per rendere davvero NPS il metodo di smoothing

library(fdANOVA)
data <- fData(1:34, t(total_curves))
plot(data)
media <- mean(data)
lines(1:34, media$values, col='black')

data_nord <- data$values[which(geo == 'nord') ,]
data_centro <- data$values[which(geo == 'centro') ,]
data_sud <- data$values[which(geo=='sud') ,]

m_n <- colMeans(data_nord)
m_c <- colMeans(data_centro)
m_s <- colMeans(data_sud)

lines(m_n)
lines(m_c)
lines(m_s)
ran <- sample(1:length(geo), size = 200)
T0 <- fanova.tests(x = total_curves, geo,  test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  
  T_stat[perm] <- fanova.tests(x = total_curves, geo_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
}

hist(T_stat)
abline(v = T0)

####p-value curve#### - ci mette 10 min, tuttavia da di rifiutare l'ipotesi in ogni punto dell'ascissa
abscissa <- 1:34
p_val <- rep(0, length(abscissa))
for (i in 1:length(abscissa))
{
  T0 <- summary(aov(t(as.matrix(total_curves[1 ,])) ~ geo))[[1]][1,4]
  B <- 1000
  for(perm in 1:B){
    permutation <- sample(1:length(geo))
    geo_perm <- geo[permutation]

    T_stat[perm] <- summary(aov(t(as.matrix(total_curves[1 ,])) ~ geo_perm))[[1]][1,4]
  }
  p_val[i] <- sum(T_stat>T0)/length(T_stat)
}

plot(p_val, type='l')
abline(h=0.05)

####fANOVA sugli anni####
anni <- 2002:2021
anni_tot <- rep(anni, 107)
anni_tot <- as.factor(anni_tot)

T0 <- fanova.tests(x = total_curves, anni_tot,  test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
for(perm in 1:B){
  permutation <- sample(1:length(anni_tot))
  anni_perm <- anni_tot[permutation]
  
  T_stat[perm] <- fanova.tests(x = total_curves, anni_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
}

hist(T_stat)
abline(v = T0)

####Differenza in media di funzioni####
prov_nord = c('Terni','Siena','Prato','Pistoia','Pisa','Pesaro e Urbino','Perugia','Massa-Carrara','Macerata','Lucca','Livorno',"L'Aquila",'Grosseto','Firenze','Fermo','Arezzo','Ancona','Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_sud = c('Viterbo','Teramo','Roma','Rieti','Pescara','Latina','Isernia','Frosinone','Chieti','Campobasso','Ascoli Piceno','Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- ifelse(prov %in% prov_nord, 'nord', 'sud')
geo <- rep(geo, each=20)
geo <- as.factor(geo)

data <- fData(1:34, t(total_curves))
plot(data)
media <- mean(data)
lines(1:34, media$values, col='black')

data_nord <- data$values[which(geo == 'nord') ,]
data_sud <- data$values[which(geo=='sud') ,]

m_n <- colMeans(data_nord)
m_s <- colMeans(data_sud)

lines(m_n)
lines(m_s)

T0 <- sum(abs(m_s-m_n))
B <- 1000
T_stat <- numeric(B)
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  
  data_nord <- data$values[which(geo_perm == 'nord') ,]
  data_sud <- data$values[which(geo_perm == 'sud') ,]
  
  m_n <- colMeans(data_nord)
  m_s <- colMeans(data_sud)  
  # Test statistic:
  T_stat[perm] <- sum(abs(m_s-m_n))
}

hist(T_stat)
abline(v = T0)

####p-value curve####
abscissa <- 1:34
p_val <- rep(0, length(abscissa))
for (i in 1:length(abscissa))
{
  T0 <- (abs(m_s[i]-m_n[i]))
  B <- 1000
  for(perm in 1:B){
    permutation <- sample(1:length(geo))
    geo_perm <- geo[permutation]
    
    data_nord <- data$values[which(geo_perm == 'nord') ,]
    data_sud <- data$values[which(geo_perm == 'sud') ,]
    
    m_n <- colMeans(data_nord)
    m_s <- colMeans(data_sud)  
    # Test statistic:
    T_stat[perm] <- (abs(m_s[i]-m_n[i]))
  }
  p_val[i] <- sum(T_stat>T0)/length(T_stat)
}

plot(p_val, type='l')
abline(h=0.05)

####Curva di varianza####
data <- read.csv("Datasets/Fecondita_Eta_Province.csv")

var_mat <- NULL
for (i in 1:length(prov))
{
  temp <- rep(0, length(eta))
  for (j in 1:length(years))
  {
    for (k in 1:length(eta))
    {
      row <- which(province$Territorio==prov[i] & province$Età.della.madre==eta[k])
      temp[k] <- var(province$Value[row])
    }
  }
  var_mat <- rbind(var_mat, temp)
}

matplot(t(var_mat), type='l')

m <- 3           # spline order 
degree <- m-1    # spline degree 
nbasis <- 10
# Create the basis of bspline - base script
basis <- create.bspline.basis(rangeval=c(1,34), nbasis=nbasis, norder=m)    # If breaks are not provided, equally spaced knots are created
basismat <- eval.basis(1:34, basis) # to obtain the first derivative argument Lfdobj=1 to add in the function
est_coef = lsfit(basismat, t(var_mat), intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef
matplot(Xsp0, type='l')

prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- ifelse(prov %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(prov[which(is.na(geo))] %in% prov_sud, 'sud', 'centro')
geo <- as.factor(geo)

data <- fData(1:34, t(Xsp0))
plot(data)
media <- mean(data)
lines(1:34, media$values, col='black')

data_nord <- data[which(prov %in% prov_nord) ,]
data_centro <- data[which(prov %in% prov_centro) ,]
data_sud <- data[which(prov %in% prov_sud) ,]

m_n <- mean(data_nord)
m_c <- mean(data_centro)
m_s <- mean(data_sud)

plot(data)
lines(1:34, m_n$values, col='black')
lines(1:34, m_c$values, col='black')
lines(1:34, m_s$values, col='black')

T0 <- sum(abs(m_n$values-m_c$values) + abs(m_c$values-m_s$values) + abs(m_s$values-m_n$values))
B <- 1000
T_stat <- numeric(B)
#Ci mette troppo non farlo
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  
  data_nord <- data[which(geo_perm == 'nord') ,]
  data_centro <- data[which(geo_perm == 'centro') ,]
  data_sud <- data[which(geo_perm == 'sud') ,]
  
  m_n <- mean(data_nord)
  m_c <- mean(data_centro)
  m_s <- mean(data_sud)  
  # Test statistic:
  T_stat[perm] <- sum(abs(m_n$values-m_c$values) + abs(m_c$values-m_s$values) + abs(m_s$values-m_n$values))
}

hist(T_stat)
abline(v = T0)
