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

T0 <- sum(abs(m_n-m_c) + abs(m_c-m_s) + abs(m_s-m_n))
B <- 1000
T_stat <- numeric(B)
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  
  data_nord <- data$values[which(geo_perm == 'nord') ,]
  data_centro <- data$values[which(geo_perm == 'centro') ,]
  data_sud <- data$values[which(geo_perm == 'sud') ,]
  
  m_n <- colMeans(data_nord)
  m_c <- colMeans(data_centro)
  m_s <- colMeans(data_sud)  
  # Test statistic:
  T_stat[perm] <- sum(abs(m_n-m_c) + abs(m_c-m_s) + abs(m_s-m_n))
}

hist(T_stat)
abline(v = T0)

####p-value curve####
abscissa <- 1:34
p_val <- rep(0, length(abscissa))
for (i in 1:length(abscissa))
{
  T0 <- abs(m_n[i]-m_c[i]) + abs(m_c[i]-m_s[i]) + abs(m_s[i]-m_n[i])
  B <- 1000
  for(perm in 1:B){
    permutation <- sample(1:length(geo))
    geo_perm <- geo[permutation]
    
    data_nord <- data$values[which(geo_perm == 'nord') ,]
    data_centro <- data$values[which(geo_perm == 'centro') ,]
    data_sud <- data$values[which(geo_perm == 'sud') ,]
    
    m_n <- colMeans(data_nord)
    m_c <- colMeans(data_centro)
    m_s <- colMeans(data_sud)  
    # Test statistic:
    T_stat[perm] <- abs(m_n[i]-m_c[i]) + abs(m_c[i]-m_s[i]) + abs(m_s[i]-m_n[i])
  }
  p_val[i] <- sum(T_stat>T0)/length(T_stat)
}

plot(p_val, type='l')
abline(h=0.05)

####Prova con la media####
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

####Functional ANOVA####
prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- ifelse(prov %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(prov[which(is.na(geo))] %in% prov_sud, 'sud', 'centro')
geo <- rep(geo, each=20)
geo <- as.factor(geo)

data <- fData(1:34, t(total_curves))

m <- colMeans(data$values)
data_nord <- data$values[which(geo == 'nord') ,]
nn <- length(which(geo == 'nord'))
data_centro <- data$values[which(geo == 'centro') ,]
nc <- length(which(geo == 'centro'))
data_sud <- data$values[which(geo=='sud') ,]
ns <- length(which(geo=='sud'))
n <- nn+nc+ns

m_n <- colMeans(data_nord)
m_c <- colMeans(data_centro)
m_s <- colMeans(data_sud)

fun <- function(dat, vec)
{
  res <- rep(0, length(eta))
  for(i in 1:(dim(dat)[1]))
  {
    res <- res + abs(dat[i ,]- vec)
  }
  res
}

T0 <- sum(((nn*abs(m_n-m) + ns*abs(m_s-m) + nc*abs(m_c-m))/(3-1)) / 
            (fun(data_sud, m_s) +  fun(data_centro, m_s) + fun(data_nord, m_n))/(n-3))
  
B <- 1000
T_stat <- numeric(B)
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  
  data_nord <- data$values[which(geo_perm == 'nord') ,]
  data_centro <- data$values[which(geo_perm == 'centro') ,]
  data_sud <- data$values[which(geo_perm == 'sud') ,]
  
  m_n <- colMeans(data_nord)
  m_c <- colMeans(data_centro)
  m_s <- colMeans(data_sud)  
  # Test statistic:
  T_stat[perm] <- sum(((nn*abs(m_n-m) + ns*abs(m_s-m) + nc*abs(m_c-m))/(3-1)) / 
                        (fun(data_sud, m_s) +  fun(data_centro, m_s) + fun(data_nord, m_n))/(n-3))
}
hist(T_stat)
abline(v = T0)

####fANOVA sugli anni####
anni <- 2002:2021
anni_tot <- rep(anni, 107)
anni_tot <- as.factor(anni_tot)

library(fdANOVA)
test <- fanova.tests(total_curves, group.label = anni_tot, test = "L2N", parallel = TRUE)

n <- rep(107, 20)
means <- NULL
for(i in anni)
{
  means <- rbind(means, rowMeans(total_curves[, which(i==anni_tot)]))
}

den <- NULL
for (i in 1:length(anni))
{
  den <- den + sum(fun(total_curves[, which(anni[i]==anni_tot)], means[i ,]))
}

T0 <- (sum(n*abs(t(means)-m))/(length(anni)-1)) / den

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
