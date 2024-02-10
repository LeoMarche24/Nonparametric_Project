#Test su derivate seconde
load("./Datasets/data")
load("./Datasets/env")
library(splines)
library(roahd)
library(fdANOVA)
library(progress)

color_pal <- colorRampPalette(colors = c("orange", "darkred"))
col.3 <- color_pal(3)
colBG <- "grey80"
####Test overall on the smoothed data####
total_curves <- matrix(0, nrow = length(eta), ncol=length(prov)*length(years))
for (i in 1:length(prov))
{
  for (j in 1:length(years))
  {
    data <- data.frame(x=17:50, y=prov_list[[i]][j ,])
    model <- with(data, smooth.spline(x, y, df=9))
    
    total_curves[, (j-1)*length(prov)+i] <- predict(model, 17:50, deriv = 2)$y
  }
}
total_curves <- data.frame(total_curves)
names(total_curves) <- rep(prov, 20)

prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- ifelse(prov %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(prov[which(is.na(geo))] %in% prov_sud, 'sud', 'centro')
geo <- rep(geo, 20)
geo <- as.factor(geo)

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


#Outlier detection#

library(fdANOVA)
data <- fData(1:34, t(total_curves))
x11()
out_mag <- roahd::fbplot(data) # no magnitude outliers

x11()
out_shape <- outliergram(data)
num <- out_shape$ID_outliers
prov_out <- match(names(total_curves)[num], prov)

View(cbind(prov[prov_out], years[(num-prov_out)/107]))

data_aux <- data[num]
plot(data_aux)
View(data_aux$values)
pre <- which(data_aux$values[, c(10,12)]>0)
prov_out_up <- match(names(total_curves)[num[pre]], prov)
prov_out_down <- match(names(total_curves)[num[-pre]], prov)

View(cbind(names(total_curves)[num[pre]], years[(num[pre]-prov_out_up)/107]))
View(cbind(names(total_curves)[num[-pre]], years[(num[-pre]-prov_out_down)/107]))

data_aux1 <- data[num[pre]]
plot(data_aux1)
data_aux2 <- data[num[-pre]]
plot(data_aux2)

##Prova con il fattore geografico su tre livelli##
x11()
plot(data, col=colBG)

data_nord <- data$values[which(geo == 'nord') ,]
data_centro <- data$values[which(geo == 'centro') ,]
data_sud <- data$values[which(geo=='sud') ,]

m_n <- colMeans(data_nord)
m_c <- colMeans(data_centro)
m_s <- colMeans(data_sud)
lines(m_n, col=col.3[1], lwd=2)
lines(m_c, col=col.3[2], lwd=2)
lines(m_s, col=col.3[3], lwd=2)
legend(x = -4.5,legend=c("North", "Center","South"),
       col=col.3, lty=1, cex=0.8)

T0 <- fanova.tests(x = total_curves, geo,  test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
pb=progress_bar$new(total=B)
pb$tick(0)
set.seed(2024)
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  T_stat[perm] <- fanova.tests(x = total_curves, geo_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
  pb$tick()
}

hist(T_stat, xlim=range(c(0,T0)))
abline(v = T0, col ="red") # rifiuto

#p-value curve# - ci mette 10 min
abscissa <- 1:34
p_val <- rep(0, length(abscissa))
pb=progress_bar$new(total=B*34)
pb$tick(0)
for (i in 1:length(abscissa))
{
  T0 <- summary(aov(t(as.matrix(total_curves[i ,])) ~ geo))[[1]][1,4]
  B <- 1000
  set.seed(2024)
  for(perm in 1:B){
    permutation <- sample(1:length(geo))
    geo_perm <- geo[permutation]
    
    T_stat[perm] <- summary(aov(t(as.matrix(total_curves[i ,])) ~ geo_perm))[[1]][1,4]
    pb$tick()
  }
  p_val[i] <- sum(T_stat>T0)/length(T_stat)
}

plot(p_val, type='l',ylim = c(0,1))
abline(h=0.05, col = "red") 

####differenze negli anni####
anni <- 2002:2021
anni_tot <- rep(anni, each=107)
anni_tot <- as.factor(anni_tot)

T0 <- fanova.tests(x = total_curves, anni_tot, test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
pb=progress_bar$new(total=B)
pb$tick(0)
for(perm in 1:B){
  permutation <- sample(1:length(anni_tot))
  anni_perm <- anni_tot[permutation]
  
  T_stat[perm] <- fanova.tests(x = total_curves, anni_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
  pb$tick()
}

hist(T_stat, xlim = range(c(0,T0)))
abline(v = T0, col = "red")

#p-value curve# - ci mette 10 min
abscissa <- 1:34
p_val <- rep(0, length(abscissa))
pb=progress_bar$new(total=B*34)
pb$tick(0)
for (i in 1:length(abscissa))
{
  T0 <- summary(aov(t(as.matrix(total_curves[i ,])) ~ anni_tot))[[1]][1,4]
  B <- 1000
  for(perm in 1:B){
    permutation <- sample(1:length(geo))
    anni_perm <- anni[permutation]
    
    T_stat[perm] <- summary(aov(t(as.matrix(total_curves[i ,])) ~ anni_perm))[[1]][1,4]
    pb$tick()
  }
  p_val[i] <- sum(T_stat>T0)/length(T_stat)
}

p_values <- read.table("pvalues_derivata2.txt", header = T)
plot(p_values[,1], type='l',ylim = c(0,1)) # 
abline(h=0.05, col = 'red')

p_val1 <- p.adjust(p_val, method = "bonferroni")
plot(p_values[,2], type='l', ylab = 'p.bonf', xlab = 'Age',,ylim = c(0,1)) 
abline(h=0.05, col = 'red')

# p_values <- data.frame(pvalue = p_val, pvalue_adj = p_val1)
# write.table(p_values, "pvalues_derivata2.txt", row.names = FALSE)

trim_total <- matrix(nrow = 20, ncol = 34)
for (i in 1:length(years)) {
  tm <- apply(total_curves[,(107*i - 107 + 1):(107*i)], MARGIN = 1, FUN = function(x){mean(x, trim = .4)} )
  trim_total[i,] <- tm
}

data1 <- fData(1:34, trim_total)
plot(data1)


# MANOVA test

Prov_fact <- factor(maxima$Province) # ******************************************************************************
Year_fact <- factor(maxima$Year) # ******************************************************************************
x <- data.frame(MaxDomain=as.numeric(maxima$MaxDomain), Max=as.numeric(maxima$Max))
fit <- manova( as.matrix(x) ~ Prov_fact + Year_fact ) 
summary.manova(fit, test="Wilks")






