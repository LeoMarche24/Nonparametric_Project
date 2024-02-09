#Test su curve base
load("Datasets/data")
load("Datasets/env")
library(splines)
library(roahd)
library(ggplot2)
library(progress)

####Test overall on the smoothed data####
total_curves <- matrix(0, nrow = length(eta), ncol=length(prov)*length(years))
for (i in 1:length(prov))
{
  for (j in 1:length(years))
  {
    data <- data.frame(x=17:50, y=prov_list[[i]][j ,])
    model <- with(data, smooth.spline(x, y, df=9))
    
    total_curves[, (j-1)*length(prov)+i] <- predict(model, 17:50)$y
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

#Outlier detection#

library(fdANOVA)
data <- fData(1:34, t(total_curves))

out_mag <- roahd::fbplot(data)
x11()
out_shape <- outliergram(data)
num <- out_shape$ID_outliers
prov_out <- match(names(total_curves)[num], prov)

View(cbind(prov[prov_out], years[(num-prov_out)/107]))

data_aux <- data[num]
plot(data_aux)
pre <- which(data_aux$values[, 5]>20)
prov_out_up <- match(names(total_curves)[num[pre]], prov)
prov_out_down <- match(names(total_curves)[num[-pre]], prov)

View(cbind(names(total_curves)[num[pre]], years[(num[pre]-prov_out_up)/107]))
View(cbind(names(total_curves)[num[-pre]], years[(num[-pre]-prov_out_down)/107]))

##Prova con il fattore geografico su tre livelli##

plot(data, col='black')

data_nord <- data$values[which(geo == 'nord') ,]
data_centro <- data$values[which(geo == 'centro') ,]
data_sud <- data$values[which(geo=='sud') ,]

m_n <- colMeans(data_nord)
m_c <- colMeans(data_centro)
m_s <- colMeans(data_sud)

lines(m_n, col='blue', lwd=2)
lines(m_c, col='green', lwd=2)
lines(m_s, col='yellow', lwd=2)

T0 <- fanova.tests(x = total_curves, geo,  test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
pb=progress_bar$new(total=B)
pb$tick(0)
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  
  T_stat[perm] <- fanova.tests(x = total_curves, geo_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
  pb$tick()

}

hist(T_stat, xlim=range(c(0,T0)))
abline(v = T0)

# #p-value curve# - ci mette 10 min non runnare, rifiutare sempre
# abscissa <- 1:34
# p_val <- rep(0, length(abscissa))
# pb=progress_bar$new(total=B*34)
# pb$tick(0)
# for (i in 1:length(abscissa))
# {
#   T0 <- summary(aov(t(as.matrix(total_curves[i ,])) ~ geo))[[1]][1,4]
#   B <- 1000
#   for(perm in 1:B){
#     permutation <- sample(1:length(geo))
#     geo_perm <- geo[permutation]
# 
#     T_stat[perm] <- summary(aov(t(as.matrix(total_curves[i ,])) ~ geo_perm))[[1]][1,4]
#     pb$tick()
#   }
#   p_val[i] <- sum(T_stat>T0)/length(T_stat)
# }
# 
# plot(p_val, type='l')
# abline(h=0.05)

## l2 norm for medians#
norms <- NULL
nomi <- levels(geo)
for (i in 1:length(nomi))
{
  med <- median_fData(fData = data[which(geo==nomi[i])], type = "MBD")
  norm_i <- norm(med$values, type = '2')
  norms[i] <- norm_i
}
norms_stand <- data.frame(geo = levels(geo), 
                          norms = norms/max(data$values)*34)

plot <- ggplot(norms_stand, aes(x = geo, y = norms)) +
  geom_point(size = 4, col = rainbow(length(nomi))) +
  labs(title = "Standardized norms of medians",
       x = "Geographic position", y = "Norms") +
  scale_color_manual(values = c("prov nord" = "red", "prov_sud" = "blue", "prov centro" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot)


####differenze negli anni####
anni <- 2002:2021
anni_tot <- rep(anni, each=107)
anni_tot <- as.factor(anni_tot)

x11()
plot(data, col='black')
for (i in 1:length(anni))
{
  med <- median_fData(fData = data[which(anni_tot==anni[i])], type = "MBD")
  lines(1:34, med$values, col=rainbow(length(anni))[i], lwd=2)
}
legend(x = 'topright',fill=rainbow(length(anni)), legend = anni)

T0 <- fanova.tests(x = total_curves, anni_tot,  test = "L2N", parallel = TRUE)$L2N$statL2

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

hist(T_stat, xlim=range(c(0, T0)))
abline(v = T0)

# #p-value curve# - ci mette una vita, non runnare
# abscissa <- 1:34
# p_val <- rep(0, length(abscissa))
# pb=progress_bar$new(total=B*34)
# pb$tick(0)
# for (i in 1:length(abscissa))
# {
#   T0 <- summary(aov(t(as.matrix(total_curves[i ,])) ~ anni_tot))[[1]][1,4]
#   B <- 1000
#   for(perm in 1:B){
#     permutation <- sample(1:length(geo))
#     anni_perm <- anni_tot[permutation]
#     
#     T_stat[perm] <- summary(aov(t(as.matrix(total_curves[i ,])) ~ anni_perm))[[1]][1,4]
#     pb$tick()
#   }
#   p_val[i] <- sum(T_stat>T0)/length(T_stat)
# }
# 
# plot(p_val, type='l')
# abline(h=0.05)

## l2 norm for medians#
norms <- NULL
nomi <- levels(anni_tot)
for (i in 1:length(nomi))
{
  med <- median_fData(fData = data[which(anni_tot==nomi[i])], type = "MBD")
  norm_i <- norm(med$values, type = '2')
  norms[i] <- norm_i
}
norms_stand <- data.frame(anni = levels(anni_tot), 
                          norms = norms/max(data$values)*34)

plot <- ggplot(norms_stand, aes(x = anni, y = norms)) +
  geom_line(color='green') + 
  geom_point(color='green') + 
  labs(title = "Standardized norms of medians",
       x = "Years", y = "Norms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_c()  # You can choose a different color scale if needed

print(plot)

##In quali anni c'è una differenza##

p_vals <- rep(0, length(anni))
pb=progress_bar$new(total=B*20)
pb$tick(0)
for (i in 1:length(anni))
{
  curves_year <- t(data$values[((107*(i-1)+1):(107*i)) ,])
  geo_year <- geo[((107*(i-1)+1):(107*i))]
  
  T0 <- fanova.tests(x = curves_year, geo_year,  test = "L2N", parallel = TRUE)$L2N$statL2
  
  T_stat <- numeric(B)
  for(perm in 1:B){
    permutation <- sample(1:length(geo_year))
    geo_perm <- geo_year[permutation]
    
    T_stat[perm] <- fanova.tests(x = curves_year, geo_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
    pb$tick()
    
  }
  
  p_vals[i] <- (sum(T_stat>T0)/B)
}

df <- data.frame(years=anni, pvalue=p_vals)
plot <- ggplot(df, aes(x = years, y = pvalue)) +
  geom_line(color='green') + 
  geom_point(color='green') + 
  labs(title = "P values",
       x = "Years", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_c()  # You can choose a different color scale if needed

print(plot)

###Viceversa, in quali aree geografiche ci sono differenze negli anni
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
geo <- as.factor(rep(geo, 20))

p_vals <- rep(0, length(levels(geo)))
pb=progress_bar$new(total=B*3)
pb$tick(0)
for (i in 1:length(levels(geo)))
{
  curves_geo <- t(data$values[which(geo==levels(geo)[i]) ,])
  year_geo <- anni_tot[which(geo==levels(geo)[i])]
  
  T0 <- fanova.tests(x = curves_geo, year_geo,  test = "L2N", parallel = TRUE)$L2N$statL2
  
  T_stat <- numeric(B)
  for(perm in 1:B){
    permutation <- sample(1:length(year_geo))
    year_perm <- year_geo[permutation]
    
    T_stat[perm] <- fanova.tests(x = curves_geo, year_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
    pb$tick()
    
  }
  
  p_vals[i] <- (sum(T_stat>T0)/B)
}

plot(p_vals)

###Quantity of interest - max of the curves

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
plot(maxima[, 3:4])
library(robustbase)
data <- data.frame(x=as.numeric(maxima$MaxDomain),y=as.numeric(maxima$Max))
fit_lts <- ltsReg(y~x, data=data, alpha=.75, mcd=TRUE)
fit <- lm(y~x, data=data)
with(data, plot(x,y, col=ifelse(1:dim(maxima)[1] %in% fit_lts$best, 'black','red')))
abline(fit_lts, col="darkgreen", lwd=2)
abline(fit, col='blue')

fit_lts
prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- ifelse(prov %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(prov[which(is.na(geo))] %in% prov_sud, 'sud', 'centro')
geo <- rep(geo, 20)
geo <- as.factor(geo)

library(DepthProc)
colori <- c('red', 'orange', 'green')
lev <- levels(geo)
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = 'darkgrey', size = 1, shape = 16) +
  geom_point(data = aggregate(cbind(x, y) ~ geo, data = data, FUN = median), 
             aes(x = x, y = y), color = col, size = 6, shape = 16) +
  scale_color_manual(values = col) +
  theme_minimal() +
  labs(title = "Scatter Plot with Depth Medians", color = "Groups") +
  guides(color = guide_legend(title = "Levels"))

fit <- aov(as.matrix(data) ~ geo)
T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
T_stat <- numeric(B) 
n <- dim(data)[1]
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  geo_perm <- geo[permutation]
  fit_perm <- aov(as.matrix(data) ~ geo_perm)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

#Inferenza tramite confidence interval per i tre coefficienti delle tre regioni
CI <- matrix(0, nrow=3, ncol=3)
alpha <- 0.05
pb=progress_bar$new(total=B*3)
pb$tick(0)
for (i in 1:length(lev))
{
  #In ogni iterazione mi calcolo il confidence interval del lts
  data_iter <- data[which(geo==lev[i]) ,]
  model <- with(data_iter, ltsReg(x,y, alpha=.80, mcd=TRUE))
  point_estimate <- model$coefficients[2][[1]]
  fitted <- model$fitted.values
  res <- model$resid
  boot <- rep(0, B)
  for (j in 1:B)
  {
    res_boot <- sample(res, replace = T)
    data_boot <- data.frame(x=data_iter$x, y=fitted+res_boot)
    model <- with(data_boot, ltsReg(x,y, alpha=.80, mcd=TRUE))
    boot[j] <- model$coefficients[2][[1]]
    pb$tick()
  }
  right <- quantile(boot, 1 - alpha/2)
  left <- quantile(boot, alpha/2)
  
  CI[i ,] <- c(point_estimate - (right - point_estimate),point_estimate, 
               point_estimate - (left - point_estimate))
}
CI <- data.frame(CI)
names(CI) <- c("left", "estimate", "right")
rownames(CI) <- c("centro", "nord", "sud")
CI

CI_param <- matrix(0, nrow=3, ncol=3)
alpha <- 0.05
pb=progress_bar$new(total=B*3)
pb$tick(0)
for (i in 1:length(lev))
{
  #In ogni iterazione mi calcolo il confidence interval del lts
  data_iter <- data[which(geo==lev[i]) ,]
  model <- with(data_iter, ltsReg(x,y, alpha=.80, mcd=TRUE))
  point_estimate <- model$coefficients[2][[1]]
  fitted <- model$fitted.values
  res <- model$resid
  boot <- rep(0, B)
  for (j in 1:B)
  {
    res_boot <- sample(res, replace = T)
    res_boot <- rnorm(length(res), mean(res_boot),sd = sd(res_boot))
    data_boot <- data.frame(x=data_iter$x, y=fitted+res_boot)
    model <- with(data_boot, ltsReg(x,y, alpha=.80, mcd=TRUE))
    boot[j] <- model$coefficients[2][[1]]
    pb$tick()
  }
  right <- quantile(boot, 1 - alpha/2)
  left <- quantile(boot, alpha/2)
  
  CI_param[i ,] <- c(point_estimate - (right - point_estimate),point_estimate, 
               point_estimate - (left - point_estimate))
}
CI_param <- data.frame(CI_param)
names(CI_param) <- c("left", "estimate", "right")
rownames(CI_param) <- c("centro", "nord", "sud")
CI_param

####Distribuzioni diverse tra pre 2005 e post 2012-2018####

int1 <- 2002:2005
interval1 <- matrix(0, nrow=length(prov)*length(int1), ncol=length(eta))
for (i in 1:length(prov))
  for (j in 1:length(int1))
  {
    interval1[((j-1)*107+i) ,] <- prov_list[[i]][j ,]
  }

int2 <- 2012:2018
interval2 <- matrix(0, nrow=length(prov)*length(int2), ncol=length(eta))
for (i in 1:length(prov))
  for (j in 1:length(int2))
  {
    interval2[((j-1)*107+i) ,] <- prov_list[[i]][(10+j) ,]
  }

matplot(t(interval1), type='l')
matplot(t(interval2), type='l')

ddPlot(x = interval1,y = interval2,depth_params = list(method='Tukey'))

##Permutational test usando i tre intervalli di anni




