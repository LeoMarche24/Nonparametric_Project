#Test su derivate prime
load("Datasets/data")
load("Datasets/env")
library(splines)
library(roahd)
library(fdANOVA)
library(progress)
library(tidyr)
library(tidyverse)
library(robustbase)

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
    
    total_curves[, (j-1)*length(prov)+i] <- predict(model, 17:50, deriv = 1)$y
  }
}
total_curves <- data.frame(total_curves)
names(total_curves) <- rep(prov, 20)

prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- case_when(
  prov %in% prov_nord ~ "1",
  prov %in% prov_centro ~ "2",
  prov %in% prov_sud ~ "3",
  TRUE ~ NA_character_
)
geo <- rep(geo, length(years))
geo <- factor(geo)
geo_names <- c("North", "Centre", "South")

# trimm_total <- matrix(nrow = 107, ncol = 20)
# 
# # creiamo una matrice trimmed mean su tutti gli anni
# for (i in 1:107) {
#   # applicare alla colonna la trimm mean
#   tm <- apply(total_curves[,(20*i+1 -20): (20*i)], MARGIN = 2, FUN = function(x){mean(x, trim = .4)} )
#   # inserire in un nuovo dataset il vettore ottenuto
#   trimm_total[i,] <- tm
# }

# Visual comparison between FD derivative and smoothed derivative 
# -> much better the smoothed one

Xobs0 <- prov_list[[55]][1,]
NT <- 34
abscissa <-17:50
rappinc <- Xobs0[2:NT]-Xobs0[1:(NT-1)]
x11()
plot(abscissa,total_curves[[1]], ylim=range(rappinc), type = 'l', col=color_pal(2)[1], lwd=3,
     xlab = "Age", ylab="Value", main="Computation of the first derivative")
lines(17:49, rappinc, col=color_pal(2)[2], lwd=3)
legend('bottomright', fill=color_pal(2), legend=c("Smoothed curves", "Finite differences"))

#Outlier detection#

library(fdANOVA)
f_data <- fData(17:50, t(total_curves))
x11()
out_mag <- roahd::fbplot(f_data) # no magnitude outliers

x11()
out_shape <- outliergram(f_data)
num <- out_shape$ID_outliers
prov_out <- match(names(total_curves)[num], prov)

View(cbind(prov[prov_out], years[(num-prov_out)/107]))

data_aux <- f_data[num]
plot(data_aux, col = color_pal(1))
pre <- which(data_aux$values[, 33-17]>(-5))
prov_out_up <- match(names(total_curves)[num[pre]], prov)
prov_out_down <- match(names(total_curves)[num[-pre]], prov)

View(cbind(names(total_curves)[num[pre]], years[(num[pre]-prov_out_up)/107]))
View(cbind(names(total_curves)[num[-pre]], years[(num[-pre]-prov_out_down)/107]))

data_aux1 <- f_data[num[pre]]
data_aux2 <- f_data[num[-pre]]
plot(data_aux1, col = color_pal(2)[1], ylim=c(-15,15), xlab="Age")
for(i in 1:7){
  lines(17:50, data_aux2$values[i,],col = color_pal(2)[2])
}


##Prova con il fattore geografico su tre livelli##
x11()
plot(f_data, col='grey80', main = 'First derivative')

data_nord <- data$values[which(geo == 'nord') ,]
data_centro <- data$values[which(geo == 'centro') ,]
data_sud <- data$values[which(geo=='sud') ,]

m_n <- colMeans(data_nord)
m_c <- colMeans(data_centro)
m_s <- colMeans(data_sud)

lines(17:50,m_n, col=color_pal(3)[1], lwd=2)
lines(17:50,m_c, col=color_pal(3)[2], lwd=2)
lines(17:50,m_s, col=color_pal(3)[3], lwd=2)

legend(x = 17,y = -10,legend=c("North", "Center","South"),
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
hist(T_stat, xlim=range(c(0,T0)), col = colBG)
abline(v = T0, col = color_pal(1)[1], lwd = 5)

#p-value curve# - ci mette 10 min
abscissa <- 17:50
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

# Procediamo con il calcolo dei p-value adjusted

plot(p_val, type='l')
abline(h=0.05)


# Benjamini-Hockberg corrections on pvalues:
p.bh <- p.adjust(p_val, 'BH')
# Indexes of the couples for which BH correction tells us that there is a 
# significant difference at level alpha=5%:
which(p.bh<.05)
plot(p.bh, type='l')
abline(h=0.05)

## l2 norm for medians as a measure for classify the difference ##
norms <- NULL
geo_lev <- levels(geo)
for (i in 1:length(geo_lev))
{
  med <- median_fData(fData = f_data[which(geo==geo_lev[i])], type = "MBD")
  norm_i <- norm(med$values, type = '2')
  norms[i] <- norm_i
}
norms_stand <- data.frame(geo = levels(geo), 
                          norms = norms/max(f_data$values)*34)

plot <- ggplot(norms_stand, aes(x = geo, y = norms)) +
  geom_point(size = 4, col = color_pal(length(geo_lev))) +
  labs(title = "Standardized norms of medians",
       x = "Geographic position", y = "Norms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13)) +
  scale_x_discrete(labels = geo_names) + geom_segment(aes(x = geo, xend = geo, y = 70, yend = norms), color =  color_pal(length(geo_lev)), linetype = "dashed", alpha = 0.7)
print(plot)

####differenze negli anni####
years_tot <- rep(years, each=length(prov))
years_tot <- as.factor(years_tot)

x11()
plot(f_data, col=colBG)
for (i in 1:length(years))
{
  m <- median_fData(fData = f_data[which(years_tot==years[i])], type = "MBD")
  lines(17:50, m$values, col=color_pal(length(years))[i], lwd=2)
}
legend(x = 'topright',fill=color_pal(length(years)), legend = years)

T0 <- fanova.tests(x = total_curves, years_tot,  test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
pb=progress_bar$new(total=B)
pb$tick(0)
set.seed(2024)
for(perm in 1:B){
  permutation <- sample(1:length(years_tot))
  years_perm <- years_tot[permutation]
  
  T_stat[perm] <- fanova.tests(x = total_curves, years_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
  pb$tick()
}

hist(T_stat, xlim=range(c(0, T0)), col=colBG)
abline(v = T0, col=color_pal(2)[1], lwd=5)

#Reject, there is a significant difference

# #p-value curve# - ci mette una vita, non runnare
# abscissa <- 17:50
# p_val <- rep(0, length(abscissa))
# pb=progress_bar$new(total=B*length(abscissa))
# pb$tick(0)
# for (i in 1:length(abscissa))
# {
#   T0 <- summary(aov(t(as.matrix(total_curves[i ,])) ~ years_tot))[[1]][1,4]
#   B <- 1000
#   for(perm in 1:B){
#     permutation <- sample(1:length(years_tot))
#     years_perm <- years_tot[permutation]
# 
#     T_stat[perm] <- summary(aov(t(as.matrix(total_curves[i ,])) ~ years_perm))[[1]][1,4]
#     pb$tick()
#   }
#   p_val[i] <- sum(T_stat>T0)/length(T_stat)
# }
# 
# plot(p_val, type='l')
# abline(h=0.05)

## l2 norm for medians#
years_tot <- rep(years, each=length(prov))
years_tot <- as.factor(years_tot)
norms <- NULL
for (i in 1:length(levels(years_tot)))
{
  med <- median_fData(fData = f_data[which(years_tot==levels(years_tot)[i])], type = "MBD")
  norm_i <- norm(med$values, type = '2')
  norms[i] <- norm_i
}
norms_stand <- data.frame(year = levels(years_tot), 
                          norms = norms/max(f_data$values)*34)

plot <- ggplot(norms_stand, aes(x = years, y = norms)) +
  geom_line(color=color_pal(2)[2], linewidth=.5) + 
  geom_point(color=color_pal(2)[2], size=2.5) + 
  labs(title = "Standardized norms of medians",
       x = "Years", y = "Norms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_c()  # You can choose a different color scale if needed

print(plot)
# Maxima and minima analysis 

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
    grid <- predict(model, new, deriv= 1)$y
    maxima[((i-1)*length(prov)+j),1] <- prov[j]
    maxima[((i-1)*length(prov)+j),2] <- years[i]
    maxima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.max(grid$x)])
    maxima[((i-1)*length(prov)+j),4] <- as.numeric(max(grid))
  }
}
maxima <- data.frame(maxima)
names(maxima) <- c("Province", "Year", "Argmax", "Max")

data_max <- data.frame(x=as.numeric(maxima$Argmax),y=as.numeric(maxima$Max))

fit_lts <- ltsReg(y~x, data=data_max, alpha=.75, mcd=TRUE)
fit <- lm(y~x, data=data_max)
with(data_max, plot(x,y, col=ifelse(1:dim(maxima)[1] %in% fit_lts$best,'gray50', 'gray90')))
abline(fit_lts, col=color_pal(2)[1], lwd=2)
abline(fit, col=color_pal(2)[2], lwd=2)

fit_lts

prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- case_when(
  prov %in% prov_nord ~ "1",
  prov %in% prov_centro ~ "2",
  prov %in% prov_sud ~ "3",
  TRUE ~ NA_character_
)
geo <- rep(geo, length(years))
geo <- factor(geo)
geo_names <- c("Nord", "Centro", "Sud")
x11()
plot(data_max, col=colBG, main="Linear regression for the three different region", 
     xlab="Max argoument in the domain", ylab="Maximum value")
for (i in 1:length(geo_names))
{
  fit <- lm(y~x, data=data_max[which(geo==levels(geo)[i]) ,])
  abline(fit, col=color_pal(length(levels(geo)))[i], lwd=3)
}
legend('topright', fill=color_pal(length(levels(geo))), legend=geo_names)


library(DepthProc)

ggplot(data_max, aes(x = x, y = y)) +
  geom_point(color = colBG, size = 1, shape = 16) +
  geom_point(data = aggregate(cbind(x, y) ~ geo, data = data_max, FUN = median), 
             aes(x = x, y = y, color = color_pal(length(geo_names))), size = 6, shape = 16) +
  scale_color_manual(name = "Legend Title",
                     breaks = color_pal(length(geo_names)),
                     labels = geo_names,
                     values = color_pal(length(geo_names))) +
  theme_minimal() +
  labs(title = "Scatter Plot with Depth Medians")

fit <- manova(as.matrix(data_max) ~ geo)
T0 <- summary(fit)[[4]][1,3]
T_stat <- numeric(B) 
n <- dim(data_max)[1]
set.seed(2024)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  geo_perm <- geo[permutation]
  fit_perm <- manova(as.matrix(data_max) ~ geo_perm)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[4]][1,3]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=colBG)
abline(v=T0,lwd=5, col=color_pal(2)[1])

year_cut <- c("2002-2005","2006-2013","2014-2018")
lev <- list(2002:2005, 2006:2013, 2014:2018)
years_tot <- rep(years, each=length(prov))
fit <- manova(as.matrix(data_max) ~ years_tot)
T0 <- summary(fit)[[4]][1,3]
T_stat <- numeric(B) 
n <- dim(data_max)[1]
set.seed(2024)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  years_perm <- years_tot[permutation]
  fit_perm <- manova(as.matrix(data_max) ~ years_perm)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[4]][1,3]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=colBG)
abline(v=T0,lwd=5, col=color_pal(2)[1])

# Test the above thing for the three different regions #
res <- matrix(0, length(levels(geo)))
for (i in 1:length(levels(geo)))
{
  inx <- which(geo==levels(geo)[i])
  temp <- data_max[inx ,]
  years_temp <- years_tot[inx]
  fit <- manova(as.matrix(temp) ~ years_temp)
  T0 <- summary(fit)[[4]][1,3]
  T_stat <- numeric(B) 
  set.seed(2024)
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:length(inx))
    years_perm <- years_temp[permutation]
    fit_perm <- manova(as.matrix(temp) ~ years_perm)
    
    # Test statistic:
    T_stat[perm] <- summary(fit_perm)[[4]][1,3]
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=colBG)
  abline(v=T0,lwd=5, col=color_pal(2)[1])
  res[i] <- sum(T_stat>T0)/B
}

res
# Reject, namely the distribution of the max is significantly different along the year even fixing
# the region

# Test if within the groups the distribution of the max do not depend on the year
years_tot_cut <- ifelse(years_tot < 2006, year_cut[1], ifelse(years_tot > 2013, year_cut[3], year_cut[2]))
years_tot_cut <- as.factor(years_tot_cut)
res <- matrix(0, nrow=length(levels(years_tot_cut)), ncol = length(levels(years_tot_cut)))
pb=progress_bar$new(total=B*3*3)
pb$tick(0)
for(i in 1:length(levels(geo)))
{
  for (j in 1:length(lev))
  {
    years_temp <- which(years_tot %in% lev[[j]])
    geo_temp <- which(geo==levels(geo)[i])
    inx <- intersect(years_temp, geo_temp)
    temp <- data_max[inx ,]
    years_temp <- years_tot[inx]
    
    T0 <- summary.aov(manova(as.matrix(temp) ~ years_temp))[[2]][1,4]
    
    T_stat <- numeric(B)
    set.seed(2024)
    for(perm in 1:B){
      permutation <- sample(1:length(years_temp))
      years_perm <- years_temp[permutation]
      
      T_stat[perm] <-summary.aov(manova(as.matrix(temp) ~ years_perm))[[2]][1,4]
      pb$tick()
      
    }
    hist(T_stat, xlim = range(c(T_stat, T0)))
    abline(v=T0)
    res[i,j] <- (sum(T_stat>T0)/B)
  }
}
res <- data.frame(res)
names(res) <- year_cut
row.names(res) <- geo_names
res

# Almost all accepted, in particular in the south, namely the statistics of the max can be taken
# as constant in the group of years, but not at North

# Now we want to quantify the differences on the three coefficients of the linear model connecting
# the value of the maximum with its abscissa.

CI <- matrix(0, nrow=9, ncol=3)
alpha <- 0.05
geo <- as.factor(geo)
pb=progress_bar$new(total=B*3*3)
pb$tick(0)
for (i in 1:length(geo_names))
{
  for (k in 1:length(year_cut))
  {
    #In ogni iterazione mi calcolo il confidence interval del lts
    data_iter <- data_max[which(geo==levels(geo)[i] & years_tot_cut==year_cut[k]) ,]
    model <- lm(y ~ x, data=data_iter)
    point_estimate <- model$coefficients[2][[1]]  #################
    fitted <- model$fitted.values
    res <- model$resid
    boot <- rep(0, B)
    set.seed(2024)
    for (j in 1:B)
    {
      res_boot <- sample(res, replace = T)
      data_boot <- data.frame(x=data_iter$x, y=fitted+res_boot)
      model <- with(data_boot, ltsReg(x,y, alpha=.70, mcd=TRUE))
      boot[j] <- model$coefficients[2][[1]]
      pb$tick()
    }
    right <- quantile(boot, 1 - alpha/2)
    left <- quantile(boot, alpha/2)
    inx <- k + (as.numeric(levels(geo))[i]-1)*length(year_cut)
    CI[inx ,] <- c(point_estimate - (right - point_estimate),point_estimate,
                   point_estimate - (left - point_estimate))
  }
}
g <- as.factor(geo_names)
y <- as.factor(year_cut)
nam <- expand.grid(y,g)
CI <- data.frame(CI)
names(CI) <- c("left", "estimate", "right")
rownames(CI) <- paste(nam[,1], nam[,2])
CI
df <- data.frame(
  y = paste(nam[,1], nam[,2]),
  x = c(CI[1, 2], CI[2, 2], CI[3, 2],CI[4,2], CI[5,2], CI[6,2],CI[7,2],CI[8,2],CI[9,2]),
  xmin = c(CI[1, 1] , CI[2, 1] , CI[3, 1] ,CI[4,1], CI[5,1], CI[6,1],CI[7,1],CI[8,1],CI[9,1]),
  xmax = c(CI[1, 3]  , CI[2, 3]  , CI[3, 3]  ,CI[4,3], CI[5,3], CI[6,3],CI[7,3],CI[8,3],CI[9,3])
)
ggplot(df, aes(x = x, y = factor(y)))+
  geom_point(size=2, col=color_pal(2)[2]) + 
  geom_linerange(aes(xmin = xmin, xmax=xmax, y=y), linewidth=1, col=color_pal(2)[1]) + 
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) + 
  ylab("Region") + 
  labs(title = "Confidence Intervals with Central Points", ylab="Region")

## Test the differences along the hree class of years ##

T0 <- fanova.tests(x = total_curves, years_tot_cut,  test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
pb=progress_bar$new(total=B)
pb$tick(0)
set.seed(2024)
for(perm in 1:B){
  permutation <- sample(1:length(years_tot_cut))
  year_perm <- years_tot_cut[permutation]
  
  T_stat[perm] <- fanova.tests(x = total_curves, year_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
  pb$tick()
  
}

hist(T_stat, xlim=range(c(0,T0)), col=colBG)
abline(v = T0, col=color_pal(2)[1], lwd=5)

fac <- geo:as.factor(years_tot)
medians <- aggregate(as.matrix(data_max) ~ as.factor(years_tot_cut) + geo, data = data.frame(data_max), FUN = median)

# Plot the scatter plot of all points in grey
he <- color_pal(3)
ggplot(data_max, aes(x = x, y = y)) +
  geom_point(color = "grey") +
  geom_point(data = medians, aes(x = x, y = y, color=rep(he,each=3)),group=rep(geo_names, each=3), 
             color = rep(he, each=3), size = 5) +
  geom_line(data=medians, aes(x=x, y=y, group=geo, color=rep(he, each=3)), linewidth=2) + 
  labs(title = "Maximum dynamics",
       x = "Maximum's abscissa",
       y = "Maximum") +
  scale_color_manual(name = "Legend Title",
                     breaks = he,
                     labels = geo_names,
                     values = he) +
  theme_minimal()

#### Minima ####

minima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
new <- data.frame(x=seq(17,50,length=1000))
for (i in 1:length(years))
{
  for (j in 1:length(prov))
  {
    temp <- data.frame(x=17:50, y=prov_list[[j]][i ,])
    model <- with(temp, smooth.spline(x,y,df = 9))
    grid <- predict(model, new,deriv = 1)$y
    minima[((i-1)*length(prov)+j),1] <- prov[j]
    minima[((i-1)*length(prov)+j),2] <- years[i]
    minima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.min(grid$x)])
    minima[((i-1)*length(prov)+j),4] <- as.numeric(min(grid))
  }
}

minima <- data.frame(minima)
names(minima) <- c("Province", "Year", "Argmin", "Min")

data_max <- data.frame(x=as.numeric(minima$Argmin),y=as.numeric(minima$Min))

fit_lts <- ltsReg(y~x, data=data_max, alpha=.75, mcd=TRUE)
fit <- lm(y~x, data=data_max)
with(data_max, plot(x,y, col=ifelse(1:dim(maxima)[1] %in% fit_lts$best,'gray50', 'gray90')))
abline(fit_lts, col=color_pal(2)[1], lwd=2)
abline(fit, col=color_pal(2)[2], lwd=2)

fit_lts

prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- case_when(
  prov %in% prov_nord ~ "1",
  prov %in% prov_centro ~ "2",
  prov %in% prov_sud ~ "3",
  TRUE ~ NA_character_
)
geo <- rep(geo, length(years))
geo <- factor(geo)
geo_names <- c("Nord", "Centro", "Sud")
x11()
plot(data_max, col=colBG, main="Linear regression for the three different region", 
     xlab="Max argoument in the domain", ylab="Maximum value")
for (i in 1:length(geo_names))
{
  fit <- lm(y~x, data=data_max[which(geo==levels(geo)[i]) ,])
  abline(fit, col=color_pal(length(levels(geo)))[i], lwd=3)
}
legend('topright', fill=color_pal(length(levels(geo))), legend=geo_names)

ggplot(data_max, aes(x = x, y = y)) +
  geom_point(color = colBG, size = 1, shape = 16) +
  geom_point(data = aggregate(cbind(x, y) ~ geo, data = data_max, FUN = median), 
             aes(x = x, y = y, color = color_pal(length(geo_names))), size = 6, shape = 16) +
  scale_color_manual(name = "Legend Title",
                     breaks = color_pal(length(geo_names)),
                     labels = geo_names,
                     values = color_pal(length(geo_names))) +
  theme_minimal() +
  labs(title = "Scatter Plot with Depth Medians")

# Da qua si vede che la x è uguale per le 3 zone
# => fare anova invece che manova solo su x

fit <- manova(as.matrix(data_max) ~ geo)
T0 <- summary(fit)[[4]][1,3]
T_stat <- numeric(B) 
n <- dim(data_max)[1]
set.seed(2024)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  geo_perm <- geo[permutation]
  fit_perm <- manova(as.matrix(data_max) ~ geo_perm)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[4]][1,3]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=colBG)
abline(v=T0,lwd=5, col=color_pal(2)[1])


year_cut <- c("2002-2005","2006-2013","2014-2018")
lev <- list(2002:2005, 2006:2013, 2014:2018)
years_tot <- rep(years, each=length(prov))
fit <- manova(as.matrix(data_max) ~ years_tot)
T0 <- summary(fit)[[4]][1,3]
T_stat <- numeric(B) 
n <- dim(data_max)[1]
set.seed(2024)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  years_perm <- years_tot[permutation]
  fit_perm <- manova(as.matrix(data_max) ~ years_perm)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[4]][1,3]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=colBG)
abline(v=T0,lwd=5, col=color_pal(2)[1])

# Test the above thing for the three different regions
res <- matrix(0, length(levels(geo)))
for (i in 1:length(levels(geo)))
{
  inx <- which(geo==levels(geo)[i])
  temp <- data_max[inx ,]
  years_temp <- years_tot[inx]
  fit <- manova(as.matrix(temp) ~ years_temp)
  T0 <- summary(fit)[[4]][1,3]
  T_stat <- numeric(B) 
  set.seed(2024)
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:length(inx))
    years_perm <- years_temp[permutation]
    fit_perm <- manova(as.matrix(temp) ~ years_perm)
    
    # Test statistic:
    T_stat[perm] <- summary(fit_perm)[[4]][1,3]
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=colBG)
  abline(v=T0,lwd=5, col=color_pal(2)[1])
  res[i] <- sum(T_stat>T0)/B
}

res
# Reject, namely the distribution of the max is significantly different along the year even fixing
# the region

# Test if within the groups the distribution of the max do not depend on the year
years_tot_cut <- ifelse(years_tot < 2006, year_cut[1], ifelse(years_tot > 2011, year_cut[3], year_cut[2]))
years_tot_cut <- as.factor(years_tot_cut)
res <- matrix(0, nrow=length(levels(years_tot_cut)), ncol = length(levels(years_tot_cut)))
pb=progress_bar$new(total=B*3*3)
pb$tick(0)
for(i in 1:length(levels(geo)))
{
  for (j in 1:length(lev))
  {
    years_temp <- which(years_tot %in% lev[[j]])
    geo_temp <- which(geo==levels(geo)[i])
    inx <- intersect(years_temp, geo_temp)
    temp <- data_max[inx ,]
    years_temp <- years_tot[inx]
    
    T0 <- summary.aov(manova(as.matrix(temp) ~ years_temp))[[2]][1,4]
    
    T_stat <- numeric(B)
    set.seed(2024)
    for(perm in 1:B){
      permutation <- sample(1:length(years_temp))
      years_perm <- years_temp[permutation]
      
      T_stat[perm] <-summary.aov(manova(as.matrix(temp) ~ years_perm))[[2]][1,4]
      pb$tick()
      
    }
    hist(T_stat, xlim = range(c(T_stat, T0)))
    abline(v=T0)
    res[i,j] <- (sum(T_stat>T0)/B)
  }
}
res <- data.frame(res)
names(res) <- year_cut
row.names(res) <- geo_names
res

# Almost all accepted, in particular in the south, namely the statistics of the max can be taken
# as constant in the group of years, but not at North

# Now we want to quantify the differences on the three coefficients of the linear model connecting
# the value of the maximum with its abscissa.
# We use a robust method, knowing that there are outliers present

CI <- matrix(0, nrow=3, ncol=3)
alpha <- 0.05
pb=progress_bar$new(total=B*length(geo_names))
pb$tick(0)
for (i in 1:length(geo_names))
{
  #In ogni iterazione mi calcolo il confidence interval del lts
  data_iter <- data_max[which(geo==levels(geo)[i]) ,]
  model <- lm(y ~ x, data=data_iter)
  point_estimate <- model$coefficients[2][[1]] #########
  fitted <- model$fitted.values
  res <- model$resid
  boot <- rep(0, B)
  set.seed(2024)
  for (j in 1:B)
  {
    res_boot <- sample(res, replace = T)
    data_boot <- data.frame(x=data_iter$x, y=fitted+res_boot)
    model <- with(data_boot, ltsReg(x,y, alpha=.70, mcd=TRUE))
    boot[j] <- model$coefficients[2][[1]]
    pb$tick()
  }
  right <- quantile(boot, 1 - alpha/2)
  left <- quantile(boot, alpha/2)
  
  CI[as.numeric(levels(geo)[i]) ,] <- c(point_estimate - (right - point_estimate),point_estimate, 
                                        point_estimate - (left - point_estimate))
}
CI <- data.frame(CI)
names(CI) <- c("left", "estimate", "right")
rownames(CI) <- c("nord", "centro", "sud")
CI

fac <- geo:as.factor(years_tot)
medians <- aggregate(as.matrix(data_max) ~ as.factor(years_tot_cut) + geo, data = data.frame(data_max), FUN = median)

# Plot the scatter plot of all points in grey
he <- color_pal(3)
ggplot(data_max, aes(x = x, y = y)) +
  geom_point(color = "grey") +
  geom_point(data = medians, aes(x = x, y = y, color=rep(he,each=3)),group=rep(geo_names, each=3), 
             color = rep(he, each=3), size = 5) +
  geom_line(data=medians, aes(x=x, y=y, group=geo, color=rep(he, each=3)), linewidth=2) + 
  labs(title = "Minimum dynamics",
       x = "Minimum's abscissa",
       y = "Minimum") +
  scale_color_manual(name = "Legend Title",
                     breaks = he,
                     labels = geo_names,
                     values = he) +
  theme_minimal()
