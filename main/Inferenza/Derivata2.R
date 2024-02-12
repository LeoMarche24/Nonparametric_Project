#Test su derivate seconde
load("./Datasets/data")
load("./Datasets/env")
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
# Maxima and minima analysis 
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

abscissa <- seq(17,50, length_out = 100)
basis <- create.bspline.basis(range(abscissa), 9, norder=6)     
functionalPar <- fdPar(fdobj=basis)
Xss <- smooth.basis(abscissa, t(observations), functionalPar)
prov_smooth <- list()
for (i in 1:length(prov))
{
  temp <- list()
  observations <- prov_list[[i]]
  Xss <- smooth.basis(abscissa, t(observations), functionalPar)
  Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
  Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
  Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)
  temp[[1]] <- Xss0
  temp[[2]] <- Xss1
  temp[[3]] <- Xss2
  prov_smooth[[i]] <- temp
}
toplot <- prov_smooth[[1]][3][[1]]
matplot(abscissa , toplot, 'l')
which.max(toplot)
toplot[68]


total_curves <- data.frame(total_curves)
names(total_curves) <- rep(prov, 20)
prov_nord = c('Alessandria', 'Asti', 'Belluno', 'Bergamo', 'Biella', 'Bologna', 'Bolzano / Bozen','Brescia', 'Como', 'Cremona', 'Cuneo', 'Ferrara', 'Forlì-Cesena', 'Genova', 'Gorizia', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano','Modena', 'Monza e della Brianza', 'Novara', 'Padova', 'Parma', 'Pavia', 'Piacenza', 'Pordenone', 'Ravenna', "Reggio nell'Emilia", 'Rimini', 'Rovigo', 'Savona', 'Sondrio', 'Torino','Trento', 'Treviso', 'Trieste', 'Udine', 'Varese', 'Venezia', "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli', 'Verona','Vicenza')
prov_centro = c('Ancona', 'Arezzo', 'Ascoli Piceno', 'Campobasso', 'Chieti', 'Fermo', 'Firenze', 'Frosinone', 'Grosseto', 'Isernia', "L'Aquila", 'Latina','Livorno','Lucca', 'Macerata','Massa-Carrara', 'Perugia', 'Pesaro e Urbino', 'Pescara','Pisa', 'Pistoia', 'Prato','Rieti', 'Roma', 'Siena', 'Teramo', 'Terni', 'Viterbo')
prov_sud = c('Agrigento', 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi', 'Cagliari', 'Caltanissetta', 'Caserta', 'Catania', 'Catanzaro', 'Cosenza', 'Crotone', 'Enna', 'Foggia', 'Lecce', 'Matera', 'Messina', 'Napoli', 'Nuoro', 'Oristano', 'Palermo', 'Potenza', 'Ragusa', 'Reggio di Calabria', 'Salerno', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Taranto', 'Trapani', 'Vibo Valentia')
geo <- ifelse(prov %in% prov_nord, 'nord', NA)
geo[which(is.na(geo))] <- ifelse(prov[which(is.na(geo))] %in% prov_sud, 'sud', 'centro')
geo <- rep(geo, 20)
geo <- as.factor(geo)
matplot(total_curves[,1:2], type = 'l')
maxima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
new <- data.frame(x=seq(17,50,length=100))
plot(1:50, grid, ylim=c(-3, 5))
for (i in 1:length(years))
{
  for (j in 1:length(prov))
  {
    temp <- data.frame(x=17:50, y=prov_list[[j]][i ,])
    model <- with(temp, smooth.spline(x,y,df = 9))
    grid <- predict(model, new, deriv= 2)$y[1:50 ,]
    maxima[((i-1)*length(prov)+j),1] <- prov[j]
    maxima[((i-1)*length(prov)+j),2] <- years[i]
    maxima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.max(grid)])
    maxima[((i-1)*length(prov)+j),4] <- as.numeric(max(grid))
    lines(1:50, grid)
  }
}
library(fda)
basis <- create.bspline.basis(rangeval = 17:50, nbasis = 9)
smoothed_fd <- smooth.basis(x, y, basis, derivs = 2)









maxima <- data.frame(maxima)
names(maxima) <- c("Province", "Year", "Argmax", "Max")

data_max <- data.frame(x=as.numeric(maxima$Argmax),y=as.numeric(maxima$Max))

fit_lts <- ltsReg(y~x, data=data_max, alpha=.70, mcd=TRUE)
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

#Test the above thing for the three different regions
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
# We use a robust method, knowing that there are outliers present


CI <- matrix(0, nrow=9, ncol=3)
alpha <- 0.05
pb=progress_bar$new(total=B*9)
geo <- as.factor(geo)
pb$tick(0)
for (i in 1:length(geo_names))
{
  for (k in 1:length(year_cut))
  {
    #In ogni iterazione mi calcolo il confidence interval del lts
    data_iter <- data_max[which(geo==levels(geo)[i] & years_tot_cut==year_cut[k]) ,]
    model <- with(data_iter, ltsReg(x,y, alpha=.70, mcd=TRUE))
    point_estimate <- model$coefficients[2][[1]]
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

ggplot(df, aes(x = x, y = factor(y)))+
  geom_point(size=2, col=color_pal(2)[2]) + 
  geom_linerange(aes(xmin = xmin, xmax=xmax, y=y), linewidth=1, col=color_pal(2)[1]) + 
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) + 
  ylab("Region") + 
  labs(title = "Confidence Intervals with Central Points", ylab="Region")



int1 <- 2002:2005
interval1 <- matrix(0, nrow=length(prov)*length(int1), ncol=length(eta))
for (i in 1:length(prov)){
  for (j in 1:length(int1))
  {
    interval1[((j-1)*107+i) ,] <- prov_list[[i]][j ,]
  }
}
int2 <- 2012:2018
interval2 <- matrix(0, nrow=length(prov)*length(int2), ncol=length(eta))
for (i in 1:length(prov)){
  for (j in 1:length(int2))
  {
    interval2[((j-1)*107+i) ,] <- prov_list[[i]][(10+j) ,]
  }
}

# farlo con le deriv

matplot(t(interval1), type='l')
matplot(t(interval2), type='l')

df <- bind_rows(data.frame(interval1), data.frame(interval2))
dd <- depthMedian(df, depth_params = list("Tukey"))

ddPlot(x = interval1,y = interval2,depth_params = list(method='Tukey'),
       title = "Pre 2006 vs 2012-2018")

##Permutational test usando i tre intervalli di anni


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




# Minima

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

#Test the above thing for the three different regions
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
  model <- with(data_iter, ltsReg(x,y, alpha=.70, mcd=TRUE))
  point_estimate <- model$coefficients[2][[1]]
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

#

int1 <- 2002:2005
interval1 <- matrix(0, nrow=length(prov)*length(int1), ncol=length(eta))
for (i in 1:length(prov)){
  for (j in 1:length(int1))
  {
    interval1[((j-1)*107+i) ,] <- prov_list[[i]][j ,]
  }
}
int2 <- 2012:2018
interval2 <- matrix(0, nrow=length(prov)*length(int2), ncol=length(eta))
for (i in 1:length(prov)){
  for (j in 1:length(int2))
  {
    interval2[((j-1)*107+i) ,] <- prov_list[[i]][(10+j) ,]
  }
}

matplot(t(interval1), type='l')
matplot(t(interval2), type='l')

df <- bind_rows(data.frame(interval1), data.frame(interval2))
dd <- depthMedian(df, depth_params = list("Tukey"))

ddPlot(x = interval1,y = interval2,depth_params = list(method='Tukey'),
       title = "Pre 2006 vs 2012-2018")

##Permutational test usando i tre intervalli di anni


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

# North and center show the same behavior, may be worth to run a test?










