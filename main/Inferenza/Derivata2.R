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
library(mgcv)
library(fda)

color_pal <- colorRampPalette(colors = c("orange", "darkred"))
col.3 <- color_pal(3)
colBG <- "grey80"

total_curves <- matrix(0, nrow = length(17:50), ncol=length(prov)*length(years))
basis <- create.bspline.basis(rangeval=c(17,50), nbasis=9, norder=5)
lowercut <- 31
uppercut <- 302
for (i in 1:length(prov))
{
  for (j in 1:length(years))
  {
    Xsp <- smooth.basis(argvals=17:50, y=prov_list[[i]][j ,], fdParobj=basis)
    temp <-  eval.fd(17:50, Xsp$fd, Lfd=2)
    total_curves[, (j-1)*length(prov)+i] <- eval.fd(17:50, Xsp$fd, Lfd=2)
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
total_curves <- total_curves[4:30,]
matplot(total_curves, type = 'l')

library(fdANOVA)
f_data <- fData(20:46, t(total_curves))
x11()
out_mag <- roahd::fbplot(f_data) # no magnitude outliers

x11()
out_shape <- outliergram(f_data)
ID_out <- out_shape$ID_outliers
prov_out <- match(names(total_curves)[num], prov)

plot(f_data[ID_out], col=color_pal(length(ID_out)))

x11()
plot(f_data, col=colBG)
for (i in 1:length(levels(geo)))
{
  m <- colMeans(f_data[which(geo == levels(geo)[i])]$values)
  lines(20:46, m, col=color_pal(length(levels(geo)))[i], lwd=2)
}
legend('topright', fill=color_pal(length(levels(geo))), legend=geo_names)

T0 <- fanova.tests(x = total_curves, geo,  test = "L2N", parallel = TRUE)$L2N$statL2

B <- 1000
T_stat <- numeric(B)
set.seed(2024)
pb=progress_bar$new(total=B)
pb$tick(0)
for(perm in 1:B){
  permutation <- sample(1:length(geo))
  geo_perm <- geo[permutation]
  
  T_stat[perm] <- fanova.tests(x = total_curves, geo_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
  pb$tick()
}

hist(T_stat, xlim=range(c(0,T0)), col=colBG)
abline(v = T0, col = color_pal(2)[1], lwd=5)

# region is significant

#p-value curve# - very long, uncomment if necessary, flat line as result
# abscissa <- grid
# p_val <- rep(0, length(abscissa))
# set.seed(2024)
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
  scale_x_discrete(labels = geo_names) + geom_segment(aes(x = geo, xend = geo, y = 50, yend = norms), color =  color_pal(length(geo_lev)), linetype = "dashed", alpha = 0.7)
print(plot)

years_tot <- rep(years, each=length(prov))
years_tot <- as.factor(years_tot)

x11()
plot(f_data, col=colBG)
for (i in 1:length(years))
{
  m <- median_fData(fData = f_data[which(years_tot==years[i])], type = "MBD")
  lines(20:46, m$values, col=color_pal(length(years))[i], lwd=2)
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

#p-value curve# - ci mette una vita, non runnare
# abscissa <- 20:46
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

# TUTTI 0

p_vals <- rep(0, length(years))
pb=progress_bar$new(total=B*length(years))
pb$tick(0)
set.seed(2024)
for (i in 1:length(years))
{
  curves_year <- t(f_data$values[((length(prov)*(i-1))+1):(length(prov)*i) ,])
  geo_year <- geo[((length(prov)*(i-1)+1):(length(prov)*i))]
  
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

df <- data.frame(years=years, pvalue=p_vals)
plot <- ggplot(df, aes(x = years, y = pvalue)) +
  geom_line(color=color_pal(2)[1]) + 
  geom_point(color=color_pal(2)[1]) + 
  labs(title = "P values",
       x = "Years", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_c()  

print(plot)

###Cutting in three years-levels
year_cut <- c("2002-2005","2006-2013","2014-2018")
lev <- list(2002:2005, 2006:2013, 2014:2018)
years_tot <- rep(years, each=length(prov))
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
    curves_temp <- total_curves[, inx]
    years_temp <- years_tot[inx]
    
    T0 <- fanova.tests(x = curves_temp, years_temp,  test = "L2N", parallel = TRUE)$L2N$statL2
    
    T_stat <- numeric(B)
    set.seed(2024)
    for(perm in 1:B){
      permutation <- sample(1:length(years_temp))
      year_perm <- years_temp[permutation]
      
      T_stat[perm] <- fanova.tests(x = curves_temp, year_perm,  test = "L2N", parallel = TRUE)$L2N$statL2
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

#### VENA ARRIVATO QUA ####

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

maxima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
new <- data.frame(x=seq(17,50,length=1000))
for (i in 1:length(years))
{
  for (j in 1:length(prov))
  {
    Xsp <- smooth.basis(argvals=17:50, y=prov_list[[j]][i ,], fdParobj=basis)
    temp <-  eval.fd(new$x, Xsp$fd, Lfd=2)
    maxima[((i-1)*length(prov)+j),1] <- prov[j]
    maxima[((i-1)*length(prov)+j),2] <- years[i]
    maxima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.max(temp)])
    maxima[((i-1)*length(prov)+j),4] <- as.numeric(max(temp))
  }
}
maxima <- data.frame(maxima)
names(maxima) <- c("Province", "Year", "MaxDomain", "Max")
library(robustbase)
years_tot_cut <- years_tot_cut[-which(maxima$MaxDomain==19 | maxima$MaxDomain==25)]
geo <- geo[-which(maxima$MaxDomain==19 | maxima$MaxDomain==25)]
maxima <- maxima[-which(maxima$MaxDomain==19 | maxima$MaxDomain==25),]

data_max <- data.frame(x=as.numeric(maxima$MaxDomain),y=as.numeric(maxima$Max))

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
geo_names <- c("North", "Centre", "South")

x11()
plot(data_max, col=colBG, main="Linear regression for the three different region")
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

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30, col=color_gray)
abline(v=T0,lwd=5, col=color_pal(2)[1])

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

# Between the class of years, are the difference significant??
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
# as constant in the group of years

# Now we want to quantify the differences on the three coefficients of the linear model connecting
# the value of the maximum with its abscissa.
# We use a robust method, knowing that there are outliers present

CI <- matrix(0, nrow=9, ncol=3)
alpha <- 0.05
pb=progress_bar$new(total=B*9)
pb$tick(0)
for (i in 1:length(geo_names))
{
  for (k in 1:length(year_cut))
  {
    #In ogni iterazione mi calcolo il confidence interval del lts
    data_iter <- data_max[which(geo==levels(geo)[i] & years_tot_cut==year_cut[k]) ,]
    model <- lm(y ~ x, data=data_iter)
    point_estimate <- model$coefficients[2][[1]]
    fitted <- model$fitted.values
    res <- model$resid
    boot <- rep(0, B)
    set.seed(2024)
    for (j in 1:B)
    {
      res_boot <- sample(res, replace = T)
      data_boot <- data.frame(x=data_iter$x, y=fitted+res_boot)
      model <- lm(y ~ x, data=data_boot)
      boot[j] <- model$coefficients[2][[1]]
      pb$tick()
    }
    right <- quantile(boot, 1 - alpha/2)
    left <- quantile(boot, alpha/2)
    inx <- ((as.numeric(levels(geo))[i]-1)*length(year_cut)) + k
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
  ord = 1:9,
  y = paste(nam[,1], nam[,2]),
  x = c(CI[1, 2], CI[2, 2], CI[3, 2],CI[4,2], CI[5,2], CI[6,2],CI[7,2],CI[8,2],CI[9,2]),
  xmin = c(CI[1, 1] , CI[2, 1] , CI[3, 1] ,CI[4,1], CI[5,1], CI[6,1],CI[7,1],CI[8,1],CI[9,1]),
  xmax = c(CI[1, 3]  , CI[2, 3]  , CI[3, 3]  ,CI[4,3], CI[5,3], CI[6,3],CI[7,3],CI[8,3],CI[9,3])
)
ggplot(df, aes(x = x, y = ord))+
  geom_point(size=2, col=color_pal(2)[2], group=df$ord) + 
  geom_linerange(aes(xmin = xmin, xmax=xmax, y=ord, group=ord), linewidth=1, col=color_pal(2)[1]) + 
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) + 
  ylab("Region") + 
  labs(title = "Confidence Intervals with Central Points", ylab="Region")

# CI_param <- matrix(0, nrow=3, ncol=3)
# alpha <- 0.05
# pb=progress_bar$new(total=B*3)
# pb$tick(0)
# for (i in 1:length(lev))
# {
#   #In ogni iterazione mi calcolo il confidence interval del lts
#   data_iter <- data[which(geo==lev[i]) ,]
#   model <- with(data_iter, ltsReg(x,y, alpha=.80, mcd=TRUE))
#   point_estimate <- model$coefficients[2][[1]]
#   fitted <- model$fitted.values
#   res <- model$resid
#   boot <- rep(0, B)
#   for (j in 1:B)
#   {
#     res_boot <- sample(res, replace = T)
#     res_boot <- rnorm(length(res), mean(res_boot),sd = sd(res_boot))
#     data_boot <- data.frame(x=data_iter$x, y=fitted+res_boot)
#     model <- with(data_boot, ltsReg(x,y, alpha=.80, mcd=TRUE))
#     boot[j] <- model$coefficients[2][[1]]
#     pb$tick()
#   }
#   right <- quantile(boot, 1 - alpha/2)
#   left <- quantile(boot, alpha/2)
#   
#   CI_param[i ,] <- c(point_estimate - (right - point_estimate),point_estimate, 
#                point_estimate - (left - point_estimate))
# }
# CI_param <- data.frame(CI_param)
# names(CI_param) <- c("left", "estimate", "right")
# rownames(CI_param) <- c("centro", "nord", "sud")
# CI_param

#### In the particular class of years, are there difference? ####

#### Let's now highlights the differences between pre 2005 & 2012-2018 ####

int1 <- 2002:2005
interval1 <- matrix(0, nrow=length(prov)*length(int1), ncol=length(eta))
for (i in 1:length(prov))
  for (j in 1:length(int1))
  {
    interval1[((j-1)*107+i) ,] <- prov_list[[i]][j ,]
  }

int2 <- 2014:2018
interval2 <- matrix(0, nrow=length(prov)*length(int2), ncol=length(eta))
for (i in 1:length(prov))
  for (j in 1:length(int2))
  {
    interval2[((j-1)*107+i) ,] <- prov_list[[i]][(10+j) ,]
  }

matplot(t(interval1), type='l')
matplot(t(interval2), type='l')

df <- bind_rows(data.frame(interval1), data.frame(interval2))
dd <- depthMedian(df, depth_params = list("Tukey"))

plot(dd[1:(length(int1)*length(prov))], dd[(length(int1)*length(prov)):length(dd)])

ddPlot(x = interval1,y = interval2,depth_params = list(method='Tukey'),
       title = "Pre 2006 vs 2012-2018")

##Permutational test usando i tre intervalli di anni

x11()
plot(f_data, col=colBG)
for (i in 1:length(levels(years_tot_cut)))
{
  m <- colMeans(f_data[which(years_tot_cut==year_cut[i])]$values)
  lines(20:46, m, col=color_pal(length(levels(years_tot_cut)))[i], lwd=2)
}
legend("topright", fill=color_pal(length(levels(years_tot_cut))), legend=year_cut)

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
  geom_point(color = colBG) +
  geom_point(data = medians, aes(x = x, y = y, color=rep(he,each=3)),group=rep(geo_names, each=3), 
             color = rep(he, each=3), size = 5) +
  geom_line(data=medians, aes(x=x, y=y, group=geo, color=rep(he, each=3)), linewidth=2) + 
  labs(title = "Maximum dynamics",
       x = "Maximum's abscissa",
       y = "Maximum") +
  scale_color_manual(name = "Dynamics",
                     breaks = he,
                     labels = geo_names,
                     values = he) +
  theme_minimal()
