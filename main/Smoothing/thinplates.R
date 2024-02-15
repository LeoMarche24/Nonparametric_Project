
# Load the necessary libraries
library(dplyr)
library(readr)
library(splines)
library(mgcv)
library(rgl)
library(plot3D)
library(progress)
# Import data 
color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))

province <- read_csv("./Datasets/Fecondita_Eta_province.csv", 
                     col_types = cols(ITTER107 = col_skip(), 
                                      TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
                                      ETA1 = col_skip(), `Seleziona periodo` = col_skip(), 
                                      `Flag Codes` = col_skip(), Flags = col_skip()))


years <- sort(unique(province$TIME))
eta <- unique(province$`Età della madre`)
prov <- sort(unique(province$Territorio))
years <- years[4:23]
eta <- eta[-34]

prov_list <- list()

# We build the dataset as a list of matrices

for (i in 1:length(prov))
{
  temp <- matrix(rep(0, length(eta)*length(years)), nrow=length(years), ncol=length(eta))
  for (j in 1:length(years))
  {
    for (k in 1:length(eta))
    {
      row <- which(province$Territorio==prov[i] & province$`Età della madre`==eta[k] & province$TIME==years[j])
      temp[j,k] <- province$Value[row]
    }
  }
  prov_list[[i]] <- temp
}

# Plot one smoothed surface 
p1 <- as.matrix(prov_list[55][[1]])
x <- 1:20
y <- 17:50
xyf <- expand.grid(x,y)
names(xyf)=c('x','y')
data <- data.frame(val=as.vector(p1), x=xyf$x, y=xyf$y)
model <- mgcv::gam(val ~ s(x, y, bs="tp", m = 2), data=data)

xgrid <- seq(1,20,length.out = 100)
ygrid <- seq(17,50,length.out = 100)
xygrid <- expand.grid(xgrid, ygrid)
names(xygrid)=c('x','y')
pred_tp = predict(model, newdata = data.frame(xygrid))
persp3d(xgrid, ygrid, pred_tp, col = color_pal(2)[1], zlab = "rate",
        xlab = "year" , ylab = "age" )
with(data,points3d(x, y, p1, col = color_gray, size = 5))

# Smooth every province
dom <- expand.grid(1:20,17:50)
surf_prov <- list()
for(p in 1:length(prov_list)){
  p1 <- as.vector(prov_list[[p]])
  data_temp <- data.frame(val=p1, x=xyf$x, y=xyf$y)
  model <- with(data_temp, mgcv::gam(val ~ s(x, y, bs="tp", m = 2)))
  surf_prov[[p]] = predict(model, newdata = data.frame(x = dom[,1], y=dom[, 2]))
}
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

lab <- list(prov_nord,prov_centro, prov_sud)
df <- data.frame()

# Exctract df from the list

for(p in 1:107){
  df[p,1:680] = surf_prov[[p]]
}
# and assign to each smoothed provicnce the correct label
geo <- rep(0,107)
for (i in 1:107)
{
  for (j in 1:length(lab))
  {
    if (prov[i] %in% lab[[j]])
      geo[i] <- geo_names[j]
  }
}

# Now we run the permutational ANOVA
# Takes about 30min, we will provide the output to be loaded

pb=progress::progress_bar$new(total=680)
pb$tick(0)
pval.fun=numeric(length(years)*length(eta))
for(i in 1:680){
  B <- 1000
  T_stat <- numeric(B)
  T0 <- summary(aov(df[,i] ~ geo))[[1]][1,4]
  set.seed(2024)
  for(perm in 1:B){
    permutation <- sample(1:107)
    geo_perm <- geo[permutation]
    test <- aov(df[,i] ~ geo_perm)
    T_stat[perm] <- summary(test)[[1]][1,4]
  }
  pval.fun[i] = sum(T_stat>=T0)/B
  pb$tick()
}

#pval.fun <- read.table("./Datasets/p-val.txt")$x

# Plot the p-value function 
x <- seq(2001,2021,length.out = 20)
y <- seq(17,50,length.out = 34)

# Adjust the p-value using BH correction
pval = p.adjust(pval.fun,"BH")
pval <- matrix(pval, nrow=length(x), ncol=length(y), byrow = F)
# and plot the p-value function
persp3d(x,y, pval, col = color_gray, zlab = "p-value", xlab = "year", ylab = "age", zlim= c(0.001,1))
planes3d(a = 0, b = 0, c = 1, d = -0.1, color = color_pal(2)[2], alpha = .75)

#save(pval, file="Smoothing/pval.csv")
  
pval = p.adjust(pval.fun,"bonferroni")
pval <- matrix(pval, nrow=length(x), ncol=length(y), byrow = T)
# and plot the p-value function
persp3d(x,y, pval, col = color_gray, zlab = "p-value", xlab = "year", ylab = "age", zlim= c(0.001,1))
planes3d(a = 0, b = 0, c = 1, d = -0.1, color = color_pal(2)[2], alpha = .75)

