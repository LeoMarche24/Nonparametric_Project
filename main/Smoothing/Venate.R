### QUA TI PUOI SBIZZARRIRE

library(dplyr)
library(readr)
province <- read_csv("Datasets/Fecondita_Eta_province.csv", 
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

matplot(17:50, t(prov_list[[which(prov=='Milano')]]), type='l', xlab = 'Age', ylab = 'Fertility rate')

library(gam)
library(splines)
library(mgcv)
library(rgl)
library(plot3D)

p1 <- as.vector(prov_list[[1]])
x <- 1:20
y <- 1:34
xyf <- expand.grid(x,y)
names(xyf)=c('x','y')
data <- data.frame(val=p1, x=xyf$x, y=xyf$y)
model <- with(data, gam(val ~ s(x, y, bs="tp", m = 2)))

# xgrid <- seq(1,20,length.out = 100)
# ygrid <- seq(1,34,length.out = 100)
xgrid <- seq(1,20,length.out = 20)
ygrid <- seq(1,34,length.out = 34)

xygrid <- expand.grid(xgrid, ygrid)

names(xygrid)=c('x','y')

pred_tp = predict(model, newdata = data.frame(xygrid))

# persp3d(xgrid, ygrid, pred_tp, col = 'grey30', forceClipregion = F)
# with(data,
#      points3d(x, y, p1, col = 'black', size = 5))

# Reshape z values to create a matrix
z_matrix <- matrix(pred_tp, nrow = length(xgrid), ncol = length(ygrid), byrow = TRUE)

# Create a 3D surface plot
surface3d(xgrid, ygrid, z_matrix, color = "black", alpha = 0.7, front="line")

# Add labels and customize the plot as needed
rgl.lab("X-axis", "Y-axis", "Z-axis", color="black", cex=1.2)




# persp3d(xgrid, ygrid, surf_prov[[4]], col = 'grey30', zlab = "z")
# persp3d(xgrid, ygrid, surf_prov[[22]], col = 'red', add = T)

# with(data,
#      points3d(x, y, p1, col = 'black', size = 5))
surf_prov <- list()
for(p in 1:length(prov_list)){
  p1 <- as.vector(prov_list[[p]])
  data <- data.frame(val=p1, x=xyf$x, y=xyf$y)
  model <- with(data, gam(val ~ s(x, y, bs="tp", m = 2)))
  surf_prov[[p]] = predict(model, newdata = data.frame(xygrid))
}

prov_nord_ovest = c('Alessandria', 'Asti', 'Bergamo', 'Biella','Brescia', 'Como', 'Cremona', 'Cuneo', 'Genova', 'Imperia', 'La Spezia', 'Lecco', 'Lodi', 'Mantova', 'Milano', 'Monza e della Brianza', 'Novara', 'Pavia', 'Savona', 'Sondrio', 'Torino', 'Varese',  "Valle d'Aosta / Vallée d'Aoste", 'Verbano-Cusio-Ossola', 'Vercelli')
prov_nord_est = c('Belluno', 'Bolzano / Bozen', 'Ferrara', 'Forlì-Cesena', 'Gorizia', 'Padova','Pordenone','Rovigo','Trento','Venezia','Treviso','Verona','Vicenza','Trieste', 'Udine')
prov_centro_ovest = c('Arezzo',  'Bologna',  'Chieti',  'Firenze', 'Frosinone', 'Grosseto', 'Latina','Livorno','Lucca', 'Massa-Carrara', 'Modena','Parma', 'Perugia','Piacenza','Pisa', 'Pistoia', 'Prato','Ravenna',"Reggio nell'Emilia",'Rieti', 'Rimini','Roma', 'Siena', 'Viterbo')
prov_centro_est = c('Ancona', 'Ascoli Piceno','Campobasso','Fermo', 'Isernia', "L'Aquila",'Macerata','Pesaro e Urbino', 'Pescara', 'Teramo', 'Terni')
prov_sud = c( 'Avellino', 'Bari', 'Barletta-Andria-Trani', 'Benevento', 'Brindisi',  'Caserta', 'Catanzaro', 'Cosenza', 'Foggia', 'Lecce', 'Matera', 'Napoli', 'Potenza', 'Reggio di Calabria', 'Salerno', 'Taranto', 'Vibo Valentia')
prov_isole = c('Catania', 'Agrigento','Cagliari', 'Caltanissetta', 'Crotone', 'Enna', 'Messina', 'Nuoro', 'Oristano', 'Palermo', 'Ragusa', 'Sassari', 'Siracusa', 'Sud Sardegna', 'Trapani')

lab <- list(prov_nord_est, prov_nord_ovest, prov_centro_est, prov_centro_ovest, prov_sud, prov_isole)
nomi <- c("prov_nord_est", "prov_nord_ovest", "prov_centro_est", "prov_centro_ovest", "prov_sud", "prov_isole")
prov <- sort(unique(province$Territorio))
df <- data.frame()

for(p in 1:107){
  df[p,1:680] = surf_prov[[p]]
}
geo <- rep(0,107)
for (i in 1:107)
{
  for (j in 1:length(lab))
  {
    if (prov[i] %in% lab[[j]])
      geo[i] <- nomi[j]
  }
}

pb=progress::progress_bar$new(total=680)
pb$tick(0)
pval.fun=numeric(680)
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

x <- seq(2001,2021,length.out = 20)
y <- seq(17,50,length.out = 34)
pval = p.adjust(pval.fun,"BH")

persp3d(x,y, pval, col = 'black', zlab = "p-value", xlab = "year", ylab = "age", zlim= c(0.001,1))
planes3d(a = 0, b = 0, c = 1, d = -0.1, color = "darkorange", alpha = .75)

