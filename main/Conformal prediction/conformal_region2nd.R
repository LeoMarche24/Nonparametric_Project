library(mgcv)
library(conformalInference)
library(progress)

# importing dataset
data.no.out <- read.csv("Regression/data_no_out2nd.csv")

Nord = c('Friuli_Venezia_Giulia', 'Liguria', 'Lombardia', 'Piemonte', 'Trentino_Alto_Adige', 'Valle_d_Aosta', 'Veneto', "Friuli-Venezia Giulia", "Trentino Alto Adige / Südtirol", "Valle d'Aosta / Vallée d'Aoste")
Centro = c('Abruzzo','Emilia_Romagna', 'Lazio', 'Marche', 'Toscana', 'Umbria', "Emilia-Romagna")
Sud = c('Molise','Basilicata', 'Calabria', 'Campania', 'Puglia', 'Sardegna', 'Sicilia')

Region.vec <- as.vector(unique(data.no.out[which(data.no.out$Year == 2021),]$Region))
n.Reg <- length(Region.vec)
sigle.Reg <- c("ABR","BAS","CAL","CAM","EMR","FVG","LAZ","LIG","LOM","MAR","MOL","PIE","PUG","SAR","SIC","TOS","TRE","UMB","VEN")

color_pal <- colorRampPalette(colors = c("orange", "darkred"))
colors <- color_pal(3)

set.seed(2024)

train_ss=function(x,y, out = NULL){ 
  data <- data.frame(x1=x[,1], x2=x[,2], x3=x[,2], y=y)
  gam(y ~ x1 
      + s(x2, bs = 'cr')
      + s(x3, bs = 'cr'),
      data=data)
}
predict_ss=function(obj, new_x){
  new <- data.frame(x1=new_x[,1], x2=new_x[,2], x3=new_x[,3])
  predict(obj,new)
}


# MAX DOMAIN
model.fin.maxdom.no.out.lin <- gam(MaxDomain ~ Immigrations
                                   + s(Employment.rate, bs = 'cr') 
                                   + s(Women.enrolled, bs = 'cr'), data = data.no.out)
summary(model.fin.maxdom.no.out.lin) # R-sq.(adj) =  0.312


# plot

plot(x = 0, y = 0, type = "p", pch = 16,xlim = c(0,n.Reg), ylim = c(19.5, 25), ylab = "Max Domain - second derivative", xlab = "Regions", xaxt = "n")
pb=progress_bar$new(total=n.Reg)
pb$tick(0)

for( i in 1:n.Reg){
  feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),c("Immigrations","Employment.rate","Women.enrolled")])
  resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),"MaxDomain"])
  
  new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("Immigrations","Employment.rate","Women.enrolled")])
  real_MD <- data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("MaxDomain")]
  
  pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.05)
  
  # adding the CPI plot
  color <- ifelse(Region.vec[i] %in% Nord, colors[1], ifelse(Region.vec[i] %in% Centro, colors[2], colors[3]))
  segments(x0 = i, y0 = pred$lo, x1 = i, y1 = pred$up, lwd = 2, col = color)
  points(i,pred$lo, pch=24, bg = color, cex = 1, col=color, lwd=3)
  points(i,pred$up, pch=25, bg = color, cex = 1, col=color, lwd=3)
  points(x = i, y = real_MD,  pch = 21, bg = color, col = 'white', cex =1.5)
  text(i, 19.5, labels = sigle.Reg[i], pos = 3, col = color)
  
  pb$tick()
}


# MAX 
model.fin.max.no.out.lin <- gam(Max ~ Immigrations
                                + s(Employment.rate, bs = 'cr') 
                                + s(Women.enrolled, bs = 'cr'), data = data.no.out)
summary(model.fin.max.no.out.lin) # R-sq.(adj) =  0.479

# plot

plot(x = -1, y = -1, type = "p", pch = 16,xlim = c(0,n.Reg), ylim = c(-0.25, 2.75), ylab = "Max - second derivative", xlab = "Regions", xaxt = "n")

for( i in 1:n.Reg){
  feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),c("Immigrations","Employment.rate","Women.enrolled")])
  resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),"Max"])
  
  new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("Immigrations","Employment.rate","Women.enrolled")])
  real_MD <- data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("Max")]
  
  pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.05)
  #data.frame(lwr=pred$lo,pred=pred$pred,upr=pred$up)
  
  # adding the CPI plot
  color <- ifelse(Region.vec[i] %in% Nord, colors[1], ifelse(Region.vec[i] %in% Centro, colors[2], colors[3]))
  segments(x0 = i, y0 = pred$lo, x1 = i, y1 = pred$up, lwd = 2, col = color)
  points(i,pred$lo, pch=24, bg = color, cex = 1, col=color, lwd=3)
  points(i,pred$up, pch=25, bg = color, cex = 1, col=color, lwd=3)
  points(x = i, y = real_MD,  pch = 21, bg = color, col = 'white', cex =1.5)
  text(i, -0.25, labels = sigle.Reg[i], pos = 3, col = color)
}



# plot a confronto: predizione max domain per 
# -> 2019 (anno succ. al nostro studio) 
# vs 
# -> 2021 (effetti covid)

plot(x = 0, y = 0, type = "p", pch = 16,xlim = c(0,10*n.Reg), ylim = c(19.5, 26.5), ylab = "Max Domain - second derivative", xlab = "Regions", xaxt = "n")

for( i in 1:n.Reg){
  i1 <- 10*i-9
  i2 <- 10*i-7
  
  # 2020 pred
  feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),c("Immigrations","Employment.rate","Women.enrolled")])
  resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),"MaxDomain"])
  
  new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("Immigrations","Employment.rate","Women.enrolled")])
  real_MD <- data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("MaxDomain")]
  
  pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.05)
  
  # adding the CPI plot
  color <- ifelse(Region.vec[i] %in% Nord, colors[1], ifelse(Region.vec[i] %in% Centro, colors[2], colors[3]))
  segments(x0 = i1, y0 = pred$lo, x1 = i1, y1 = pred$up, lwd = 2, col = color)
  points(i1,pred$lo, pch=24, bg = color, cex = 1, col=color, lwd=3)
  points(i1,pred$up, pch=25, bg = color, cex = 1, col=color, lwd=3)
  points(x = i1, y = real_MD, pch = 21, bg = color, col = 'white', cex =1.5)
  text((i1+i2)/2, 19.5, labels = sigle.Reg[i], pos = 3, col = color)
  
  # 2021 pred
  feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020),c("Immigrations","Employment.rate","Women.enrolled")])
  resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020),"MaxDomain"])
  
  new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2021 & data.no.out$Region == Region.vec[i]),c("Immigrations","Employment.rate","Women.enrolled")])
  real_MD <- data.no.out[which(data.no.out$Year == 2021 & data.no.out$Region == Region.vec[i]),c("MaxDomain")]
  
  pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.05)
  
  # adding the CPI plot
  color <- ifelse(Region.vec[i] %in% Nord, colors[1], ifelse(Region.vec[i] %in% Centro, colors[2], colors[3]))
  segments(x0 = i2, y0 = pred$lo, x1 = i2, y1 = pred$up, lwd = 2, col = color)
  points(i2,pred$lo, pch=24, bg = color, cex = 1, col=color, lwd=3)
  points(i2,pred$up, pch=25, bg = color, cex = 1, col=color, lwd=3)
  points(x = i2, y = real_MD, pch = 21, bg = color, col = 'white', cex =1.5)
  
}


# plot a confronto: predizione max per 
# -> 2019 (anno succ. al nostro studio) 
# vs 
# -> 2021 (effetti covid)

plot(x = -1, y = -1, type = "p", pch = 16,xlim = c(0,10*n.Reg), ylim = c(-0.25, 3.25), ylab = "Max - second derivative", xlab = "Regions", xaxt = "n")

for( i in 1:n.Reg){
  i1 <- 10*i-9
  i2 <- 10*i-7
  
  # 2020 pred
  feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),c("Immigrations","Employment.rate","Women.enrolled")])
  resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2018),"Max"])
  
  new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("Immigrations","Employment.rate","Women.enrolled")])
  real_MD <- data.no.out[which(data.no.out$Year == 2019 & data.no.out$Region == Region.vec[i]),c("Max")]
  
  pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.05)
  
  # adding the CPI plot
  color <- ifelse(Region.vec[i] %in% Nord, colors[1], ifelse(Region.vec[i] %in% Centro, colors[2], colors[3]))
  segments(x0 = i1, y0 = pred$lo, x1 = i1, y1 = pred$up, lwd = 2, col = color)
  points(i1,pred$lo, pch=24, bg = color, cex = 1, col=color, lwd=3)
  points(i1,pred$up, pch=25, bg = color, cex = 1, col=color, lwd=3)
  points(x = i1, y = real_MD, pch = 21, bg = color, col = 'white', cex =1.5)
  text((i1+i2)/2, -0.25, labels = sigle.Reg[i], pos = 3, col = color)
  
  # 2021 pred
  feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020),c("Immigrations","Employment.rate","Women.enrolled")])
  resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020),"Max"])
  
  new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2021 & data.no.out$Region == Region.vec[i]),c("Immigrations","Employment.rate","Women.enrolled")])
  real_MD <- data.no.out[which(data.no.out$Year == 2021 & data.no.out$Region == Region.vec[i]),c("Max")]
  
  pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.05)
  
  # adding the CPI plot
  color <- ifelse(Region.vec[i] %in% Nord, colors[1], ifelse(Region.vec[i] %in% Centro, colors[2], colors[3]))
  segments(x0 = i2, y0 = pred$lo, x1 = i2, y1 = pred$up, lwd = 2, col = color)
  points(i2,pred$lo, pch=24, bg = color, cex = 1, col=color, lwd=3)
  points(i2,pred$up, pch=25, bg = color, cex = 1, col=color, lwd=3)
  points(x = i2, y = real_MD, pch = 21, bg = color, col = 'white', cex =1.5)
  
}


