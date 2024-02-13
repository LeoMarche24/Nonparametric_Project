library(conformalInference)
library(progress)

# aggiungere solo data.no.out

Nord = c('Friuli_Venezia_Giulia', 'Liguria', 'Lombardia', 'Piemonte', 'Trentino_Alto_Adige', 'Valle_d_Aosta', 'Veneto', "Friuli-Venezia Giulia", "Trentino Alto Adige / Südtirol", "Valle d'Aosta / Vallée d'Aoste")
Centro = c('Abruzzo','Emilia_Romagna', 'Lazio', 'Marche', 'Molise', 'Toscana', 'Umbria', "Emilia-Romagna")
Sud = c('Basilicata', 'Calabria', 'Campania', 'Puglia', 'Sardegna', 'Sicilia')

colors <- color_pal(3)

# linear effect on Emigrations
model.fin.maxdom.no.out.lin <- gam(MaxDomain ~ Immigrations
                                   + s(Employment.rate, bs = 'cr')
                                   + s(Women.enrolled, bs = 'cr'), 
                                   data = data.no.out[which(data.no.out$Year %in% 2008:2020),])
summary(model.fin.maxdom.no.out.lin)

data <- data.frame(x1=x[,1], x2=x[,2], x3=x[,3], y=y)

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
set.seed(2024)
feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020),c("Immigrations","Employment.rate","Women.enrolled")])
resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020),"MaxDomain"])
new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2021 & data.no.out$Region == "Marche"),c("Immigrations","Employment.rate","Women.enrolled")])

pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.01)
data.frame(lwr=pred$lo,pred=pred$pred,upr=pred$up)

train_ss(feature.matrix,resp.vect)

# plot
Region.vec <- as.vector(unique(data.no.out[which(data.no.out$Year == 2021),]$Region))
n.Reg <- length(Region.vec)
sigle.Reg <- c("ABR","BAS","CAL","CAM","EMR","FVG","LAZ","LIG","LOM","MAR","MOL","PIE","PUG","SAR","SIC","TOS","TAA","UMB","VEN")

plot(x = 0, y = 0, type = "p", pch = 16,xlim = c(0,n.Reg), ylim = c(30, 34.5), ylab = "Conformal prediction", xlab = "Regions")
pb=progress_bar$new(total=n.Reg)
pb$tick(0)

for( i in 1:n.Reg){
  Area <- ifelse(Region.vec[i] %in% Nord, "Nord", ifelse(Region.vec[i] %in% Centro, "Centro", "Sud"))
  feature.matrix <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020 & data.no.out$Area == Area),c("Immigrations","Employment.rate","Women.enrolled")])
  resp.vect <- as.matrix(data.no.out[which(data.no.out$Year %in% 2008:2020 & data.no.out$Area == Area),"MaxDomain"])
  
  new_obs <- as.matrix(data.no.out[which(data.no.out$Year == 2021 & data.no.out$Region == Region.vec[i]),c("Immigrations","Employment.rate","Women.enrolled")])
  real_MD <- data.no.out[which(data.no.out$Year == 2021 & data.no.out$Region == Region.vec[i]),c("MaxDomain")]
  
  pred=conformal.pred(x = feature.matrix, y = resp.vect, x0 = new_obs, train.fun = train_ss, predict.fun = predict_ss, alpha=0.05)
  #data.frame(lwr=pred$lo,pred=pred$pred,upr=pred$up)
  
  # Aggiunta dell'intervallo di confidenza
  color <- ifelse(Region.vec[i] %in% Nord, colors[1], ifelse(Region.vec[i] %in% Centro, colors[2], colors[3]))
  segments(x0 = i, y0 = pred$lo, x1 = i, y1 = pred$up, lwd = 2, col = color)
  points(i,pred$lo, pch='_', cex = 2, col=color, lwd=3)
  points(i,pred$up, pch='_', cex = 2, col=color, lwd=3)
  points(x = i, y = real_MD, pch = 19, col = color)
  text(i, 30, labels = sigle.Reg[i], pos = 3, col = color)
  
  pb$tick()
}








