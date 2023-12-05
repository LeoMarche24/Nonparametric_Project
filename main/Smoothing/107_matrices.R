library(readr)
province <- read_csv("Datasets/Fecondita_Eta_province.csv", 
           col_types = cols(ITTER107 = col_skip(), 
           TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
           ETA1 = col_skip(), `Seleziona periodo` = col_skip(), 
          `Flag Codes` = col_skip(), Flags = col_skip()))

###Le province della Sardegna sono state unite dopo il 2013, France con un idea di soluzione
#Nel frattempo, continuare analisi con quello che si ha.
#province unite per creare Sud Sardegna hanno rilevazioni < 2002 --> Appunti Fra

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
plot(prov_list[[1]][, 1:2])

####Smoothing####
library(fda)

nbasis <- 6:30
m <- 6
abscissa <- 17:50
observations <- temp
gcv_sum <- 0

for (j in 1:length(prov))
{
  gcv <- numeric(length(nbasis))
  observations <- prov_list[[j]]
  for (i in 1:length(nbasis)){
    basis <- create.bspline.basis(rangeval = range(abscissa), nbasis[i], norder = m)
    gcv[i] <- mean(smooth.basis(abscissa, t(observations), basis)$gcv)
  }
  gcv_sum <- gcv_sum + gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv_sum)
nbasis[which.min(gcv_sum)]
abline(v = nbasis[which.min(gcv_sum)], col = 2)

basis <- create.bspline.basis(range(abscissa), 9, norder=m)     
functionalPar <- fdPar(fdobj=basis)
Xss <- smooth.basis(abscissa, t(observations), functionalPar)
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
matplot(Xss0,type='l')

Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)
matplot(Xss2,type='l')

####Create a dataset with the smoothed functions####
prov_smooth <- list()
for (i in 1:length(prov))
{
  temp <- list()
  Xss <- smooth.basis(abscissa, t(observations), functionalPar)
  Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
  Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
  Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)
  temp[[1]] <- Xss0
  temp[[2]] <- Xss1
  temp[[3]] <- Xss2
  prov_smooth[[i]] <- temp
}

save(prov_smooth, file = "Datasets/data")
save(list = c("years", "eta", "prov"), file="Datasets/env")
