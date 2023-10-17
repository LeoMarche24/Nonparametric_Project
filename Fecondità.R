####Popolazione italiana####
fecondita <- read_delim("Fecondita-totale-2000-2021.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
years <- 2000:2021
eta <- 17:50
fac <- matrix(rep(0, length(years)*length(eta)), nrow = length(years), ncol = length(eta))
a <- 1
for (j in 1:length(eta))
{
  for (i in 1:length(years))
  {
    fac[i,j] <- fecondita$Value[a]
    a <- a+1
  }
}
colnames(fac) <- eta
row.names(fac) <- years

x11()
matplot(t(fac), type='l', col=1:length(years))

####Proviamo l'approccio functional e vediamo che succede - smoothing####
library(fda)
abscissa <- 1:length(eta)
observations <- t(fac)

orders <- 2:6
gcv <- matrix(rep(0, length(orders)*length(abscissa)), nrow = length(orders), ncol = (length(abscissa)))
row.names(gcv) <- orders
colnames(gcv) <- abscissa
for (m in orders)
{
  grid <- (m+1):(length(abscissa)-1)
  for (i in grid)
  {
    basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=i, norder = m)
    functionalPar <- fdPar(fdobj=basis) 
    for (j in 1:length(years))
    { gcv[m-1, i] <- gcv[m-1, i] + smooth.basis(1:34, observations[, j], functionalPar)$gcv}
  }
}
View(gcv)
min <- min(gcv[-which(gcv==0)])
which(gcv == min, arr.ind=TRUE)

##Optimal : m = 2, nbasis = 10

basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=10, norder = 2)
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, observations, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef
matplot(Xsp0, type='l')

####Derivatives####

functionalPar <- fdPar(fdobj=basis)
Xss <- smooth.basis(abscissa, observations, functionalPar)
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
matplot(Xss0, type='l')
matplot(Xss1, type='l')

####PCA####

library(fields)
fd <- Data2fd(observations, abscissa, basis)
pca_dati <- pca.fd(fd,nharm=5,centerfns=TRUE)
plot(cumsum(pca_dati$values)[1:5]/sum(pca_dati$values),xlab='j',ylab='CPV',ylim=c(0,1))

# plot of the FPCs as perturbation of the mean
media <- mean.fd(fd)
plot(media,lwd=2,ylim=c(-15,95),ylab='fertilità',main='FPC1')
lines(media+pca_dati$harmonics[1,]*sqrt(pca_dati$values[1]), col=3)
lines(media-pca_dati$harmonics[1,]*sqrt(pca_dati$values[1]), col=2)   #+ -> verde / - -> rosso

media <- mean.fd(fd)
plot(media,lwd=2,ylim=c(-15,95),ylab='fertilità',main='FPC2')
lines(media+pca_dati$harmonics[2,]*sqrt(pca_dati$values[2]), col=3)
lines(media-pca_dati$harmonics[2,]*sqrt(pca_dati$values[2]), col=2)   #+ -> verde / - -> rosso

media <- mean.fd(fd)
plot(media,lwd=2,ylim=c(-15,95),ylab='fertilità',main='FPC3')
lines(media+pca_dati$harmonics[3,]*sqrt(pca_dati$values[3]), col=3)
lines(media-pca_dati$harmonics[3,]*sqrt(pca_dati$values[3]), col=2)   #+ -> verde / - -> rosso

plot(pca_dati$scores[, 1], type='l')
plot(pca_dati$scores[, 2], type='l')   #C'è da parlarne per una vita
#La pc1 mi da la fertilità in giovane eta ed era cresciuta per diminuire di nuovo
#La pc2 mi da la fertilità in eta adulta e invece cresce, ma sembra aver plafonato

####Nonparametric - depth####
library(roahd)
data <- fData(eta, t(Xsp0))
med <- median_fData(data)
int <- seq(med$t0, med$tP, by=med$h)
x11()
plot(data, type='l')
lines(int, med$values, lwd=6, col='red')

hypo <- MHI(data)
epi <- MEI(data)

plot(years, hypo, type='l')  #Quante linee ci sono sopra a quella in particolare, indice di alteza overall della curva
#Indice di bassezza overall della fertilità
plot(years, epi, type='l')
