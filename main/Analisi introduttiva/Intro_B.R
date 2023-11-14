# INTRO B

####Popolazione italiana####
# creazine del dataset nati per 1000 donne dal 2002 al 2019 vs età delle donne

library(readr)
fecondita <- read_delim("Analisi Introduttiva/Fecondita-totale-2000-2021.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)
years <- 2002:2021
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

# plot dei dati
# Define the number of colors in your series
n_colors <- length(years)
# Create a color palette that smoothly transitions from red to blue
color_palette <- colorRampPalette(c("black", "green"))(n_colors)

x11()
matplot(eta,t(fac), type='l', col=color_palette)
legend('topright', fill=color_palette, legend=years)


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
# m = 2 order = 13, prendere ordine 2 non ha senso.
# scegliamo allora la prima base dispari di ordine sufficientemente grande (m = 5) e numero di basi 10

##Optimal : m = 6, nbasis = 10

basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=10, norder = 5)
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, observations, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef

x11()
matplot(Xsp0, type='l', col=color_palette)
legend('topright', fill=color_palette, legend=years)

####Derivatives####

functionalPar <- fdPar(fdobj=basis)
Xss <- smooth.basis(abscissa, observations, functionalPar)
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)
matplot(Xss0, type='l')
matplot(Xss1, type='l')
quartz()
matplot(Xss2, type='l', col=color_palette)  #Potenzialmente interessante
abline(h=0, col = 'red')

####Nonparametric - depth####
library(roahd)
data <- fData(eta, t(Xsp0))
med <- median_fData(data) #anche se non ha molto senso prendere in considerazione la mediana di time series
int <- seq(med$t0, med$tP, by=med$h)
x11()
plot(data, type='l')
lines(int, med$values, lwd=6, col='red')

hypo <- MHI(data)
epi <- MEI(data)

plot(years, hypo, type='l')  #Quante linee ci sono sopra a quella in particolare, indice di alteza overall della curva
#Indice di bassezza overall della fertilità
plot(years, epi, type='l')


















