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
color_palette <- colorRampPalette(c("skyblue", "darkblue"))(n_colors)

x11()
matplot(eta,t(fac), type='l', col=color_palette)
legend('topright', fill=color_palette, legend=years)


####Proviamo l'approccio functional e vediamo che succede - smoothing####
library(fda)
abscissa <- 1:length(eta)
observations <- t(fac)
##Optimal : m = 6, nbasis = 9

basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=9, norder = 5)
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, observations, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef

x11()
matplot(17:50, Xsp0, type='l', xlab = 'Age', ylab = 'Fertility rate')
#legend('topright', fill=color_palette, legend=years)

####Derivatives####

functionalPar <- fdPar(fdobj=basis)
Xss <- smooth.basis(abscissa, observations, functionalPar)
Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)
matplot(Xss0, type='l', xlab = 'Age')
matplot(Xss1, type='l')
x11()
matplot(17:50, Xss2, type='l', xlab = 'Age')  #Potenzialmente interessante

dataXss1 <- fData(eta, t(Xss1))
plot(dataXss1, type='l')

dataXss2 <- fData(eta, t(Xss2))
plot(dataXss2, type='l', xlab = 'Ages')

####Nonparametric - depth####
library(roahd)
data <- fData(eta, t(Xsp0))
med <- median_fData(data) #anche se non ha molto senso prendere in considerazione la mediana di time series
int <- seq(med$t0, med$tP, by=med$h)
x11()
plot(data, type='l', xlab = 'Ages', ylab = 'Fertility rate')
lines(int, med$values, lwd=6, col='red')

hypo <- MHI(data)
epi <- MEI(data)

plot(years, hypo, type='l')  #Quante linee ci sono sopra a quella in particolare, indice di alteza overall della curva
#Indice di bassezza overall della fertilità
plot(years, epi, type='l')
