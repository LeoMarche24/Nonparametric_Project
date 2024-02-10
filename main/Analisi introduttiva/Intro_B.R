# INTRO B
color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))
#### Italian population ####

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

x11()
matplot(eta,t(fac), type='l', col=color_pal(dim(fac)[1]))

####Nonparametric - depth####
library(roahd)
data <- fData(eta, fac)
med <- median_fData(data) #anche se non ha molto senso prendere in considerazione la mediana di time series
int <- seq(med$t0, med$tP, by=med$h)

hypo <- MHI(data)
epi <- MEI(data)

plot(years, hypo, type='l')  #Quante linee ci sono sopra a quella in particolare, indice di alteza overall della curva
#Indice di bassezza overall della fertilità
plot(years, epi, type='l', col=color_pal(2)[1], lwd=3)
