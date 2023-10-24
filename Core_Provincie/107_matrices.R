setwd("C:/Users/leoma/OneDrive/Documents/PoliMi/Nonparametric Statistics/Progetto_Locale/Core_Provincie")
library(readr)
provincie <- read_csv("Fecondita_Eta_Provincie.csv", 
           col_types = cols(ITTER107 = col_skip(), 
           TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
           ETA1 = col_skip(), `Seleziona periodo` = col_skip(), 
          `Flag Codes` = col_skip(), Flags = col_skip()))

###Le provincie della Sardegna sono state unite dopo il 2013, France con un idea di soluzione
#Nel frattempo, continuare analisi con quello che si ha.
#Provincie unite per creare Sud Sardegna hanno rilevazioni < 2002 --> Appunti Fra

years <- sort(unique(provincie$TIME))
eta <- unique(provincie$`Età della madre`)
prov <- sort(unique(provincie$Territorio))
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
      row <- which(provincie$Territorio==prov[i] & provincie$`Età della madre`==eta[k] & provincie$TIME==years[j])
      temp[j,k] <- provincie$Value[row]
    }
  }
  prov_list[[i]] <- temp
}
