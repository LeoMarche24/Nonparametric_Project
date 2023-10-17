library(readr)

nati <- read_csv("Nati totali.csv", 
                 col_types = cols(ITTER107 = col_skip(), 
                                  TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
                                  `Seleziona periodo` = col_skip(), 
                                  `Flag Codes` = col_skip(), Flags = col_skip()))
length(unique(nati$Territorio))
prov <- sort(unique(nati$Territorio))
years <- sort(unique(nati$TIME))[4:21]

birth <- matrix(rep(0, length(years)*length(prov)), nrow = length(prov), ncol = length(years))
for(p in 1:length(prov))
{
  for(y in 1:length(years))
  {
    birth[p,y] <- nati$Value[which(nati$Territorio == prov[p] & nati$TIME == years[y])]
  }
}

row.names(birth) <- prov
colnames(birth) <- years
rm(nati)

matplot(t(birth), type='l')

##Ora creo un altro dataset col numero di residenti

res <- read_csv("Residenti storico.csv", 
                col_types = cols(ITTER107 = col_skip(), TIPO_DATO15 = col_skip(), `Tipo dato` = col_skip(), 
                                 ETA1 = col_skip(), `Classe di età` = col_skip(),SEXISTAT1 = col_skip(), Sesso = col_skip(), 
                                 CITTADINANZA = col_skip(), Cittadinanza = col_skip(), `Seleziona periodo` = col_skip(), 
                                 `Flag Codes` = col_skip(), Flags = col_skip()))

popolazione <- matrix(rep(0, length(years)*length(prov)), nrow = length(prov), ncol = length(years))
for(p in 1:length(prov))
{
  for(y in 1:length(years))
  {
    popolazione[p,y] <- res$Value[which(res$Territorio == prov[p] & res$TIME == years[y])]
  }
}

nati <- birth/popolazione
rm(list = c('birth', 'res', 'popolazione'))
matplot(t(nati), type='l')

###Questo è il nostro dataset con cui iniziare l'analisi.

View(as.matrix(nati[, 1]))
View(as.matrix(nati[, 18]))

#Analisi funzionale (depth measure) sulla fda e magari derivate?
#Altro su indici specifici? 
#->Flourish per grafici fighi
