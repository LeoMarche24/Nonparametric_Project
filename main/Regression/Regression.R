# regression on maximum values of data
load("Datasets/data")
load("Datasets/env")

color_gray <- "gray80"
color_pal <- colorRampPalette(colors = c("orange", "darkred"))

# taking the QoI 
maxima <- matrix(NA, ncol = 4, nrow = length(years)*length(prov))
new <- data.frame(x=seq(17,50,length=1000))
for (i in 1:length(years))
{
  for (j in 1:length(prov))
  {
    temp <- data.frame(x=17:50, y=prov_list[[j]][i ,])
    model <- with(temp, smooth.spline(x,y,df = 9))
    grid <- predict(model, new)$y
    maxima[((i-1)*length(prov)+j),1] <- prov[j]
    maxima[((i-1)*length(prov)+j),2] <- years[i]
    maxima[((i-1)*length(prov)+j),3] <- as.numeric(new$x[which.max(grid$x)])
    maxima[((i-1)*length(prov)+j),4] <- as.numeric(max(grid))
  }
}
maxima <- data.frame(maxima)
names(maxima) <- c("Province", "Year", "MaxDomain", "Max")

data_max <- data.frame(x=as.numeric(maxima$MaxDomain),y=as.numeric(maxima$Max))
plot(data_max, col = color_gray)

# create columns for regions in maxima
associa_regione <- function(provincia) {
  regioni <- list(
    Abruzzo = c("L'Aquila", "Chieti", "Pescara", "Teramo"),
    Basilicata = c("Matera", "Potenza"),
    Calabria = c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia"),
    Campania = c("Avellino", "Benevento", "Caserta", "Napoli", "Salerno"),
    Emilia_Romagna = c("Bologna", "Ferrara", "Forlì-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini"),
    Friuli_Venezia_Giulia = c("Gorizia", "Pordenone", "Trieste", "Udine"),
    Lazio = c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo"),
    Liguria = c("Genova", "Imperia", "La Spezia", "Savona"),
    Lombardia = c("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese"),
    Marche = c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino"),
    Molise = c("Campobasso", "Isernia"),
    Piemonte = c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli"),
    Puglia = c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto"),
    Sardegna = c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna"),
    Sicilia = c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani"),
    Toscana = c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa-Carrara", "Pisa", "Pistoia", "Prato", "Siena"),
    Trentino_Alto_Adige = c("Bolzano / Bozen", "Trento"),
    Umbria = c("Perugia", "Terni"),
    Valle_d_Aosta = c("Valle d'Aosta / Vallée d'Aoste"),
    Veneto = c("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")
  )
  
  for(regione in names(regioni)) {
    if(provincia %in% regioni[[regione]]) {
      return(regione)
    }
  }
  return(NA) # Se la provincia non è stata trovata
}

maxima$Region <- sapply(maxima$Province, associa_regione)

# mean of max values per region
maxima$MaxDomain <- as.numeric(maxima$MaxDomain)
maxima$Max <- as.numeric(maxima$Max)
media.regione.max <- aggregate(Max ~ Region + Year, data = maxima, FUN = mean)
media.regione.maxdom <- aggregate(MaxDomain ~ Region + Year, data = maxima, FUN = mean)

# dataset with maximum values for regions 
maxima.region <- merge(media.regione.max, media.regione.maxdom, by = c('Year', 'Region'), all.x = T)

plot(maxima.region$MaxDomain, maxima.region$Max, col = color_gray)

# load covariates 
uni <- read.csv('Regression/Dati_clean/dati_uni.txt', header = T)
pop <- read.csv('Regression/Dati_clean/dati_immigrazioni_emigrazioni.txt', header = T)
occ <- read.csv('Regression/Dati_clean/dati_inattivita_occupazione.txt', header = T)
grav <- read.csv('Regression/Dati_clean/dati_interruzioni_gravidanze.txt', header = T)


model = gam(maxima ~ education + s(income,bs='cr'),data = Prestige)

