## population per regions
res <- read.csv('DS_covariate/residenti.csv', header = T)
res2020.21 <- read.csv('DS_covariate/residenti1.csv', header = T)

res1 <- res[,c("Territorio", "TIME", "Value")]
res2020.21 <- res2020.21[which(res2020.21$TIME == c(2020,2021)), c("Territorio", "TIME", "Value")]

res_tot <- rbind(res1, res2020.21)
rm(res,res1,res2020.21)

names(res_tot)[names(res_tot) == "Territorio"] <- "Region"
names(res_tot)[names(res_tot) == "TIME"] <- "Year"

regioni <- c("Abruzzo", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli-Venezia Giulia", "Lazio", "Liguria", "Lombardia", "Marche", "Molise", "Piemonte", "Puglia", "Sardegna", "Sicilia", "Toscana", "Trentino Alto Adige / Südtirol", "Umbria", "Valle d'Aosta / Vallée d'Aoste", "Veneto")


## university
rinuncia <- read.csv("DS_covariate/Iscritti all'università/rinunci_studi.csv", header = T)
iscritti <- read.csv("DS_covariate/Iscritti all'università/Iscritti_uni_tot.csv", header = T)
rinuncia1 <- rinuncia[which(rinuncia$Sesso == "femmine"), c("Territorio", "TIME", "Value")] # territorio indica la residenza non dove sta l'uni
iscritti1 <- iscritti[which(iscritti$Gruppo.di.corsi.di.laurea == 'totale'), c("Regione.di.residenza", "Sesso", "TIME", "Value")]

unique(iscritti1$TIME)
unique(rinuncia1$TIME)

unique(iscritti1$Regione.di.residenza)
unique(rinuncia1$Territorio)

rinuncia1 <- subset(rinuncia1, (rinuncia1$Territorio %in% regioni))

iscritti1 <- subset(iscritti1, (iscritti1$Regione.di.residenza %in% regioni))
iscritti1 <- iscritti1[which(iscritti1$Sesso != "maschi"), ]

names(iscritti1)[names(iscritti1) == "Regione.di.residenza"] <- "Territorio"
names(iscritti1)[names(iscritti1) == "Value"] <- "Iscritti"
names(rinuncia1)[names(rinuncia1) == "Value"] <- "Rinunce"

iscritti2 <- iscritti1 %>%
  group_by(TIME, Territorio, Sesso) %>%
  summarise(iscritti_tot = sum(Iscritti))

iscritti3 <- matrix(nrow = nrow(iscritti2)/2, ncol = 3)
len <- nrow(iscritti2)/2
for (i in 1:len) {
  iscritti3[i,1] <- iscritti2$TIME[2*i]
  iscritti3[i,2] <- iscritti2$Territorio[2*i]
  iscritti3[i,3] <- as.numeric(iscritti2[2*i-1, 4])/as.numeric(iscritti2[2*i, 4])
}

iscritti.tot <- as.data.frame(iscritti3)
names(iscritti.tot)[names(iscritti.tot) == "V2"] <- "Territorio"
names(iscritti.tot)[names(iscritti.tot) == "V1"] <- "TIME"
names(iscritti.tot)[names(iscritti.tot) == "V3"] <- "Women.enrolled"

universita <- merge(rinuncia1, iscritti.tot, by = c("Territorio", "TIME"), all.x = TRUE)

# changing names of columns
names(universita)[names(universita) == "Territorio"] <- "Region"
names(universita)[names(universita) == "TIME"] <- "Year"
names(universita)[names(universita) == "X..rinunce"] <- "Dropouts"

rm(iscritti, iscritti1, iscritti2, rinuncia, rinuncia1)

# write.csv(universita, "dati_uni.txt", row.names = FALSE)


## population
# emigrations
emi <- read.csv("DS_covariate/popolazione e famiglie/emigrazioni_prov.csv", header = T)
colnames(emi) # time, value, età, sesso, territorio di origine
emi1 <- emi[,c("Territorio.di.origine", "Sesso", "Età", "TIME", "Value")]

unique(emi1$Territorio.di.origine)
unique(emi1$TIME) 
unique(emi1$Sesso)
unique(emi1$Età)

regioni <- unique(universita$Territorio)
regioni <-  subset(regioni, !(regioni %in% c("Trento","Bolzano / Bozen")))

emi1 <- emi1[which(emi1$Sesso == 'totale'),]
emi1 <- emi1[which(emi1$Età == 'totale'),]
emi1 <- subset(emi1, (emi1$Territorio.di.origine %in% regioni))

emi2 <- emi1 %>%
  group_by(TIME, Territorio.di.origine, Sesso, Età) %>%
  summarise(Emigrations = sum(Value))
rm(emi, emi1)

# imigrations
imm <- read.csv("DS_covariate/popolazione e famiglie/immigrazione_prov.csv", header = T)
colnames(imm) # time, value, età, sesso, territorio di origine
imm1 <- imm[,c("Territorio.di.di.destinazione", "Sesso", "Età", "TIME", "Value")]

unique(imm1$Territorio.di.di.destinazione)
unique(imm1$TIME) 
unique(imm1$Sesso) # eliminiamo totale
unique(imm1$Età)

imm1 <- imm1[which(imm1$Sesso == 'totale'),]
imm1 <- imm1[which(imm1$Età == 'totale'),]
imm1 <- subset(imm1, (imm1$Territorio.di.di.destinazione %in% regioni))

imm2 <- imm1 %>%
  group_by(TIME, Territorio.di.di.destinazione, Sesso, Età) %>%
  summarise(Imigrations = sum(Value))

names(emi2)[names(emi2) == "Territorio.di.origine"] <- "Region"
names(imm2)[names(imm2) == "Territorio.di.di.destinazione"] <- "Region"

popolazione <- merge(emi2, imm2, by = c("Sesso", "Region", "TIME", "Età"), all.x = TRUE)
popolazione <- popolazione[which(popolazione$TIME != 2022),]

# change colnames 
names(popolazione)[names(popolazione) == "Sesso"] <- "Sex"
names(popolazione)[names(popolazione) == "TIME"] <- "Year"

# normalize over popolation
dati_completi <- inner_join(popolazione, res_tot, by = c("Region", "Year"))
dati_completi <- dati_completi %>%
  mutate(Emigrations = Emigrations / Value)
dati_completi <- dati_completi %>%
  mutate(Imigrations = Imigrations / Value)

# create dataset
popolazione.std <- dati_completi[,c('Region', 'Year', 'Emigrations', 'Imigrations')]

# write.csv(popolazione.std, "dati_immigrazioni_emigrazioni.txt", row.names = FALSE)


## health
interr_per_prov_eta_anno <- read.csv("DS_covariate/Salute e sanità/interr_per_prov_eta_anno.csv")
interr_per_prov_eta_anno <- interr_per_prov_eta_anno[which(interr_per_prov_eta_anno$ITTER107_A!="ITC20"),]
interruzioni <- interr_per_prov_eta_anno[which(interr_per_prov_eta_anno$Tipo.dato=="interruzioni volontarie della gravidanza - valori percentuali"),c("TIME","Territorio.di.residenza","Età.e.classe.di.età","Value")]

unique(interruzioni$Età.e.classe.di.età)
# tolgo Età.e.classe.di.età = "non indicato", "fino a 14 anni", "totale", "50 anni e più"
interruzioni <- subset(interruzioni, (interruzioni$Età.e.classe.di.età %in% c("25-29 anni","30-34 anni")))

# lasciando Territorio.di.residenza con solo le province
interruzioni <- interruzioni[which(interruzioni$Territorio.di.residenza %in% regioni),]
names(interruzioni)[names(interruzioni) == "Territorio.di.residenza"] <- "Region"
names(interruzioni)[names(interruzioni) == "Value"] <- "Abortions" # % interruzioni per gruppo sul totale delle interruzioni nell'anno
names(interruzioni)[names(interruzioni) == "Età.e.classe.di.età"] <- "Age"
names(interruzioni)[names(interruzioni) == "TIME"] <- "Year"

# write.csv(interruzioni, "dati_interruzioni_gravidanze.txt", row.names = FALSE)


## economy
# employment rate
to <- read.csv("DS_covariate/TO (tasso di occupazione)/occupazioneistat.csv", header = T)
to_pre <- read.csv("DS_covariate/TO (tasso di occupazione)/occupazionepre.csv", header = T)
# titolo di studio totale

colnames(to_pre)

length(unique(to_tot$Territorio))
length(unique(to_tot$TIME))
length(unique(to_tot$Classe.di.età))

anni <- 2002:2020
to_pre1 <- subset(to_pre, (to_pre$TIME %in% anni))
to_pre1 <- to_pre1[which(to_pre1$Titolo.di.studio == 'totale'), c('Titolo.di.studio', "Territorio", "Sesso", "Classe.di.età", "TIME", "Value")]
to_pre1 <- to_pre1[which(to_pre1$Classe.di.età == "15-64 anni"),]
to_pre1 <- to_pre1[which(to_pre1$Sesso == 'totale'),]
to_pre1 <- subset(to_pre1, (to_pre1$Territorio %in% regioni))

to_2021 <- to[which(to$TIME == 2021), c("Territorio", "Sesso", "Classe.di.età", "TIME", "Value", 'Titolo.di.studio')]
to_2021 <- to_2021[which(to_2021$Titolo.di.studio == 'totale'), ]
to_2021 <- to_2021[which(to_2021$Classe.di.età == "15-64 anni"),]
to_2021 <- to_2021[which(to_2021$Sesso == 'totale'),]
to_2021 <- subset(to_2021, (to_2021$Territorio %in% regioni))

to_tot <- rbind(to_pre1, to_2021)
rm(to_pre, to, to_pre1, to_2021)


# unemployment rate
dis <- read.csv("DS_covariate/TO (tasso di occupazione)/disoccupazioneistat.csv", header = T)
dis2021 <- read.csv("DS_covariate/TO (tasso di occupazione)/dis2021.csv", header = T)
colnames(dis)
unique(dis$TIME)

anni <- 2002:2020
dis1 <- subset(dis, (dis$TIME %in% anni))
dis1 <- dis1[,c("Territorio", "Sesso", "Classe.di.età", "TIME", "Value")]
dis1 <- dis1[which(dis1$Classe.di.età == "15 anni e più"),]

dis2021 <- dis2021[which(dis2021$TIME == 2021), c("Territorio", "Sesso", "Classe.di.età", "TIME", "Value")]
dis2021 <- dis2021[which(dis2021$Classe.di.età == "15-64 anni"),]

dis_tot <- rbind(dis1, dis2021)
rm(dis, dis2021, dis1)

dis_tot <- dis_tot[which(dis_tot$Sesso == 'totale'),]
dis_tot <- subset(dis_tot, (dis_tot$Territorio %in% regioni))

names(dis_tot)[names(dis_tot) == "Value"] <- "Unemployment rate" 
names(to_tot)[names(to_tot) == "Value"] <- "Employment rate" 

occupazione <- merge(to_tot, dis_tot, by = c("Territorio", "TIME"), all.x = TRUE)
occupazione <- occupazione[, c('Territorio', 'TIME', "Employment rate", "Unemployment rate")]

names(occupazione)[names(occupazione) == "Territorio"] <- "Region"
names(occupazione)[names(occupazione) == "TIME"] <- "Year"

# write.csv(occupazione, "dati_inattivita_occupazione.txt", row.names = FALSE)
