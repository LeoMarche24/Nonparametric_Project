# università
rinuncia <- read.csv("DS_covariate/Iscritti all'università/rinunci_studi.csv", header = T)
iscritti <- read.csv("DS_covariate/Iscritti all'università/Iscritti_uni_tot.csv", header = T)
rinuncia1 <- rinuncia[, c("Territorio", "Sesso", "TIME", "Value")] # territorio indica la residenza non dove sta l'uni
iscritti1 <- iscritti[, c("Regione.di.residenza", "Sesso", "TIME", "Value")]

unique(iscritti1$TIME)
unique(rinuncia1$TIME)

unique(iscritti1$Regione.di.residenza)
unique(rinuncia1$Territorio)

rinuncia1 <- subset(rinuncia1, !(rinuncia1$Territorio %in% c("Italia","Nord","Nord-ovest","Nord-est","Centro","Mezzogiorno")))
rinuncia1 <- rinuncia1[which(rinuncia1$Sesso != "totale"), ]

iscritti1 <- subset(iscritti1, !(iscritti1$Regione.di.residenza %in% c("Italia","Estero","Non indicato","Totale")))
iscritti1 <- iscritti1[which(iscritti1$Sesso != "totale"), ]

names(iscritti1)[names(iscritti1) == "Regione.di.residenza"] <- "Territorio"
names(iscritti1)[names(iscritti1) == "Value"] <- "Iscritti"
names(rinuncia1)[names(rinuncia1) == "Value"] <- "% rinunce"

iscritti2 <- iscritti1 %>%
  group_by(TIME, Territorio, Sesso) %>%
  summarise(iscritti_tot = sum(Iscritti))

universita <- merge(rinuncia1, iscritti2, by = c("Sesso", "Territorio", "TIME"), all.x = TRUE)
universita <- subset(universita, !(universita$Territorio %in% c("Trento","Bolzano / Bozen")))
rm(iscritti, iscritti1, iscritti2, rinuncia, rinuncia1)

# write.table(universita, "dati_uni.txt", row.names = FALSE)



# popolazione
emi <- read.csv("DS_covariate/popolazione e famiglie/emigrazioni_prov.csv", header = T)
colnames(emi) # time, value, età, sesso, territorio di origine
emi1 <- emi[,c("Territorio.di.origine", "Sesso", "Età", "TIME", "Value")]

unique(emi1$Territorio.di.origine)
unique(emi1$TIME) 
unique(emi1$Sesso)
unique(emi1$Età)

regioni <- universita$Territorio
emi1 <- emi1[which(emi1$Sesso != 'totale'),]
emi1 <- subset(emi1, (emi1$Età %in% c("40-64 anni","18-39 anni")))
emi1 <- subset(emi1, (emi1$Territorio.di.origine %in% regioni))

emi2 <- emi1 %>%
  group_by(TIME, Territorio.di.origine, Sesso, Età) %>%
  summarise(emigrazioni_tot = sum(Value))
# emi1: numero di emigrati da Territorio.di.origine per anno, sesso e età
rm(emi, emi1)


imm <- read.csv("DS_covariate/popolazione e famiglie/immigrazione_prov.csv", header = T)
colnames(imm) # time, value, età, sesso, territorio di origine
imm1 <- imm[,c("Territorio.di.di.destinazione", "Sesso", "Età", "TIME", "Value")]

unique(imm1$Territorio.di.di.destinazione)
unique(imm1$TIME) 
unique(imm1$Sesso) # eliminiamo totale
unique(imm1$Età)

imm1 <- imm1[which(imm1$Sesso != 'totale'),]
imm1 <- subset(imm1, (imm1$Età %in% c("40-64 anni","18-39 anni")))
imm1 <- subset(imm1, (imm1$Territorio.di.di.destinazione %in% regioni))

imm2 <- imm1 %>%
  group_by(TIME, Territorio.di.di.destinazione, Sesso, Età) %>%
  summarise(immigrazioni_tot = sum(Value))

names(emi2)[names(emi2) == "Territorio.di.origine"] <- "Territorio"
names(imm2)[names(imm2) == "Territorio.di.di.destinazione"] <- "Territorio"

popolazione <-  merge(emi2, imm2, by = c("Sesso", "Territorio", "TIME", "Età"), all.x = TRUE)

# write.table(popolazione, "dati_immigrazioni_emigrazioni.txt", row.names = FALSE)


# salute
interr_per_prov_eta_anno <- read.csv("DS_covariate/Salute e sanità/interr_per_prov_eta_anno.csv")
interr_per_prov_eta_anno <- interr_per_prov_eta_anno[which(interr_per_prov_eta_anno$ITTER107_A!="ITC20"),]
interruzioni <- interr_per_prov_eta_anno[which(interr_per_prov_eta_anno$Tipo.dato=="interruzioni volontarie della gravidanza - valori percentuali"),c("TIME","Territorio.di.residenza","Età.e.classe.di.età","Value")]

# tolgo Età.e.classe.di.età = "non indicato", "fino a 14 anni", "totale", "50 anni e più"
excluded_categories <- c("non indicato", "fino a 14 anni", "totale", "50 anni e più")
interruzioni <- subset(interruzioni, !(Età.e.classe.di.età %in% excluded_categories))

# lasciando Territorio.di.residenza con solo le province
interruzioni <- interruzioni[which(interruzioni$Territorio.di.residenza %in% regioni),]
names(interruzioni)[names(interruzioni) == "Territorio.di.residenza"] <- "Territorio"
names(interruzioni)[names(interruzioni) == "Value"] <- "% interruzioni" # % interruzioni per gruppo sul totale delle interruzioni nell'anno
names(interruzioni)[names(interruzioni) == "Età.e.classe.di.età"] <- "Età"

# write.table(interruzioni, "dati_interruzioni_gravidanze.txt", row.names = FALSE)


# tasso di occupazione e disoccupazione
to <- read.csv("occupazioneistat.csv", header = T)
to_pre <- read.csv("occupazionepre.csv", header = T)

colnames(to_pre)

length(unique(to_tot$Territorio))
length(unique(to_tot$TIME))
length(unique(to_tot$Classe.di.età))

anni <- 2002:2020
to_pre1 <- subset(to_pre, (to_pre$TIME %in% anni))
to_pre1 <- to_pre1[,c("Territorio", "Sesso", "Classe.di.età", "TIME", "Value")]
to_2021 <- to[which(to$TIME == 2021), c("Territorio", "Sesso", "Classe.di.età", "TIME", "Value")]

rm(to_pre, to)
to_tot <- rbind(to_pre1, to_2021)

to_tot <- subset(to_tot, (to_tot$Classe.di.età %in% c("15-24 anni","25-34 anni","35-44 anni","45-54 anni","55-64 anni")))
to_tot <- to_tot[which(to_tot$Sesso != 'totale'),]

# disoccupazione
dis <- read.csv("disoccupazioneistat.csv", header = T)
dis2021 <- read.csv("dis2021.csv", header = T)
colnames(dis)
unique(dis$TIME)

anni <- 2002:2020
dis1 <- subset(dis, (dis$TIME %in% anni))
dis1 <- dis1[,c("Territorio", "Sesso", "Classe.di.età", "TIME", "Value")]
dis2021 <- dis2021[which(dis2021$TIME == 2021), c("Territorio", "Sesso", "Classe.di.età", "TIME", "Value")]

dis_tot <- rbind(dis1, dis2021)

dis_tot <- subset(dis_tot, (dis_tot$Classe.di.età %in% c("15-24 anni","25-34 anni","35-44 anni","45-54 anni","55-64 anni")))
dis_tot <- dis_tot[which(dis_tot$Sesso != 'totale'),]
dis_tot <- subset(dis_tot, !(dis_tot$Territorio %in% c("Italia", "Nord", "Nord-ovest", "Nord-est", "Centro", "Mezzogiorno")))
names(dis_tot)[names(dis_tot) == "Value"] <- "Tasso disoccupazione" 
names(to_tot)[names(to_tot) == "Value"] <- "Tasso occupazione" 

occupazione <- merge(to_tot, dis_tot, by = c("Sesso", "Territorio", "TIME", "Classe.di.età"), all.x = TRUE)
occupazione[which(is.na(occupazione), arr.ind = TRUE),] # c'è un na

write.table(occupazione, "dati_inattivita_occupazione.txt", row.names = FALSE)
