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


# tasso di occupazione
to_prov <- read.csv("DS_covariate/TO (tasso di occupazione)/TO_dati_prov_04.csv", header = T)
to_prov <- to_prov[which(to_prov$ITTER107!="ITC20"),]

colnames(to_prov)

# in to_prov tengo le colonne: Territorio (per regione), Sesso (in realta 
# no perchè c'è solo totale), classe età, value, time
to_prov1 <- to_prov[,c("Territorio", "Classe.di.età", "TIME", "Value")]
unique(to_prov1$Territorio)
unique(to_prov1$TIME)

anni <- 2004:2021

to_prov1 <- subset(to_prov1, (to_prov1$TIME %in% anni))
to_prov1 <- subset(to_prov1, (to_prov1$Classe.di.età %in% c("15-24 anni", "25-34 anni", "35-44 anni", "45-54 anni", "55-64 anni")))

regioni <- universita$Territorio
to_prov2 <- subset(to_prov1, (to_prov1$Territorio %in% regioni))

to_aree <- subset(to_prov1, (to_prov1$Territorio %in% c("Nord", "Mezzogiorno", "Centro")))
to_tot <- to_prov1[which(to_prov1$Territorio == 'Italia'), ]
# attenzione perchè mancano un po di dati, per mezzogiorno ma anche per quello totale
rm(to_prov)

# write.table(to_prov2, "dati_tasso_occupazione_regione.txt", row.names = FALSE)
# write.table(to_aree, "dati_tasso_occupazione_aree.txt", row.names = FALSE)
# write.table(to_tot, "dati_tasso_occupazione_totale.txt", row.names = FALSE)


# popolazione
emi <- read.csv("DS_covariate/popolazione e famiglie/emigrazioni_prov.csv", header = T)
colnames(emi) # time, value, età, sesso, territorio di origine
emi1 <- emi[,c("Territorio.di.origine", "Sesso", "Età", "TIME", "Value")]

unique(emi1$Territorio.di.origine)
unique(emi1$TIME) 
unique(emi1$Sesso)
unique(emi1$Età)

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

