dati <- read_delim("DS_covariate/Iscritti all'università/Serie_iscritti.csv", 
                  delim = ";", escape_double = FALSE, col_types = cols(AteneoCOD = col_skip(), 
                  AteneoNOME = col_skip(), AteneoAREAGEO = col_skip(), 
                 CorsoTIPO = col_skip(), COD_FoET2013 = col_skip(), 
                   DESC_FoET2013 = col_skip()), trim_ws = TRUE)
anni <- unique(dati$ANNO)
regioni <- unique(dati$AteneoREGIONE)[-1]
tasso <- matrix(0, nrow=length(anni), ncol=length(regioni))
for (i in 1:length(anni))
{
  for(j in 1:length(regioni))
  {
    inx <- which(dati$ANNO==anni[i] & dati$AteneoREGIONE==regioni[j])
    inx_f <- which(dati$ANNO==anni[i] & dati$AteneoREGIONE==regioni[j] & dati$Genere=="F")
    temp_tot <- sum(dati$ISC[inx])
    temp_f <- sum(dati$ISC[inx_f])
    tasso[i,j] <- temp_f/temp_tot
  }
}
tasso <- data.frame(tasso)
names(tasso) <- regioni
row.names(tasso) <- anni
write.csv(tasso, file="Regression/Dati_clean/dati_uni_post.csv")
