#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Matching neighboring cities by presidential votes
# October 22, 2018

# Clearing
rm(list=ls())

# Loading the neighbors data #------------------------------------------------------
#setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/") # Change HERE
#data<-read.table("Data/BR_mun_neighbors.txt", encoding="latin1") # Neighboring Municipality Data

pres2010<-get_votes(2010, "President", "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

pres2010<-pres2010[, -which(colnames(pres2010) %in% c("DESCRICAO_CARGO", "DESCRICAO_ELEICAO", "CODIGO_CARGO", "SIGLA_UE"))] # Subsetting

pres2010_round1<-pres2010[pres2010$NUM_TURNO==1,] # Por turno
pres2010_round2<-pres2010[pres2010$NUM_TURNO==2,]

pres2010_municipality1<-pres2010_round1[, c("QTDE_VOTOS")] # Subsetting
head(pres2010_municipality1); nrow(pres2010_municipality1)

pres2010_mun<-aggregate(pres2010_municipality1, by=list(pres2010_round1$COD_MUN_IBGE), FUN=sum)
head(pres2010_mun); nrow(pres2010_mun)
colnames(pres2010_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# Aécio
pres2010_aecio<-as.data.frame( # Votes for Aecio
  pres2010_round1[pres2010_round1 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "NOME_MUNICIPIO", "UF", "NOME_MACRO", "COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2010_aecio); nrow(pres2010_aecio); colnames(pres2010_aecio)[6]<-"votos_aecio"

# Dilma 
pres2010_dilma<-as.data.frame( # Votes for Dilma
  pres2010_round1[pres2010_round1 $NUMERO_CANDIDATO==13,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2010_dilma); nrow(pres2010_dilma); colnames(pres2010_dilma)[2]<-"votos_dilma"

pres2010_aecio[!pres2010_aecio$COD_MUN_IBGE %in% pres2010_dilma$COD_MUN_IBGE,] 
# In what municipality did Dilma not have votes?
pres2010_dilma[nrow(pres2010_dilma)+1,]<-c(29297, 0) # Creating the extra municipality

# Marina
pres2010_marina<-as.data.frame( # Votes for Marina
  pres2010_round1[pres2010_round1$NUMERO_CANDIDATO==40,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2010_marina); nrow(pres2010_marina); colnames(pres2010_marina)[2]<-"votos_marina"

# Invalid votes #-------------------------------------------------------

pres2010_inv<-as.data.frame( # Votes - Invalid
  pres2010_round1[pres2010_round1$NUMERO_CANDIDATO==95,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2010_inv); nrow(pres2010_inv); colnames(pres2010_inv)[2]<-"votos_inv"

# In what municipality there was not invalid votes?
mis<-pres2010_aecio[!pres2010_aecio$COD_MUN_IBGE %in% pres2010_inv$COD_MUN_IBGE,] 
pres2010_inv<-rbind(pres2010_inv,
                    data.frame(COD_MUN_IBGE=mis[,1], votos_inv=rep(0, nrow(mis))))

pres2010_inv2<-as.data.frame( # Votes - Invalid
  pres2010_round1[pres2010_round1$NUMERO_CANDIDATO==96,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2010_inv2); nrow(pres2010_inv2); colnames(pres2010_inv2)[2]<-"votos_inv2"

pres2010_inv<-merge(pres2010_inv, pres2010_inv2, by="COD_MUN_IBGE", all=T)
pres2010_inv$votos_inv<-pres2010_inv$votos_inv+pres2010_inv$votos_inv2
pres2010_inv$votos_inv2<-NULL
pres2010_inv$votos_inv[is.na(pres2010_inv$votos_inv)]<-0

# Downloading senator votes --------------------------------------------------------- 
senate2010<- get_elections(2010, "Senator", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

senate2010_municipality<-senate2010[, c("QTDE_VOTOS")] # Subsetting
head(senate2010_municipality); nrow(senate2010_municipality)

senate2010_mun<-aggregate(senate2010_municipality, by=list(senate2010$COD_MUN_IBGE), FUN=sum)
head(senate2010_mun); nrow(senate2010_mun)
colnames(senate2010_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
senate2010_PSDB<-as.data.frame( # Votes for PSDB Senators
  senate2010[senate2010 $NUMERO_PARTIDO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2010_PSDB); nrow(senate2010_PSDB); colnames(senate2010_PSDB)[4]<-"votos_PSDB_sen"

# PT
senate2010_PT<-as.data.frame( # Votes for PT Senators
  senate2010[senate2010 $NUMERO_PARTIDO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2010_PT); nrow(senate2010_PT); colnames(senate2010_PT)[4]<-"votos_PT_sen"

# PSB
senate2010_PSB<-as.data.frame( # Votes for PSB Senators
  senate2010[senate2010 $NUMERO_PARTIDO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2010_PSB); nrow(senate2010_PSB); colnames(senate2010_PSB)[4]<-"votos_PSB_sen"

# Invalid votes for Senator
senate2010_invalid<-get_elections(2010, "Senator", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
senate2010_invalid<-senate2010_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QT_VOTOS_NOMINAIS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_sen=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_sen=QT_VOTOS_NOMINAIS)

# Downloading legislative federal votes -------------------------------------------  
DF2010<- get_votes(2010, "Deputado Federal", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

DF2010_municipality<-DF2010[, c("QTDE_VOTOS")] # Subsetting
head(DF2010_municipality); nrow(DF2010_municipality)

DF2010_mun<-aggregate(DF2010_municipality, by=list(DF2010$COD_MUN_IBGE), FUN=sum)
head(DF2010_mun); nrow(DF2010_mun)
colnames(DF2010_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
DF2010_PSDB<-as.data.frame( # Votes for PSDB Senators
  DF2010[DF2010 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2010_PSDB); nrow(DF2010_PSDB); colnames(DF2010_PSDB)[4]<-"votos_PSDB_PLV"

# PT
DF2010_PT<-as.data.frame( # Votes for PT Senators
  DF2010[DF2010 $NUMERO_CANDIDATO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2010_PT); nrow(DF2010_PT); colnames(DF2010_PT)[4]<-"votos_PT_PLV"

# PSB
DF2010_PSB<-as.data.frame( # Votes for PSB Senators
  DF2010[DF2010 $NUMERO_CANDIDATO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2010_PSB); nrow(DF2010_PSB); colnames(DF2010_PSB)[4]<-"votos_PSB_PLV"

# Invalid votes for Senator
DF2010_invalid<-get_elections(2010, "Deputado Federal", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
DF2010_invalid<-DF2010_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS","QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_DF=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_DF=QT_VOTOS_NOMINAIS)

rm(DF2010)

# Now, let's merge the presidential votes datafiles #-----------------------------

data_politics<-merge(pres2010_aecio, pres2010_dilma, by="COD_MUN_IBGE", all=T) %>% filter(UF!="ZZ")
data_politics<-left_join(data_politics, pres2010_marina, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2010_inv, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2010_mun, by="COD_MUN_IBGE"); head(data_politics)

data_politics$votos_aecio_pc<-100*data_politics$votos_aecio/data_politics$QTDE_VOTOS
data_politics$votos_dilma_pc<-100*data_politics$votos_dilma/data_politics$QTDE_VOTOS # Percentages
data_politics$votos_marina_pc<-100*data_politics$votos_marina/data_politics$QTDE_VOTOS
data_politics$votos_inv_pc<-100*data_politics$votos_inv/data_politics$QTDE_VOTOS; head(data_politics)

head(data_politics)

# Now, let's merge the senator votes datafiles #-----------------------------
data_politics<-left_join(data_politics, senate2010_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2010_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2010_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2010_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

#sub NA per 0
data_politics$votos_PSDB_sen<-ifelse(is.na(data_politics$votos_PSDB_sen),0,data_politics$votos_PSDB_sen)
data_politics$votos_PT_sen<-ifelse(is.na(data_politics$votos_PT_sen),0,data_politics$votos_PT_sen)
data_politics$votos_PSB_sen<-ifelse(is.na(data_politics$votos_PSB_sen),0,data_politics$votos_PSB_sen)

data_politics$votos_PSDB_sen_pc<-100*data_politics$votos_PSDB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_PT_sen_pc<-100*data_politics$votos_PT_sen/data_politics$QT_VOTOS_sen # Percentages
data_politics$votos_PSB_sen_pc<-100*data_politics$votos_PSB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_inv_sen_pc<-100*data_politics$votos_inv_sen/data_politics$QT_VOTOS_sen; head(data_politics)

# Now, let's merge the Federal Deputy votes datafiles #-----------------------------
data_politics<-left_join(data_politics, DF2010_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2010_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2010_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2010_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

#sub NA per 0
data_politics$votos_PSDB_PLV<-ifelse(is.na(data_politics$votos_PSDB_PLV),0,data_politics$votos_PSDB_PLV)
data_politics$votos_PT_PLV<-ifelse(is.na(data_politics$votos_PT_PLV),0,data_politics$votos_PT_PLV)
data_politics$votos_PSB_PLV<-ifelse(is.na(data_politics$votos_PSB_PLV),0,data_politics$votos_PSB_PLV)

data_politics$votos_PSDB_PLV_pc<-100*data_politics$votos_PSDB_PLV/data_politics$QT_VOTOS_DF
data_politics$votos_PT_PLV_pc<-100*data_politics$votos_PT_PLV/data_politics$QT_VOTOS_DF # Percentages
data_politics$votos_PSB_PLV_pc<-100*data_politics$votos_PSB_PLV/data_politics$QT_VOTOS_DF
data_politics$votos_inv_PLV_pc<-100*data_politics$votos_inv_DF/data_politics$QT_VOTOS_DF; head(data_politics)

library(ggplot2)
data_politics %>%
  select(COD_MUN_IBGE, votos_PT_PLV_pc, votos_PSDB_PLV_pc, votos_PSB_PLV_pc) %>% 
  gather(COD_MUN_IBGE) %>% 
  ggplot(aes(x = factor(COD_MUN_IBGE),  y=value)) + 
  geom_boxplot()
#voto na legenda do PSDB tem mediana bem próxima a do PT e maior variância 

PLV_2010<-data_politics %>% mutate(regiao = substr(COD_MUN_IBGE,1,1)) %>% 
  select(regiao, COD_MUN_IBGE, votos_PT_PLV_pc, votos_PSDB_PLV_pc, votos_PSB_PLV_pc) %>% 
  gather(variavel, valor, 3:5) %>% 
  ggplot(aes(x = regiao,  y=valor)) + 
  geom_boxplot(aes(fill=variavel)) +
  scale_y_continuous(limits = c(0,10))

PLV_2010