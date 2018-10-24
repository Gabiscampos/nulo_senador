#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Matching neighboring cities by presidential votes
# October 22, 2018

# Clearing
rm(list=ls())

# Loading the neighbors data #------------------------------------------------------
#setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/") # Change HERE
#data<-read.table("Data/BR_mun_neighbors.txt", encoding="latin1") # Neighboring Municipality Data

pres2006<-get_votes(2006, "President", "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

pres2006<-pres2006[, -which(colnames(pres2006) %in% c("DESCRICAO_CARGO", "DESCRICAO_ELEICAO", "CODIGO_CARGO", "SIGLA_UE"))] # Subsetting

pres2006_round1<-pres2006[pres2006$NUM_TURNO==1,] # Por turno
pres2006_round2<-pres2006[pres2006$NUM_TURNO==2,]

pres2006_municipality1<-pres2006_round1[, c("QTDE_VOTOS")] # Subsetting
head(pres2006_municipality1); nrow(pres2006_municipality1)

pres2006_mun<-aggregate(pres2006_municipality1, by=list(pres2006_round1$COD_MUN_IBGE), FUN=sum)
head(pres2006_mun); nrow(pres2006_mun)
colnames(pres2006_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# Aécio
pres2006_aecio<-as.data.frame( # Votes for Aecio
  pres2006_round1[pres2006_round1 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "NOME_MUNICIPIO", "UF", "NOME_MACRO", "COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2006_aecio); nrow(pres2006_aecio); colnames(pres2006_aecio)[6]<-"votos_aecio"

# Dilma 
pres2006_dilma<-as.data.frame( # Votes for Dilma
  pres2006_round1[pres2006_round1 $NUMERO_CANDIDATO==13,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2006_dilma); nrow(pres2006_dilma); colnames(pres2006_dilma)[2]<-"votos_dilma"

pres2006_aecio[!pres2006_aecio$COD_MUN_IBGE %in% pres2006_dilma$COD_MUN_IBGE,] 
# In what municipality did Dilma not have votes?
pres2006_dilma[nrow(pres2006_dilma)+1,]<-c(29297, 0) # Creating the extra municipality

# Marina
pres2006_marina<-as.data.frame( # Votes for Marina
  pres2006_round1[pres2006_round1$NUMERO_CANDIDATO==40,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2006_marina); nrow(pres2006_marina); colnames(pres2006_marina)[2]<-"votos_marina"

# Invalid votes #-------------------------------------------------------

pres2006_inv<-as.data.frame( # Votes - Invalid
  pres2006_round1[pres2006_round1$NUMERO_CANDIDATO==95,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2006_inv); nrow(pres2006_inv); colnames(pres2006_inv)[2]<-"votos_inv"

# In what municipality there was not invalid votes?
mis<-pres2006_aecio[!pres2006_aecio$COD_MUN_IBGE %in% pres2006_inv$COD_MUN_IBGE,] 
pres2006_inv<-rbind(pres2006_inv,
                    data.frame(COD_MUN_IBGE=mis[,1], votos_inv=rep(0, nrow(mis))))

pres2006_inv2<-as.data.frame( # Votes - Invalid
  pres2006_round1[pres2006_round1$NUMERO_CANDIDATO==96,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2006_inv2); nrow(pres2006_inv2); colnames(pres2006_inv2)[2]<-"votos_inv2"

pres2006_inv<-merge(pres2006_inv, pres2006_inv2, by="COD_MUN_IBGE", all=T)
pres2006_inv$votos_inv<-pres2006_inv$votos_inv+pres2006_inv$votos_inv2
pres2006_inv$votos_inv2<-NULL
pres2006_inv$votos_inv[is.na(pres2006_inv$votos_inv)]<-0

# Downloading senator votes --------------------------------------------------------- 
senate2006<- get_elections(2006, "Senator", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

senate2006_municipality<-senate2006[, c("QTDE_VOTOS")] # Subsetting
head(senate2006_municipality); nrow(senate2006_municipality)

senate2006_mun<-aggregate(senate2006_municipality, by=list(senate2006$COD_MUN_IBGE), FUN=sum)
head(senate2006_mun); nrow(senate2006_mun)
colnames(senate2006_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
senate2006_PSDB<-as.data.frame( # Votes for PSDB Senators
  senate2006[senate2006 $NUMERO_PARTIDO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2006_PSDB); nrow(senate2006_PSDB); colnames(senate2006_PSDB)[4]<-"votos_PSDB_sen"

# PT
senate2006_PT<-as.data.frame( # Votes for PT Senators
  senate2006[senate2006 $NUMERO_PARTIDO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2006_PT); nrow(senate2006_PT); colnames(senate2006_PT)[4]<-"votos_PT_sen"

# PSB
senate2006_PSB<-as.data.frame( # Votes for PSB Senators
  senate2006[senate2006 $NUMERO_PARTIDO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2006_PSB); nrow(senate2006_PSB); colnames(senate2006_PSB)[4]<-"votos_PSB_sen"

# Invalid votes for Senator
senate2006_invalid<-get_elections(2006, "Senator", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
senate2006_invalid<-senate2006_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QT_VOTOS_NOMINAIS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_sen=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_sen=QT_VOTOS_NOMINAIS)

# Downloading legislative federal votes -------------------------------------------  
DF2006<- get_votes(2006, "Deputado Federal", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

DF2006_municipality<-DF2006[, c("QTDE_VOTOS")] # Subsetting
head(DF2006_municipality); nrow(DF2006_municipality)

DF2006_mun<-aggregate(DF2006_municipality, by=list(DF2006$COD_MUN_IBGE), FUN=sum)
head(DF2006_mun); nrow(DF2006_mun)
colnames(DF2006_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
DF2006_PSDB<-as.data.frame( # Votes for PSDB Senators
  DF2006[DF2006 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2006_PSDB); nrow(DF2006_PSDB); colnames(DF2006_PSDB)[4]<-"votos_PSDB_PLV"

# PT
DF2006_PT<-as.data.frame( # Votes for PT Senators
  DF2006[DF2006 $NUMERO_CANDIDATO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2006_PT); nrow(DF2006_PT); colnames(DF2006_PT)[4]<-"votos_PT_PLV"

# PSB
DF2006_PSB<-as.data.frame( # Votes for PSB Senators
  DF2006[DF2006 $NUMERO_CANDIDATO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2006_PSB); nrow(DF2006_PSB); colnames(DF2006_PSB)[4]<-"votos_PSB_PLV"

# Invalid votes for Senator
DF2006_invalid<-get_elections(2006, "Deputado Federal", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
DF2006_invalid<-DF2006_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS","QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_DF=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_DF=QT_VOTOS_NOMINAIS)

rm(DF2006)

# Now, let's merge the presidential votes datafiles #-----------------------------

data_politics<-merge(pres2006_aecio, pres2006_dilma, by="COD_MUN_IBGE", all=T) %>% filter(UF!="ZZ")
data_politics<-left_join(data_politics, pres2006_marina, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2006_inv, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2006_mun, by="COD_MUN_IBGE"); head(data_politics)

data_politics$votos_aecio_pc<-100*data_politics$votos_aecio/data_politics$QTDE_VOTOS
data_politics$votos_dilma_pc<-100*data_politics$votos_dilma/data_politics$QTDE_VOTOS # Percentages
data_politics$votos_marina_pc<-100*data_politics$votos_marina/data_politics$QTDE_VOTOS
data_politics$votos_inv_pc<-100*data_politics$votos_inv/data_politics$QTDE_VOTOS; head(data_politics)

head(data_politics)

# Now, let's merge the senator votes datafiles #-----------------------------
data_politics<-left_join(data_politics, senate2006_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2006_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2006_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2006_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

#sub NA per 0
data_politics$votos_PSDB_sen<-ifelse(is.na(data_politics$votos_PSDB_sen),0,data_politics$votos_PSDB_sen)
data_politics$votos_PT_sen<-ifelse(is.na(data_politics$votos_PT_sen),0,data_politics$votos_PT_sen)
data_politics$votos_PSB_sen<-ifelse(is.na(data_politics$votos_PSB_sen),0,data_politics$votos_PSB_sen)

data_politics$votos_PSDB_sen_pc<-100*data_politics$votos_PSDB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_PT_sen_pc<-100*data_politics$votos_PT_sen/data_politics$QT_VOTOS_sen # Percentages
data_politics$votos_PSB_sen_pc<-100*data_politics$votos_PSB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_inv_sen_pc<-100*data_politics$votos_inv_sen/data_politics$QT_VOTOS_sen; head(data_politics)

# Now, let's merge the Federal Deputy votes datafiles #-----------------------------
data_politics<-left_join(data_politics, DF2006_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2006_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2006_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2006_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

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

PLV_2006<-data_politics %>% mutate(regiao = substr(COD_MUN_IBGE,1,1)) %>% 
  select(regiao, COD_MUN_IBGE, votos_PT_PLV_pc, votos_PSDB_PLV_pc, votos_PSB_PLV_pc) %>% 
  gather(variavel, valor, 3:5) %>% 
  ggplot(aes(x = regiao,  y=valor)) + 
  geom_boxplot(aes(fill=variavel)) +
  scale_y_continuous(limits = c(0,10))

PLV_2006
