#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Matching neighboring cities by presidential votes
# October 22, 2018

# Clearing
rm(list=ls())

# Loading the neighbors data #------------------------------------------------------
#setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/") # Change HERE
#data<-read.table("Data/BR_mun_neighbors.txt", encoding="latin1") # Neighboring Municipality Data

pres2002<-get_votes(2002, "President", "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

pres2002<-pres2002[, -which(colnames(pres2002) %in% c("DESCRICAO_CARGO", "DESCRICAO_ELEICAO", "CODIGO_CARGO", "SIGLA_UE"))] # Subsetting

pres2002_round1<-pres2002[pres2002$NUM_TURNO==1,] # Por turno
pres2002_round2<-pres2002[pres2002$NUM_TURNO==2,]

pres2002_municipality1<-pres2002_round1[, c("QTDE_VOTOS")] # Subsetting
head(pres2002_municipality1); nrow(pres2002_municipality1)

pres2002_mun<-aggregate(pres2002_municipality1, by=list(pres2002_round1$COD_MUN_IBGE), FUN=sum)
head(pres2002_mun); nrow(pres2002_mun)
colnames(pres2002_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# Aécio
pres2002_aecio<-as.data.frame( # Votes for Aecio
  pres2002_round1[pres2002_round1 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "NOME_MUNICIPIO", "UF", "NOME_MACRO", "COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2002_aecio); nrow(pres2002_aecio); colnames(pres2002_aecio)[6]<-"votos_aecio"

# Dilma 
pres2002_dilma<-as.data.frame( # Votes for Dilma
  pres2002_round1[pres2002_round1 $NUMERO_CANDIDATO==13,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2002_dilma); nrow(pres2002_dilma); colnames(pres2002_dilma)[2]<-"votos_dilma"

pres2002_aecio[!pres2002_aecio$COD_MUN_IBGE %in% pres2002_dilma$COD_MUN_IBGE,] 
# In what municipality did Dilma not have votes?
pres2002_dilma[nrow(pres2002_dilma)+1,]<-c(29297, 0) # Creating the extra municipality

# Marina
pres2002_marina<-as.data.frame( # Votes for Marina
  pres2002_round1[pres2002_round1$NUMERO_CANDIDATO==40,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2002_marina); nrow(pres2002_marina); colnames(pres2002_marina)[2]<-"votos_marina"

# Invalid votes #-------------------------------------------------------

pres2002_inv<-as.data.frame( # Votes - Invalid
  pres2002_round1[pres2002_round1$NUMERO_CANDIDATO==95,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2002_inv); nrow(pres2002_inv); colnames(pres2002_inv)[2]<-"votos_inv"

# In what municipality there was not invalid votes?
mis<-pres2002_aecio[!pres2002_aecio$COD_MUN_IBGE %in% pres2002_inv$COD_MUN_IBGE,] 
pres2002_inv<-rbind(pres2002_inv,
                    data.frame(COD_MUN_IBGE=mis[,1], votos_inv=rep(0, nrow(mis))))

pres2002_inv2<-as.data.frame( # Votes - Invalid
  pres2002_round1[pres2002_round1$NUMERO_CANDIDATO==96,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2002_inv2); nrow(pres2002_inv2); colnames(pres2002_inv2)[2]<-"votos_inv2"

pres2002_inv<-merge(pres2002_inv, pres2002_inv2, by="COD_MUN_IBGE", all=T)
pres2002_inv$votos_inv<-pres2002_inv$votos_inv+pres2002_inv$votos_inv2
pres2002_inv$votos_inv2<-NULL
pres2002_inv$votos_inv[is.na(pres2002_inv$votos_inv)]<-0

# Downloading senator votes --------------------------------------------------------- 
senate2002<- get_elections(2002, "Senator", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

senate2002_municipality<-senate2002[, c("QTDE_VOTOS")] # Subsetting
head(senate2002_municipality); nrow(senate2002_municipality)

senate2002_mun<-aggregate(senate2002_municipality, by=list(senate2002$COD_MUN_IBGE), FUN=sum)
head(senate2002_mun); nrow(senate2002_mun)
colnames(senate2002_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
senate2002_PSDB<-as.data.frame( # Votes for PSDB Senators
  senate2002[senate2002 $NUMERO_PARTIDO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2002_PSDB); nrow(senate2002_PSDB); colnames(senate2002_PSDB)[4]<-"votos_PSDB_sen"

# PT
senate2002_PT<-as.data.frame( # Votes for PT Senators
  senate2002[senate2002 $NUMERO_PARTIDO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2002_PT); nrow(senate2002_PT); colnames(senate2002_PT)[4]<-"votos_PT_sen"

# PSB
senate2002_PSB<-as.data.frame( # Votes for PSB Senators
  senate2002[senate2002 $NUMERO_PARTIDO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2002_PSB); nrow(senate2002_PSB); colnames(senate2002_PSB)[4]<-"votos_PSB_sen"

# Invalid votes for Senator
senate2002_invalid<-get_elections(2002, "Senator", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
senate2002_invalid<-senate2002_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QT_VOTOS_NOMINAIS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_sen=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_sen=QT_VOTOS_NOMINAIS)

# Downloading legislative federal votes -------------------------------------------  
DF2002<- get_votes(2002, "Deputado Federal", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

DF2002_municipality<-DF2002[, c("QTDE_VOTOS")] # Subsetting
head(DF2002_municipality); nrow(DF2002_municipality)

DF2002_mun<-aggregate(DF2002_municipality, by=list(DF2002$COD_MUN_IBGE), FUN=sum)
head(DF2002_mun); nrow(DF2002_mun)
colnames(DF2002_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
DF2002_PSDB<-as.data.frame( # Votes for PSDB Senators
  DF2002[DF2002 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2002_PSDB); nrow(DF2002_PSDB); colnames(DF2002_PSDB)[4]<-"votos_PSDB_PLV"

# PT
DF2002_PT<-as.data.frame( # Votes for PT Senators
  DF2002[DF2002 $NUMERO_CANDIDATO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2002_PT); nrow(DF2002_PT); colnames(DF2002_PT)[4]<-"votos_PT_PLV"

# PSB
DF2002_PSB<-as.data.frame( # Votes for PSB Senators
  DF2002[DF2002 $NUMERO_CANDIDATO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2002_PSB); nrow(DF2002_PSB); colnames(DF2002_PSB)[4]<-"votos_PSB_PLV"

# Invalid votes for Senator
DF2002_invalid<-get_elections(2002, "Deputado Federal", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
DF2002_invalid<-DF2002_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS","QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_DF=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_DF=QT_VOTOS_NOMINAIS)

rm(DF2002)

# Now, let's merge the presidential votes datafiles #-----------------------------

data_politics<-merge(pres2002_aecio, pres2002_dilma, by="COD_MUN_IBGE", all=T) %>% filter(UF!="ZZ")
data_politics<-left_join(data_politics, pres2002_marina, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2002_inv, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2002_mun, by="COD_MUN_IBGE"); head(data_politics)

data_politics$votos_aecio_pc<-100*data_politics$votos_aecio/data_politics$QTDE_VOTOS
data_politics$votos_dilma_pc<-100*data_politics$votos_dilma/data_politics$QTDE_VOTOS # Percentages
data_politics$votos_marina_pc<-100*data_politics$votos_marina/data_politics$QTDE_VOTOS
data_politics$votos_inv_pc<-100*data_politics$votos_inv/data_politics$QTDE_VOTOS; head(data_politics)

head(data_politics)

# Now, let's merge the senator votes datafiles #-----------------------------
data_politics<-left_join(data_politics, senate2002_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2002_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2002_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2002_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

#sub NA per 0
data_politics$votos_PSDB_sen<-ifelse(is.na(data_politics$votos_PSDB_sen),0,data_politics$votos_PSDB_sen)
data_politics$votos_PT_sen<-ifelse(is.na(data_politics$votos_PT_sen),0,data_politics$votos_PT_sen)
data_politics$votos_PSB_sen<-ifelse(is.na(data_politics$votos_PSB_sen),0,data_politics$votos_PSB_sen)

data_politics$votos_PSDB_sen_pc<-100*data_politics$votos_PSDB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_PT_sen_pc<-100*data_politics$votos_PT_sen/data_politics$QT_VOTOS_sen # Percentages
data_politics$votos_PSB_sen_pc<-100*data_politics$votos_PSB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_inv_sen_pc<-100*data_politics$votos_inv_sen/data_politics$QT_VOTOS_sen; head(data_politics)

# Now, let's merge the Federal Deputy votes datafiles #-----------------------------
data_politics<-left_join(data_politics, DF2002_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2002_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2002_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2002_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

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

PLV_2002<-data_politics %>% mutate(regiao = substr(COD_MUN_IBGE,1,1)) %>% 
  select(regiao, COD_MUN_IBGE, votos_PT_PLV_pc, votos_PSDB_PLV_pc, votos_PSB_PLV_pc) %>% 
  gather(variavel, valor, 3:5) %>% 
  ggplot(aes(x = regiao,  y=valor)) + 
  geom_boxplot(aes(fill=variavel)) +
  scale_y_continuous(limits = c(0,10))

PLV_2002
