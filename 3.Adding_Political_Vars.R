#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Matching neighboring cities by presidential votes
# October 22, 2018

# Clearing
rm(list=ls())

# Loading the neighbors data #------------------------------------------------------
#setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/") # Change HERE
#data<-read.table("Data/BR_mun_neighbors.txt", encoding="latin1") # Neighboring Municipality Data

data<-read.csv("Data/BR_mun_neighbors.csv") # Neighboring Municipality Data
pres2014<- read.table("Data/presidential_2014_municipality.txt") # Electoral Data

pres2014_round1<-pres2014[pres2014$NUM_TURNO==1,] # Por turno
pres2014_round2<-pres2014[pres2014$NUM_TURNO==2,]

pres2014_municipality1<-pres2014_round1[, c("QTDE_VOTOS")] # Subsetting
head(pres2014_municipality1); nrow(pres2014_municipality1)

pres2014_mun<-aggregate(pres2014_municipality1, by=list(pres2014_round1$COD_MUN_IBGE), FUN=sum)
head(pres2014_mun); nrow(pres2014_mun)
colnames(pres2014_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# Aécio
pres2014_aecio<-as.data.frame( # Votes for Aecio
pres2014_round1[pres2014_round1 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "NOME_MUNICIPIO", "UF", "NOME_MACRO", "COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2014_aecio); nrow(pres2014_aecio); colnames(pres2014_aecio)[6]<-"votos_aecio"

# Dilma 
pres2014_dilma<-as.data.frame( # Votes for Dilma
pres2014_round1[pres2014_round1 $NUMERO_CANDIDATO==13,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2014_dilma); nrow(pres2014_dilma); colnames(pres2014_dilma)[2]<-"votos_dilma"

pres2014_aecio[!pres2014_aecio$COD_MUN_IBGE %in% pres2014_dilma$COD_MUN_IBGE,] 
# In what municipality did Dilma not have votes?
pres2014_dilma[nrow(pres2014_dilma)+1,]<-c(29297, 0) # Creating the extra municipality

# Marina
pres2014_marina<-as.data.frame( # Votes for Marina
pres2014_round1[pres2014_round1$NUMERO_CANDIDATO==40,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2014_marina); nrow(pres2014_marina); colnames(pres2014_marina)[2]<-"votos_marina"

# Invalid votes #-------------------------------------------------------

pres2014_inv<-as.data.frame( # Votes - Invalid
pres2014_round1[pres2014_round1$NUMERO_CANDIDATO==95,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2014_inv); nrow(pres2014_inv); colnames(pres2014_inv)[2]<-"votos_inv"

# In what municipality there was not invalid votes?
mis<-pres2014_aecio[!pres2014_aecio$COD_MUN_IBGE %in% pres2014_inv$COD_MUN_IBGE,] 
pres2014_inv<-rbind(pres2014_inv,
data.frame(COD_MUN_IBGE=mis[,1], votos_inv=rep(0, nrow(mis))))

pres2014_inv2<-as.data.frame( # Votes - Invalid
pres2014_round1[pres2014_round1$NUMERO_CANDIDATO==96,c("COD_MUN_IBGE", "QTDE_VOTOS")])
head(pres2014_inv2); nrow(pres2014_inv2); colnames(pres2014_inv2)[2]<-"votos_inv2"

pres2014_inv<-merge(pres2014_inv, pres2014_inv2, by="COD_MUN_IBGE", all=T)
pres2014_inv$votos_inv<-pres2014_inv$votos_inv+pres2014_inv$votos_inv2
pres2014_inv$votos_inv2<-NULL
pres2014_inv$votos_inv[is.na(pres2014_inv$votos_inv)]<-0

# Downloading senator votes --------------------------------------------------------- 
senate2014<- get_elections(2014, "Senator", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

senate2014_municipality<-senate2014[, c("QTDE_VOTOS")] # Subsetting
head(senate2014_municipality); nrow(senate2014_municipality)

senate2014_mun<-aggregate(senate2014_municipality, by=list(senate2014$COD_MUN_IBGE), FUN=sum)
head(senate2014_mun); nrow(senate2014_mun)
colnames(senate2014_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
senate2014_PSDB<-as.data.frame( # Votes for PSDB Senators
  senate2014[senate2014 $NUMERO_PARTIDO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2014_PSDB); nrow(senate2014_PSDB); colnames(senate2014_PSDB)[4]<-"votos_PSDB_sen"

# PT
senate2014_PT<-as.data.frame( # Votes for PT Senators
  senate2014[senate2014 $NUMERO_PARTIDO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2014_PT); nrow(senate2014_PT); colnames(senate2014_PT)[4]<-"votos_PT_sen"

# PSB
senate2014_PSB<-as.data.frame( # Votes for PSB Senators
  senate2014[senate2014 $NUMERO_PARTIDO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(senate2014_PSB); nrow(senate2014_PSB); colnames(senate2014_PSB)[4]<-"votos_PSB_sen"

# Invalid votes for Senator
senate2014_invalid<-get_elections(2014, "Senator", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
senate2014_invalid<-senate2014_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS, QT_VOTOS_NOMINAIS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_sen=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_sen=QT_VOTOS_NOMINAIS)

# Downloading legislative federal votes -------------------------------------------  
DF2014<- get_votes(2014, "Deputado Federal", regional_aggregation = "Municipality") %>% mutate(COD_MUN_TSE=as.integer(COD_MUN_TSE))  # Electoral Data

DF2014_municipality<-DF2014[, c("QTDE_VOTOS")] # Subsetting
head(DF2014_municipality); nrow(DF2014_municipality)

DF2014_mun<-aggregate(DF2014_municipality, by=list(DF2014$COD_MUN_IBGE), FUN=sum)
head(DF2014_mun); nrow(DF2014_mun)
colnames(DF2014_mun)<-c("COD_MUN_IBGE", "QTDE_VOTOS") # Total number of votes

# PSDB
DF2014_PSDB<-as.data.frame( # Votes for PSDB Senators
  DF2014[DF2014 $NUMERO_CANDIDATO==45,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2014_PSDB); nrow(DF2014_PSDB); colnames(DF2014_PSDB)[4]<-"votos_PSDB_PLV"

# PT
DF2014_PT<-as.data.frame( # Votes for PT Senators
  DF2014[DF2014 $NUMERO_CANDIDATO==13,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2014_PT); nrow(DF2014_PT); colnames(DF2014_PT)[4]<-"votos_PT_PLV"

# PSB
DF2014_PSB<-as.data.frame( # Votes for PSB Senators
  DF2014[DF2014 $NUMERO_CANDIDATO==40,c("COD_MUN_TSE", "UF","COD_MUN_IBGE", "QTDE_VOTOS")])
head(DF2014_PSB); nrow(DF2014_PSB); colnames(DF2014_PSB)[4]<-"votos_PSB_PLV"

# Invalid votes for Senator
DF2014_invalid<-get_elections(2014, "Deputado Federal", regional_aggregation = "Municipality", political_aggregation = "Consolidated")
DF2014_invalid<-DF2014_invalid %>% select(COD_MUN_TSE, UF, QT_VOTOS_NOMINAIS, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS) %>% 
  group_by(COD_MUN_TSE, UF) %>% 
  summarise_at(c("QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS","QT_VOTOS_NOMINAIS"), funs(sum)) %>% mutate(votos_inv_DF=(QT_VOTOS_BRANCOS+QT_VOTOS_NULOS)) %>% 
  select(-QT_VOTOS_NULOS, -QT_VOTOS_BRANCOS) %>% rename(QT_VOTOS_DF=QT_VOTOS_NOMINAIS)

rm(DF2014)

# Now, let's merge the presidential votes datafiles #-----------------------------

data_politics<-merge(pres2014_aecio, pres2014_dilma, by="COD_MUN_IBGE", all=T) %>% filter(UF!="ZZ")
data_politics<-left_join(data_politics, pres2014_marina, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2014_inv, by="COD_MUN_IBGE")
data_politics<-left_join(data_politics, pres2014_mun, by="COD_MUN_IBGE"); head(data_politics)

data_politics$votos_aecio_pc<-100*data_politics$votos_aecio/data_politics$QTDE_VOTOS
data_politics$votos_dilma_pc<-100*data_politics$votos_dilma/data_politics$QTDE_VOTOS # Percentages
data_politics$votos_marina_pc<-100*data_politics$votos_marina/data_politics$QTDE_VOTOS
data_politics$votos_inv_pc<-100*data_politics$votos_inv/data_politics$QTDE_VOTOS; head(data_politics)

head(data_politics)

# Now, let's merge the senator votes datafiles #-----------------------------
data_politics<-left_join(data_politics, senate2014_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2014_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2014_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, senate2014_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

#sub NA per 0
data_politics$votos_PSDB_sen<-ifelse(is.na(data_politics$votos_PSDB_sen),0,data_politics$votos_PSDB_sen)
data_politics$votos_PT_sen<-ifelse(is.na(data_politics$votos_PT_sen),0,data_politics$votos_PT_sen)
data_politics$votos_PSB_sen<-ifelse(is.na(data_politics$votos_PSB_sen),0,data_politics$votos_PSB_sen)

data_politics$votos_PSDB_sen_pc<-100*data_politics$votos_PSDB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_PT_sen_pc<-100*data_politics$votos_PT_sen/data_politics$QT_VOTOS_sen # Percentages
data_politics$votos_PSB_sen_pc<-100*data_politics$votos_PSB_sen/data_politics$QT_VOTOS_sen
data_politics$votos_inv_sen_pc<-100*data_politics$votos_inv_sen/data_politics$QT_VOTOS_sen; head(data_politics)

# Now, let's merge the Federal Deputy votes datafiles #-----------------------------
data_politics<-left_join(data_politics, DF2014_PSDB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2014_PT, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2014_PSB, by=c("COD_MUN_IBGE", "COD_MUN_TSE", "UF") , all=T) 
data_politics<-left_join(data_politics, DF2014_invalid, by=c("COD_MUN_TSE", "UF") , all=T) 

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

data_politics %>% mutate(regiao = substr(COD_MUN_IBGE,1,1)) %>% 
  select(regiao, COD_MUN_IBGE, votos_PT_PLV_pc, votos_PSDB_PLV_pc, votos_PSB_PLV_pc) %>% 
  gather(variavel, valor, 3:5) %>% 
  ggplot(aes(x = regiao,  y=valor)) + 
  geom_boxplot(aes(fill=variavel))
#grande varição entre regiões com PT mantendo votação no partido mais constante -
#PSDB tem maior nível de votação na legenda no sudeste, não igualado por nenhum partido em outros estados


df<-merge(data, data_politics, by = c("COD_MUN_TSE", "UF"))
head(df); nrow(df); table(df$UF.x, df$UF.y)

df<-df[order(df$pair),]
head(df)
df[df$pair==127 | df$pair==4921,] # Pair where the Lagoa Mirim was the main municipality
df<-df[df$pair!=127 & df$pair!=4921,] # Eliminating it from the dataset

#pra que isso?
df$votos_dilma_pc_ref<-NA
df$votos_aecio_pc_ref<-NA
df$votos_inv_pc_ref<-NA
df$same_state<-NA

for(p in unique(df$pair)){
for(o in unique(df$order)){
	df$votos_dilma_pc_ref[df$pair==p & df$order==o]<-
	df$votos_dilma_pc[df$pair==p & df$order==o]-df$votos_dilma_pc[df$pair==p & df$order==1]
	df$votos_aecio_pc_ref[df$pair==p & df$order==o]<-
	df$votos_aecio_pc[df$pair==p & df$order==o]-df$votos_aecio_pc[df$pair==p & df$order==1]
	df$votos_inv_pc_ref[df$pair==p & df$order==o]<-
	df$votos_inv_pc[df$pair==p & df$order==o]-df$votos_inv_pc[df$pair==p & df$order==1]
	
	df$same_state[df$pair==p & df$order==o]<-df$UF[df$pair==p & df$order==1]==df$UF[df$pair==p & df$order==o]
}}

write.csv(df, "Data/BR_mun_neighbors_polvars.csv", fileEncoding="latin1") # Neighboring Municipality 

