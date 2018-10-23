#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Matching neighboring cities by presidential votes
# October 22, 2018

# Clearing
rm(list=ls())

##############################
# Loading the neighbors data #
setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/") # Change HERE
data<-read.table("Data/BR_mun_neighbors.txt", encoding="latin1") # Neighboring Municipality Data
pres2014<- read.table("Data/presidential_2014municipality.txt") # Electoral Data

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

#################
# Invalid votes #
#################
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
#####################################################
# Now, let's merge the presidential votes datafiles #
#####################################################
data_pres<-merge(pres2014_aecio, pres2014_dilma, by="COD_MUN_IBGE", all=T) 
data_pres<-merge(data_pres, pres2014_marina, by="COD_MUN_IBGE", all=T)
data_pres<-merge(data_pres, pres2014_inv, by="COD_MUN_IBGE", all=T)
data_pres<-merge(data_pres, pres2014_mun, by="COD_MUN_IBGE", all=T); head(data_pres)

data_pres$votos_aecio_pc<-100*data_pres$votos_aecio/data_pres$QTDE_VOTOS
data_pres$votos_dilma_pc<-100*data_pres$votos_dilma/data_pres$QTDE_VOTOS # Percentages
data_pres$votos_marina_pc<-100*data_pres$votos_marina/data_pres$QTDE_VOTOS
data_pres$votos_inv_pc<-100*data_pres$votos_inv/data_pres$QTDE_VOTOS; head(data_pres)

head(data_pres)

df<-merge(data, data_pres, by.x="COD_MUN_TSE", by.y="COD_MUN_TSE")
head(df); nrow(df); table(df$UF.x, df$UF.y)
df$UF.y<-NULL; colnames(df)[which(names(df)=="UF.x")]<-"UF"

df<-df[order(df$pair),]
head(df)
df[df$pair==127 | df$pair==4921,] # Pair where the Lagoa Mirim was the main municipality
df<-df[df$pair!=127 & df$pair!=4921,] # Eliminating it from the dataset

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

write.table(df, "Data/BR_mun_neighbors_polvars.txt", fileEncoding="latin1") # Neighboring Municipality 
