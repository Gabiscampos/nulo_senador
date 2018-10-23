#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## 
# October 22, 2018

# Clearing
rm(list=ls())

##############################
# Loading the neighbors data #
setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/") # Change HERE
data<- read.table("Data/BR_mun_neighbors_polvars.txt", encoding="latin1") # Neighboring Municipality 
head(data)

################################################################
### Let's compare municipalities with 1% difference in votes ###
data_1pc<-data[abs(data$votos_dilma_pc_ref)<1,]
table(data_1pc$same_state[data_1pc$order>1])

data_1pc<-data[abs(data$votos_aecio_pc_ref)<1,]
table(data_1pc$same_state[data_1pc$order>1])

data_1pc<-data[abs(data$votos_inv_pc_ref)<1,]
table(data_1pc$same_state[data_1pc$order>1])

data_1pc<-data[abs(data$votos_dilma_pc_ref)<1 &
abs(data$votos_aecio_pc_ref)<1 & abs(data$votos_inv_pc_ref)<1,]

table(data_1pc$same_state[data_1pc$order>1]==F)
pairs<-data_1pc$pair[data_1pc$same_state==F & data_1pc$order>1]
data_1pc[data_1pc$pair %in% pairs,c("NM_MUNICIP", "UF", "pair", "order", "X1", "X2")]

##############
### Now 2% ###
data_2pc<-data[abs(data$votos_dilma_pc_ref)<2 &
abs(data$votos_aecio_pc_ref)<2 & abs(data$votos_inv_pc_ref)<2,]

table(data_2pc$same_state[data_2pc$order>1]==F)
pairs<-data_2pc$pair[data_2pc$same_state==F & data_2pc$order>1]
data_2pc[data_2pc$pair %in% pairs,c("NM_MUNICIP", "UF", "pair", "order", "X1", "X2")]

###############################
### Now a mix of 1.5 and 1% ###
data_1.5pc<-data[abs(data$votos_dilma_pc_ref<)1.5 &
abs(data$votos_aecio_pc_ref)<1.5 & abs(data$votos_inv_pc_ref)<1,]

table(data_1.5pc$same_state[data_1.5pc$order>1]==F)
pairs<-data_1.5pc$pair[data_1.5pc$same_state==F & data_1.5pc$order>1]
data_1.5pc[data_1.5pc$pair %in% pairs,c("NM_MUNICIP", "UF", "pair", "order", "X1", "X2")]

###############################
### Now a mix of 2.5 and 2% ###
data_2.5pc<-data[abs(data$votos_dilma_pc_ref)<2.5 &
abs(data$votos_aecio_pc_ref)<2.5 & abs(data$votos_inv_pc_ref)<2,]

table(data_2.5pc$same_state[data_2.5pc$order>1]==F)
pairs<-data_2.5pc$pair[data_2.5pc$same_state==F & data_2.5pc$order>1]
data_2.5pc[data_2.5pc$pair %in% pairs,c("NM_MUNICIP", "UF", "pair", "order")]
