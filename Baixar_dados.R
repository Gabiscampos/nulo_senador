#Vamos fazer uma análise da anulação de senador em comparação a outros cargos(legislativo e executivo) 
#a nível de município. O objetivo é entender se a relação observada a nivel de pais
#se repete no nível do município 

#Primeiro vamos baixar os dados do CepespData consolidados por município
library(cepespR)#baixar dados cepespData
library(dplyr)#tratamento de dados
library(tidyr)#tratamento de dados
library(ggplot2)#vizualição de dados

anos<-c(1998, 2002, 2006, 2010, 2014)

for(j in anos){
  print(j)
  x<-get_elections(j, "Presidente", regional_aggregation = "Municipality", political_aggregation = "Consolidado")
  if(j==1998){elections_presidente<-x}else{elections_presidente<-rbind(elections_presidente,x)}
  rm(x)
}

for(j in anos){
  print(j)
  x<-get_elections(j, "Senador", regional_aggregation = "Municipality", political_aggregation = "Consolidado")
  if(j==1998){elections_senador<-x}else{elections_senador<-rbind(elections_senador,x)}
  rm(x)
}

for(j in anos){
  print(j)
  x<-get_elections(j, "Governador", regional_aggregation = "Municipality", political_aggregation = "Consolidado")
  if(j==1998){elections_governador<-x}else{elections_governador<-rbind(elections_governador,x)}
  rm(x)
}

for(j in anos){
  print(j)
  x<-get_elections(j, "Deputado Estadual", regional_aggregation = "Municipality", political_aggregation = "Consolidado")
  if(j==1998){elections_DE<-x}else{elections_DE<-rbind(elections_DE,x)}
  rm(x)
}

for(j in anos){
  print(j)
  x<-get_elections(j, "Deputado Federal", regional_aggregation = "Municipality", political_aggregation = "Consolidado")
  if(j==1998){elections_DF<-x}else{elections_DF<-rbind(elections_DF,x)}
  rm(x)
}

#Agora vamos filtrar apenas o primeiro turno para as eleições majoritárias
elections_governador<-elections_governador %>% filter(NUM_TURNO==1)
elections_presidente<-elections_presidente %>% filter(NUM_TURNO==1) %>% filter(UF!="ZZ")

#Vamos juntar os bancos
base_nulo<-rbind(elections_DE, elections_DF, elections_senador, elections_governador, elections_presidente)
rm(elections_DE, elections_DF, elections_senador, elections_governador, elections_presidente)

#vamos calcular o percentual de nulos por ano-municipio-cargo
base_nulo$perc_nulo<-base_nulo$QT_VOTOS_NULOS/base_nulo$QTD_COMPARECIMENTO

#e vamos olhar só para o percentual de nulos por ano
#vamos tirar as observações do DF 
base_nulo_cand<- base_nulo %>% select(DESCRICAO_CARGO, NOME_MUNICIPIO, UF, COD_MUN_TSE, ANO_ELEICAO, 
                                 perc_nulo) %>% spread(DESCRICAO_CARGO, perc_nulo) %>% 
  select(-5) %>% filter(UF!="DF") %>% filter(ANO_ELEICAO==2014)

#Análise 1: diferença da anulação de voto para senador vs governador
base_nulo_cand$dif_senador<-(base_nulo_cand$SENADOR - base_nulo_cand$GOVERNADOR)

ggplot(base_nulo_cand, aes(x=SENADOR, y=GOVERNADOR)) +
  geom_point(shape=1) + geom_abline(intercept = 0, slope = 1, color="red") 

#Análise 2: diferença da anulação de voto para senador vs deputado federal
base_nulo_cand$dif_DF<-(base_nulo_cand$SENADOR - base_nulo_cand$`DEPUTADO FEDERAL`)

ggplot(base_nulo_cand, aes(x=SENADOR, y=`DEPUTADO FEDERAL`)) +
  geom_point(shape=1) + geom_abline(intercept = 0, slope = 1, color="red")  

ggplot(base_nulo_cand, aes(x=GOVERNADOR, y=`DEPUTADO FEDERAL`)) +
  geom_point(shape=1) + geom_abline(intercept = 0, slope = 1, color="red")  
