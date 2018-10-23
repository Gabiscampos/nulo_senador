#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Creating a Dataset for voting in PT by municipality
# October 16, 2018

library(dplyr)
library(tidyverse)
library(tibble)
library(data.table)
library(pastecs)
library(cepespR)
library(ggplot2)
library(Zelig)

#Script for data download - source: Cepesp Data ------------------------------------------

#Downloand PT voting for president
voto_pres<-get_votes(2014, "Presidente", regional_aggregation = "Municipio")

voto_pres<-voto_pres %>% group_by(COD_MUN_IBGE) %>% mutate(vots_mun=sum(QTDE_VOTOS)) %>% 
  mutate(perc_votos_presPT=QTDE_VOTOS/vots_mun) %>% filter(NUMERO_CANDIDATO==13) %>% 
  filter(NUM_TURNO==1) %>% select(COD_MUN_IBGE, perc_votos_presPT, UF)

#Download data of federal legislative voting for PT and party voting for other parties
voto_DF<-get_votes(2014, "Deputado Federal", regional_aggregation = "Municipio")

voto_DF<-voto_DF %>% group_by(COD_MUN_IBGE) %>% mutate(vots_mun=sum(QTDE_VOTOS)) %>% 
  ungroup() %>% 
  mutate(vote_type=ifelse(str_length(NUMERO_CANDIDATO)==2, "Party Vote", "Candidate")) %>% 
  mutate(PT=ifelse(substr(NUMERO_CANDIDATO,1,2)==13, "PT", "Other Party")) %>% 
  group_by(COD_MUN_IBGE, vote_type, PT, vots_mun) %>% 
  summarise(votos=sum(QTDE_VOTOS)) %>% 
  mutate(perc_votos=votos/vots_mun) %>%  
  spread(PT, perc_votos) %>% group_by(COD_MUN_IBGE, vote_type) %>%
  summarise_at(c("Other Party", "PT"), sum, na.rm = T) %>% 
  spread(vote_type, PT) %>%
  mutate(`Other Party`=ifelse(is.na(Candidate), `Other Party`,0)) %>% ungroup() %>% 
  group_by(COD_MUN_IBGE) %>% 
  summarise_all(sum, na.rm=TRUE) %>% 
  rename(PT_candidate = Candidate, 
         PT_party_vote = `Party Vote`, 
         Other_party_vote = `Other Party`)

#Merge data frames 
perc_vote<-left_join(voto_pres, voto_DF, by = "COD_MUN_IBGE") %>% 
  mutate(regiao=substr(COD_MUN_IBGE,1,1)) %>% 
  filter(str_length(COD_MUN_IBGE)>6)
perc_vote<-perc_vote %>% mutate(COD_MUN_IBGE=as.character(COD_MUN_IBGE))
rm(voto_pres, voto_DF)

ggplot(perc_vote, aes(x=perc_votos_presPT, y=PT_party_vote)) +
  geom_point(shape=1) + stat_smooth(method=lm)

#Merge voting data with municipality neighbourhood information
load("mun_pairs.RData")

mun_pairs<-left_join(mun_pairs, perc_vote, by = c("CD_GEOCMU"="COD_MUN_IBGE"))
rm(perc_vote)

#Analysis - pair comparison -------------------------------------------------------------------------------
#We are going to verify if the voting patterns are similar in neighbouring municipalities

#Municipality (order==1) versus rest of the pair
analysis_1<-mun_pairs %>% mutate(mun_base=ifelse(order==1,"Ref","Comp"))
analysis_1<-analysis_1 %>% group_by(pair, mun_base) %>% 
  summarise_at(vars(Other_party_vote, PT_candidate, PT_party_vote, perc_votos_presPT), mean)

tapply(analysis_1$Other_party_vote, analysis_1$mun_base, summary)
tapply(analysis_1$PT_candidate, analysis_1$mun_base, summary)
tapply(analysis_1$PT_party_vote, analysis_1$mun_base, summary)
tapply(analysis_1$perc_votos_presPT, analysis_1$mun_base, summary)


#Diference between order==1 and rest of the pair
myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

analysis_2<-myspread(analysis_1, mun_base, c("Other_party_vote", "PT_candidate", "PT_party_vote", "perc_votos_presPT"))
analysis_2$dif_Other_party_vote<-analysis_2$Ref_Other_party_vote - analysis_2$Comp_Other_party_vote
analysis_2$dif_PT_candidate<-analysis_2$Ref_PT_candidate - analysis_2$Comp_PT_candidate
analysis_2$dif_PT_party_vote<-analysis_2$Ref_PT_party_vote - analysis_2$Comp_PT_party_vote
analysis_2$dif_perc_votos_presPT<-analysis_2$Ref_perc_votos_presPT - analysis_2$Comp_perc_votos_presPT

(stat.desc(analysis_2[,c("dif_Other_party_vote", "dif_PT_party_vote", "dif_PT_candidate", "dif_perc_votos_presPT")]))

qplot(analysis_2$Ref_PT_party_vote, geom="histogram") 
qplot(analysis_2$dif_PT_party_vote, geom="histogram")+  geom_histogram() 

#Data visualization
analysis_3a<-analysis_1 %>% select(-pair) %>%  group_by(mun_base) %>% summarise_all(funs(mean, sd))

#Mean and standard deviation
ggplot(analysis_3, aes(x=mun_base, y=PT_party_vote_mean)) + 
  geom_errorbar(aes(ymin=PT_party_vote_mean-PT_party_vote_sd, ymax=PT_party_vote_mean+PT_party_vote_sd), width=.1) +
  #  geom_line() +
  geom_point() + ggtitle("PT party vote balance")

ggplot(analysis_3, aes(x=mun_base, y=perc_votos_presPT_mean)) + 
  geom_errorbar(aes(ymin=perc_votos_presPT_mean-perc_votos_presPT_sd, ymax=perc_votos_presPT_mean+perc_votos_presPT_sd), width=.1) +
  #  geom_line() +
  geom_point() + ggtitle("PT president vote balance")

##mean in pairs - Pt and Other Party Vote
analysis_2 %>% select(pair, Comp_PT_party_vote, Ref_PT_party_vote, Comp_Other_party_vote, Ref_Other_party_vote) %>% 
gather(pair) %>% 
ggplot(aes(x = factor(pair),  y=value)) + 
  geom_boxplot()

##difference in pairs - PT and Other party vote
analysis_2 %>% select(pair, dif_PT_party_vote, dif_Other_party_vote, dif_perc_votos_presPT) %>% 
  gather(pair) %>% 
  ggplot(aes(x = factor(pair),  y=value)) + 
  geom_boxplot()

#Analysis - pair comparison for interstate-------------------------------------------------------------------------------

#Municipality (order==1) versus rest of the pair
analysis_1<-mun_pairs %>% filter(two_states==TRUE) %>%  mutate(mun_base=ifelse(order==1,"Ref","Comp"))
analysis_1<-analysis_1 %>% group_by(pair, mun_base) %>% 
  summarise_at(vars(Other_party_vote, PT_candidate, PT_party_vote, perc_votos_presPT), mean)

tapply(analysis_1$Other_party_vote, analysis_1$mun_base, summary)
tapply(analysis_1$PT_candidate, analysis_1$mun_base, summary)
tapply(analysis_1$PT_party_vote, analysis_1$mun_base, summary)
tapply(analysis_1$perc_votos_presPT, analysis_1$mun_base, summary)


#Diference between order==1 and rest of the pair
analysis_2<-myspread(analysis_1, mun_base, c("Other_party_vote", "PT_candidate", "PT_party_vote", "perc_votos_presPT"))
analysis_2$dif_Other_party_vote<-analysis_2$Ref_Other_party_vote - analysis_2$Comp_Other_party_vote
analysis_2$dif_PT_candidate<-analysis_2$Ref_PT_candidate - analysis_2$Comp_PT_candidate
analysis_2$dif_PT_party_vote<-analysis_2$Ref_PT_party_vote - analysis_2$Comp_PT_party_vote
analysis_2$dif_perc_votos_presPT<-analysis_2$Ref_perc_votos_presPT - analysis_2$Comp_perc_votos_presPT

tibble(stat.desc(analysis_2[,c("dif_Other_party_vote", "dif_PT_party_vote", "dif_PT_candidate", "dif_perc_votos_presPT")]))

qplot(analysis_2$Ref_PT_party_vote, geom="histogram") 
qplot(analysis_2$dif_PT_party_vote, geom="histogram")+  geom_histogram() 

#Data visualization
analysis_3b<-analysis_1 %>% select(-pair) %>%  group_by(mun_base) %>% summarise_all(funs(mean, sd))

#Mean and standard deviation
ggplot(analysis_3, aes(x=mun_base, y=PT_party_vote_mean)) + 
  geom_errorbar(aes(ymin=PT_party_vote_mean-PT_party_vote_sd, ymax=PT_party_vote_mean+PT_party_vote_sd), width=.1) +
  #  geom_line() +
  geom_point() + ggtitle("PT party vote balance")

ggplot(analysis_3, aes(x=mun_base, y=perc_votos_presPT_mean)) + 
  geom_errorbar(aes(ymin=perc_votos_presPT_mean-perc_votos_presPT_sd, ymax=perc_votos_presPT_mean+perc_votos_presPT_sd), width=.1) +
  #  geom_line() +
  geom_point() + ggtitle("PT president vote balance")

##mean in pairs - Pt and Other Party Vote
analysis_2 %>% select(pair, Comp_PT_party_vote, Ref_PT_party_vote, Comp_Other_party_vote, Ref_Other_party_vote) %>% 
  gather(pair) %>% 
  ggplot(aes(x = factor(pair),  y=value)) + 
  geom_boxplot()

##difference in pairs - PT and Other party vote
analysis_2 %>% select(pair, dif_PT_party_vote, dif_Other_party_vote, dif_perc_votos_presPT) %>% 
  gather(pair) %>% 
  ggplot(aes(x = factor(pair),  y=value)) + 
  geom_boxplot()

#Null voting  for Senator per mucipality 1998 - 2014 --------------------------------------------------
rm(list = ls())

#download data
anos<-c(1998, 2002, 2006, 2010, 2014)

for(j in anos){
  print(j)
  x<-get_elections(j, "Senador", regional_aggregation = "Municipality", political_aggregation = "Consolidado")
  if(j==1998){elections_senador<-x}else{elections_senador<-rbind(elections_senador,x)}
  rm(x)
}

#check number of municipalities
table(elections_senador$ANO_ELEICAO)

#percentage of null voting versus total
elections_senador$perc_null<- 1 - (elections_senador$QT_VOTOS_NOMINAIS/elections_senador$QTD_COMPARECIMENTO)
elections_senador<-elections_senador %>% select(COD_MUN_TSE, NOME_MUNICIPIO, UF, ANO_ELEICAO, perc_null) %>% mutate(ANO_ELEICAO=as.character(ANO_ELEICAO))
elections_senador<-elections_senador %>% spread(ANO_ELEICAO, perc_null)

lm(`2014` ~ `2006`, elections_senador)

#now we are going to mark which states had senator with the same party as the president
for(j in anos){
  print(j)
  x<-get_elections(j, "Senador", regional_aggregation = "State", political_aggregation = "Candidato")
  if(j==1998){voting_senador<-x}else{voting_senador<-rbind(voting_senador,x)}
  rm(x)
}

table(voting_senador$ANO_ELEICAO)

voting_senador <- voting_senador %>% group_by(ANO_ELEICAO, UF, NUMERO_PARTIDO, NUMERO_CANDIDATO) %>% 
  summarise(QTDE_VOTOS=sum(QTDE_VOTOS)) %>% ungroup() %>% group_by(ANO_ELEICAO,UF) %>% 
  mutate(VOTOS_UF=sum(QTDE_VOTOS)) %>% mutate(perc_vot_candidate=QTDE_VOTOS/VOTOS_UF)

#create the dummy for threatment states
tret_dummy<- voting_senador %>% group_by(ANO_ELEICAO, UF) %>%
  mutate(d1=ifelse(NUMERO_PARTIDO==13,1,0)) %>% 
  mutate(d2=ifelse(NUMERO_PARTIDO==13 | NUMERO_PARTIDO==45, 1, 0)) %>% 
  mutate(d3=ifelse(NUMERO_PARTIDO==13 & perc_vot_candidate==max(perc_vot_candidate),1,0)) %>% 
  mutate(d4=ifelse((NUMERO_PARTIDO==13 | NUMERO_PARTIDO==45)&perc_vot_candidate==max(perc_vot_candidate),1,0)) %>% 
  select(ANO_ELEICAO, UF, d1, d2, d3, d4) %>% ungroup() %>% 
  group_by(ANO_ELEICAO, UF) %>% summarise_all(funs(max))

#lets select only 2014 for the analysis
tret_dummy<-tret_dummy %>% filter(ANO_ELEICAO==2014)

table(tret_dummy$d1)
table(tret_dummy$d2)
table(tret_dummy$d3)
table(tret_dummy$d4)

tret_dummy %>% filter(d4==1)

elections_senador<-left_join(elections_senador, tret_dummy, by = "UF")

lm(`2014` ~ `2006`+d1, elections_senador)
