#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Downloading Presidential 2014 Election Data from www.cepesp.io
# October 16, 2018

# Clearing
rm(list=ls())

##################################
# Packages and Working Directory #
packages<-c("cepespR"); lapply(packages, require, character.only=T) # install if needed
setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/")

pres2014<-get_votes(2014, "President", "Municipality"); head(pres2014)
pres2014<-pres2014[, -which(colnames(pres2014) %in% c("DESCRICAO_CARGO", "DESCRICAO_ELEICAO", "CODIGO_CARGO", "SIGLA_UE"))] # Subsetting
 
write.table(pres2014, "Data/presidential_2014_municipality.txt")
  