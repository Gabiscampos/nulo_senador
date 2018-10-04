#Trabalho final de econometria - análise de efeito da lei de candidaturas femininas
#no total de candidatas viáveis

library(cepespR) #baixar dados cepesp
library(tidyverse) #tratamento de dados
library(dplyr) #tratamento de dados

#baixar dados de eleição de deputado estadual

columns <- list("ANO_ELEICAO", "NUMERO_CANDIDATO", "NUMERO_PARTIDO","UF","QTDE_VOTOS","CPF_CANDIDATO", "CODIGO_SEXO", "DESCRICAO_SEXO","DES_SITUACAO_CANDIDATURA", "DESC_SIT_TOT_TURNO", "TIPO_LEGENDA", "CODIGO_LEGENDA")

DE_2014 <- get_elections(year = 2014, position=7, regional_aggregation="State", columns_list=columns)
DE_2010 <- get_elections(year = 2010, position=7, regional_aggregation="State", columns_list=columns)
DE_2006 <- get_elections(year = 2006, position=7, regional_aggregation="State", columns_list=columns)
DE_2002 <- get_elections(year = 2002, position=7, regional_aggregation="State", columns_list=columns)

dados_DE<- rbind(DE_2014, DE_2010, DE_2006, DE_2002)
rm(DE_2014, DE_2010, DE_2006, DE_2002)

dados_DE$ID_PARTIDO_UF<- paste0(dados_DE.NUMERO_PARTIDO, dados_DE.UF, sep="_")
