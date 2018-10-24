#### Gui Russo e Gabi Campos
### Invalid Votes for Senator- Error in the Ballot
## Creating a Dataset for Neighboring Municipalities: Brazil 
# October 16, 2018

# Clearing
rm(list=ls())

##################################
# Packages and Working Directory #
packages<-c("rgdal", "spdep", "cepespR"); lapply(packages, require, character.only=T) # install if needed

#setwd("/Users/robertaazzi1/Desktop/Nulo_Senador/") # Change HERE

###########################
# Acess to the shapefile: #
# https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais #
#shp<-readOGR("Shapefiles/br_municipios/BRMUE250GC_SIR.shp")
shp<-readOGR("br_municipios/BRMUE250GC_SIR.shp")

head(shp@data); nrow(shp@data)

# Creating the datasets of neighboring cities
neighbors<-poly2nb(shp)
length(neighbors); neighbors[[1]]

matching<-list(NULL)
for(i in 1:length(neighbors)){
	matching[[i]]<-rbind(
	shp@data[i,],
	shp@data[neighbors[[i]],])

	matching[[i]]<-cbind(matching[[i]],
	coordinates(shp[c(i, neighbors[[i]]), ]
	))
}

##############
# CepespData #
df<- read.table("Data/presidential_2014_municipality.txt"); names(df)

# Creating a dictionary of IBGE and TSE codes
vars<-c("COD_MUN_TSE", "UF", "COD_MUN_IBGE")
df_unique<-df[!duplicated(df[,vars]),vars]

################################
# Assigning UF to the matching #
for(i in 1:length(matching)){
	matching[[i]]$order<-seq(1, nrow(matching[[i]]))
	matching[[i]]<-merge(matching[[i]], df_unique, by.x="CD_GEOCMU", by.y="COD_MUN_IBGE")
	matching[[i]]<-matching[[i]][order(matching[[i]]$order),]
	matching[[i]]$pair<-i
}

################################################################
# Finding out which neighboring city borders a different state #
n_states<-rep(NA, length(matching))
for(i in 1:length(matching)){
	n_states[i]<-length(unique(matching[[i]]$UF))
	matching[[i]]$two_states<-n_states[i]>1
}	
two_states<-n_states>1
table(n_states); table(two_states)

matching[[which(n_states>3)[1]]] # First case the city borders three states

data<-do.call(rbind, matching)
head(data); nrow(data); table(data$two_states)

#write.table(data, "Data/BR_mun_neighbors.txt", fileEncoding="latin1")
write.csv(data, "Data/BR_mun_neighbors.csv")


