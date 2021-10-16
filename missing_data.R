#install.packages("funModeling")
#install.packages("VIM")
#install.packages("naniar")
#install.packages("explor")
#install.packages("shiny")
#install.packages("FactoMiner")

library(VIM)
library(naniar)
library(grid)
library(colorspace)

#VISUALISATION DES DONNEES MANQUANTES

table <-read.csv("/home/adrien/analyse de données/Table_Ciqual.csv", sep=';',fileEncoding="latin1",na.strings = c("-","")) #import table

fromages<-subset(table, alim_ssgrp_nom_fr == "fromages et assimilés") #restriction aux fromages et assimilés
fromages <- fromages[-1,] # On supprime la 1ere ligne (fromage "moyen")
rownames(fromages) <- 1:nrow(fromages)
#fromages[fromages=="-" ||fromages=="" ]<-NA # changement "-" -> NA (pour utiliser les fonctions qui suivent)

Etude_NA <- aggr(fromages,
                 col=c('navyblue','red'),
                 numbers=TRUE,
                 sortVars=TRUE,
                 labels=names(data),
                 cex.axis=.7, gap=3,
                 ylab=c("Histogram of missing data","Pattern"))


gg_miss_var(fromages,show_pct = TRUE) #assez parlant

NA_lignes <- apply(fromages,1,function(x) sum(is.na(x)))
NA_lignes <- NA_lignes /76
fromages_a_delete <- NA_lignes[NA_lignes > 0.5]
fromages_a_delete
#fromages_a_delete = 2 7 8 12 14 15 29 36 39 40 57 63 65 79 88 90 94 97 99 104 107 108 109
#23 fromages à supprimer
#et par lecture graphique, la variable vitamine K2
plot(rownames(fromages),NA_lignes,type="h")
