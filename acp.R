#library(explor) #données ACP
#library(shiny) #visualisation
#library(FactoMineR)
#library(VIM)
#library(funModeling)
library(missMDA)
library(plotrix)
dataset <-read.csv("/home/adrien/analyse de données/Table_Ciqual.csv", sep=';',fileEncoding="latin1", header=TRUE,dec=",",na.strings = c("-","")) #import table
dataset.fromages <- subset(dataset, alim_ssgrp_nom_fr == "fromages et assimilés") #restriction aux fromages et assimilés
#enlever fromages avec trop de données manquantes (>50%) et variables non quantitatives & vitamine K2
dataset.fromages <- dataset.fromages[,c(-9:-1,-30,-68)] #non quantitatives, vitamine K2 et alcool (que des 0)

dataset.fromages.clean <- dataset.fromages[c(-2, -7, -8, -12, -14, -15, -29, -36, -39, -40, -57, -63, -65, -79, -88, -90, -94, -97, -99, -104, -107, -108, -109),]
#summary(dataset.fromages.clean)
#nb = estim_ncpPCA(dataset.fromages.clean)
#nb

#sapply(dataset,class)
#dim(dataset.fromages.clean)
dataset.fromages.clean.impute <- imputePCA(dataset.fromages.clean)[[2]]
colnames(dataset.fromages.clean.impute) = colnames(dataset.fromages.clean )
rownames(dataset.fromages.clean.impute) = rownames(dataset.fromages.clean )
#dataset.fromages.clean.impute
#ACP FactoMiner
#pca=PCA(dataset.fromages.clean)
#explor(pca) #visualisation de l'acp

#ACP

# définir D en fonction de X (cours)
D = function(X){
  return(diag(dim(X)[1])/(dim(X)[1]-1))
}

# définir la matrice de covariance S d'une matrice X
S = function(X){
  X = as.matrix(X)
  X = scale(X)
  return( t(X) %*% D(X) %*% X)
}


# Fonction pour définir la matrice de corrélation R d'une matrice X
R = function(X){
  X = as.matrix(X)
  X = scale(X)
  return( t(X) %*% D(X) %*% X)
}

# Définition d'un vecteur qui servira à numeroter les colonnes/lignes correspondant aux dimensions dans les matrices
numeros = function (X){
  numeros = rep(c('0'), max(dim(X)[1],dim(X)[2]))
  for (k in (1:max(dim(X)[1],dim(X)[2]))){
    numeros[k] = as.character(k)
  }
  return (numeros)
}

#valeurs propres

vp = function(X){
  valeurs = eigen(R(X))[[1]]
  return(valeurs[round(valeurs,10)!=0])
}

nb_vp = function(X){
  return (length(vp(X)))
}

vectp = function(X){
  return(eigen(R(X))[[2]][,1:nb_vp(X)])
}


# Matrice des Valeurs propres, Variances (%) et Variances cumulées (%)
ACP_vp = function(X){
  ACPvp = matrix(0,nb_vp(X),3)
  ACPvp[,1] = round(vp(X),3)
  ACPvp[,2] = round((vp(X)/sum(vp(X)))*100,2)
  ACPvp[,3] = round(cumsum((vp(X)/sum(vp(X)))*100),2)
  row.names(ACPvp) = numeros(X)[1:nb_vp(X)]
  colnames(ACPvp) = c("Valeurs Propres","Variances(%)","Variances cumulées(%)")
  return(ACPvp)
}

# Eboulis des valeurs propres
eboulis_vp = function(X){
  # Diagramme des Variances en % de chaque composantes
  barplot(ACP_vp(X)[,2], names.arg = numeros(X)[1:nb_vp(X)], col = "blue", main = "Eboulis des valeurs propres", xlab = "Dimensions",ylab = "Inertie (%)", ylim = c(0,100))
}

# Défini le nombre de valeurs propres respectant le critère de Kaiser (val.propre >1)
# Permet de savoir jusqu'à quelle composante on va
kaiser = function (X){
  return (length(ValP(X)[ValP(X)>1]))
}

# Variables (coordonnées puis cercle des corrélations)

coordonnees_variables = function(X){
  G = matrix(0,dim(X)[2],nb_vp(X))
  for (k in (1:nb_vp(X))){
    Gk = sqrt(vp(X)[k])*vectp(X)[,k]
    G[,k] = Gk
  }
  row.names(G)= colnames(X)
  colnames(G) = numeros(X)[1:nb_vp(X)]
  return(G)
}

cercle_correlations = function(X){
  plot.new()
  dev.new(width = 40,
        height = 40)
  plot(c(-1:1),c(-1:1),type="n",xlab=paste("Dim.1 (",ACP_vp(X)[1,2],"%)"),ylab=paste("Dim.2 (",ACP_vp(X)[2,2],"%)"),asp=1)
  grid()
  title("Cercle de corrélation des variables")
  draw.circle(0,0,1,lwd=2)
  abline(h = 0,lty=2)
  abline(v = 0,lty=2)
  arrows(0,0,-coordonnees_variables(X)[,1],coordonnees_variables(X)[,2],code=2,length=0.1,angle = 20)
  text(-coordonnees_variables(X)[,1],coordonnees_variables(X)[,2],colnames(X),cex=1,col="black")
}


# individus (coordonnées et graphes)

F = function(X){
  X <- scale(X)
  Ft = X %*% vectp(X)
  colnames(Ft) = numeros(X)[1:nb_vp(X)]
  return(Ft)
}

F(dataset.fromages.clean.impute)
