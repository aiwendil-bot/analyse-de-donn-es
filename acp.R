#library(explor) #données ACP
#library(shiny) #visualisation
#library(FactoMineR)
#library(VIM)
#library(funModeling)

dataset <-read.csv("Table_Ciqual.csv", sep=';',fileEncoding="latin1", header=TRUE,dec=",",na.strings = "-") #import table

dataset.fromages <- subset(dataset, alim_ssgrp_nom_fr == "fromages et assimilés") #restriction aux fromages et assimilés

#enlever données manquantes et non quantitatives

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
    numeros = as.character(k)
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


# Matrice des Valeurs propres, Variances (%) et Variances cumulées (%)
ACP_vp = function(X){
  ACPvp = matrix(0,r(X),3)
  ACPvp[,1] = round(ValP(X),3)
  ACPvp[,2] = round((ValP(X)/sum(ValP(X)))*100,2)
  ACPvp[,3] = round(cumsum((ValP(X)/sum(ValP(X)))*100),2)
  row.names(ACPvp) = numeros(X)[1:r(X)]
  colnames(ACPvp) = c("Valeurs Propres","Variances(%)","Variances cumulées(%)")
  return(ACPvp)
}

# Eboulis des valeurs propres
ACPgvp = function(X){
  # Diagramme des Variances en % de chaque composantes
  barplot(ACP_vp(X)[,2], names.arg = numeros(X)[1:r(X)], col = "blue", main = "Eboulis des valeurs propres", xlab = "Dimensions",ylab = "Inertie (%)", ylim = c(0,100))
  # Courbe des variances cumulées
  lines(ACPvp(X)[,3], col = 'red', lty = 1, lwd = 3)
  # Définition d'un troisième axe pour la courbe des variance cumulées
  axis(side = 4, col = 'red')
  mtext("Inerties cumulées (%)", side=4, line = -2, col = 'green')
  # Droite qui indique les composantes principales dont les valeurs propres associées respectent le critère de Kaiser
  abline(h = 10, lty = 2, lwd = 3, col = 'green3')
  text(9,8, label = "Critère de Kaiser", col = 'red')
}

# Défini le nombre de valeurs propres respectant le critère de Kaiser (val.propre >1)
# Permet de savoir jusqu'à quelle composante on va
kaiser = function (X){
  return (length(ValP(X)[ValP(X)>1]))
}
