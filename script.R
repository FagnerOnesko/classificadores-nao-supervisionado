library(cluster)

irisCopia <- iris
irisNorm <- iris
###################Ligação média e Distância Euclidiana#########################

#normalizar
for(col in 1:4){
  for (lin in 1:150) {
    irisNorm[lin,col] <- (irisCopia[lin,col] - min(irisCopia[,col])) / (max(irisCopia[,col]) - min(irisCopia[,col]))
  }
}

#DISTANCIA
distancia <- dist(irisNorm[,1:4], diag = TRUE, upper = NULL, method = "euclidean")

#DENDOGRAMA
dendograma <- hclust(dist(distancia), method="average")

plot(dendograma,main = "Dendrograma para dataset Iris" ,xlim = c(1,500),
     xlab="Ligação média e Distância Euclidiana", sub="",cex.axis = 3 ,cex.lab=3, cex.main=3)


plot(dendograma,main = "Dendrograma para dataset Iris" ,xlim = c(1,500),
     xlab="Ligação média e Distância Euclidiana", sub="",cex.axis = 3 ,cex.lab=3, cex.main=3)
abline(h=3, lty=2, lwd=2, col = "#E31A1C")

#PUREZA
output_cluster<-pam(distancia,3)

classeSet = classeVir = classeVer= purezaEuc = matrix(nrow=1,ncol=3);
classeSet[] = classeVir[] = classeVer[] = 0;

for (lin in 1:150) {
  for (col in 1:3) {
    if(output_cluster$cluster[lin] == col){
      
      if(irisCopia[lin,5] == "setosa")
        classeSet[col] = classeSet[col]+1
      else if(irisCopia[lin,5] == "versicolor")
        classeVer[col] = classeVer[col]+1
      else 
        classeVir[col] = classeVir[col]+1;
    }
  }  
}

purezaEuc[1] <- max(classeSet)/sum(classeSet)
purezaEuc[2] <- max(classeVer)/sum(classeVer)
purezaEuc[3] <- max(classeVir)/sum(classeVir)

  ###################Ligação média e Correlação de Pearson#########################

#CORRELAÇÃO
distancia <- as.dist(1-cor(t(irisCopia[,1:4]), method="pearson"),diag = TRUE, upper = NULL)

#DENDOGRAMA
dendograma <- hclust(dist(distancia), method="average")

plot(dendograma,main = "Dendrograma para dataset Iris" ,xlim = c(1,500),
     xlab="Ligação Média e Correlação de Pearson",sub="",cex.axis = 3 ,cex.lab=3, cex.main=3)

plot(dendograma,main = "Dendrograma para dataset Iris" ,xlim = c(1,500),
     xlab="Ligação Média e Correlação de Pearson",sub="",cex.axis = 3 ,cex.lab=3, cex.main=3)

abline(h=1, lty=2, lwd=2, col = "#E31A1C")


#PUREZA

output_cluster2<- pam(distancia, 3)

classeSet = classeVir = classeVer= purezaPer = matrix(nrow=1,ncol=3);
classeSet[] = classeVir[] = classeVer[] = 0;

for (lin in 1:150) {
  for (col in 1:3) {
    if(output_cluster2$cluster[lin] == col){
      
      if(irisCopia[lin,5] == "setosa")
        classeSet[col] = classeSet[col]+1
      else if(irisCopia[lin,5] == "versicolor")
        classeVer[col] = classeVer[col]+1
      else 
        classeVir[col] = classeVir[col]+1;
    }
  }  
}

purezaPer[1] <- max(classeSet)/sum(classeSet)
purezaPer[2] <- max(classeVer)/sum(classeVer)
purezaPer[3] <- max(classeVir)/sum(classeVir)
