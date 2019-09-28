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
dendrograma <- hclust(dist(distancia), method="average")

plot(dendrograma,main = "Dendograma para dataset Iris" ,xlim = c(1,500),
     xlab="Distância Euclidiana")


###################Ligação média e Correlação de Pearson#########################
#DISTANCIA
distancia <- as.dist(1-cor(t(irisCopia[,1:4]), method="pearson"),diag = TRUE, upper = NULL)
#DENDOGRAMA
dendrograma <- hclust(dist(distancia), method="average")
plot(dendrograma,main = "Dendograma para dataset Iris" ,xlim = c(1,500),
     xlab="Correlação de Pearson")
