irisCopia <- iris

#armazenar media
medias <- matrix(nrow = 150, ncol = 2)

for (lin in 1:150) {

  medias[lin,1] <- sum(irisCopia[lin,1:2])/2
  medias[lin,2] <- sum(irisCopia[lin,3:4])/2
}
plot(medias)


###################Ligação média e Distância Euclidiana###################
#DISTANCIA
distancia <- dist(medias, diag = TRUE, upper = NULL, method = "euclidean")

#DENDOGRAMA
dendrograma <- hclust(dist(distancia), method="average")
plot(dendrograma,  xlim = c(1,500))


###################Ligação média e Distância Euclidiana###################
#DISTANCIA
distancia <- dist(medias, diag = TRUE, upper = NULL, method = "pearson")

#DENDOGRAMA
dendrograma <- hclust(dist(distancia), method="average")
plot(dendrograma,  xlim = c(1,500))
