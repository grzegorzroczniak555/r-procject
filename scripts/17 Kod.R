#biblioteki
library(tm)
library(hunspell)
library(stringr)

library(lsa)
library(dendextend)
library(corrplot)
library(flexclust)
library(proxy)

#Katalogi funkcjonalne
inputDir <- ".\\data"
outputDir <- ".\\result"

#Korpus
#ścieżka
corpusDir <- paste(
  inputDir,
  "dokumenty-tekstowe",
  sep = "\\"
)
#tworzenie korpusu
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#przetwarzanie
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))

stoplistFile <- paste(
  inputDir,
  "stopwords_pl.txt",
  sep = "\\"
)
stoplist <- readLines(stoplistFile, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

#usunięcie rozszerzeń z nazw dokumentów
cutExtensions <- function(document, extension) {
  meta(document, "id") <- gsub(paste("\\.", extension, "$", sep=""),"", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions, "txt")

#usunięcie podziału na akapity
pasteParagraphs <- content_transformer(function(text, char) paste(text, collapse = char ))
corpus <- tm_map(corpus, pasteParagraphs, " ")

#lematyzacja
polish <- dictionary(lang="pl_PL")
lemmatize <- function(text) {
  simpleText <- str_trim(as.character(text))  #usunięcie białych znaków
  vectorizedText <- str_split(simpleText, pattern = " ") #podział na pojedyncze słowa
  lemmatizedText <- hunspell_stem(vectorizedText[[1]], dict = polish)
  for (i in 1:length(lemmatizedText)) {
    if(length(lemmatizedText[[i]]) == 0) lemmatizedText[i] <- vectorizedText[[1]][i]
    if(length(lemmatizedText[[i]]) >  1) lemmatizedText[i] <- lemmatizedText[[i]][1]
  }
  newText <- paste(lemmatizedText, collapse = " ")
  return(newText)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

#eksport do osobnego pliku po przetworzeniu
preprocessedDir <- paste(
  inputDir,
  "dokumenty-tekstowe-poPrzetworzeniu",
  sep = "\\"
)
writeCorpus(corpus, path = preprocessedDir)

#macierz częstości
#TDM - słowa jako wiersze, dokumenty jako kolumny
#bez parametrów
tdmTfAll <- TermDocumentMatrix(corpus)
#inverse document frequency
tdmTfidfAll <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf
  )
)
#zakres liczby dokumentów w których musi być spełniony warunek
tdmTfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(2,14)
    )
  )
)
#weightTfIdf + bounds
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,14)
    )
  )
)
# DTM - dokumenty jako wiersze, słowa jako kolumny
#bez parametrów
dtmTfAll <- DocumentTermMatrix(corpus)
#inverse document frequency
dtmTfidfAll <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf
  )
)
##weightTfIdf + bounds
dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,14)
    )
  )
)

#konwersja na macierz klasyczną (z macierzy rzadkich)
tdmTfAllMatrix <- as.matrix(tdmTfAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
dtmTfidfAllMatrix <- as.matrix(dtmTfidfAll)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBounds)

#eksport do pliku
matrixFile <- paste(
  outputDir,
  "tdmTfAll.csv",
  sep = "\\"
)
write.table(tdmTfAllMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(
  outputDir,
  "tdmTfidfAllMatrix.csv",
  sep = "\\"
)
write.table(tdmTfidfAllMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(
  outputDir,
  "tdmTfBoundsMatrix.csv",
  sep = "\\"
)
write.table(tdmTfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(
  outputDir,
  "tdmTfidfBoundsMatrix.csv",
  sep = "\\"
)
write.table(tdmTfidfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(
  outputDir,
  "dtmTfAllMatrix.csv",
  sep = "\\"
)
write.table(dtmTfAllMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(
  outputDir,
  "dtmTfidfAllMatrix.csv",
  sep = "\\"
)
write.table(dtmTfidfAllMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

matrixFile <- paste(
  outputDir,
  "dtmTfidfBoundsMatrix.csv",
  sep = "\\"
)
write.table(dtmTfidfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)




####
#### redukcja wymiarów macierzy częstości
####
pca <- prcomp(dtmTfidfBounds)

#dane
legend <- paste(paste("d", 1:19, sep = ""), rownames(dtmTfidfBounds), sep = ": ")
x <- pca$x[,1]
y <- pca$x[,2]


#wykres
options(scipen = 5)
plot(
  x,
  y,
  col = "orange",
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  xlim = c(-0.16,0.16),
  #ylim = c(,)
)
text(
  x,
  y, 
  paste("d", 1:19, sep = ""),
  col = "orange",
  pos = 4
)
legend(
  "bottom",
  legend,
  cex = 0.6,
  text.col = "orange"
)

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "wykres-glowne-skladowe.png",
  sep = "\\"
)
png(filename = plotFile)
options(scipen = 5)
plot(
  x,
  y,
  col = "green",
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2"
)
text(
  x,
  y, 
  paste("d", 1:19, sep = ""),
  col = "red",
  pos = 4
)

##par(xpd=TRUE)

legend(
  "bottomleft",
  legend,
  cex = 0.4,
  text.col = "blue"
)
dev.off()





######
## Dekompozycja według wartości osobliwych
######
lsa <- lsa(tdmTfBoundsMatrix)

#przygotowanie danych do wykresu
coordDocs <- lsa$dk%*%diag(lsa$sk)
coordTerms <- lsa$tk%*%diag(lsa$sk)
termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantTerms <- names(tail(sort(termsImportance),30))
coordImportantTerms <- coordTerms[importantTerms,]
legend <- paste(paste("d", 1:19, sep = ""), rownames(coordDocs), sep = ": ")
x1 <- coordDocs[,1]
y1 <- coordDocs[,2]
x2 <- coordImportantTerms[,1]
y2 <- coordImportantTerms[,2]

#wykres dokumentów w przestrzeni dwuwymiarowej
options(scipen = 5)
plot(
  x1,
  y1,
  col = "orange",
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "DS2",
  #xlim = c(-0.16,0.16),
  #ylim = c(,)
)
text(
  x1,
  y1, 
  paste("d", 1:19, sep = ""),
  col = "orange",
  pos = 4
)
points(
  x2,
  y2,
  pch = 2,
  col = "brown"
)
text(
  x2,
  y2,
  rownames(coordImportantTerms),
  col = "brown"
)
legend(
  "topleft",
  legend,
  cex = 0.6,
  text.col = "orange"
)

#zapis do pliku
plotFile <- paste(
  outputDir,
  "wykres-dekompozycja.png",
  sep = "\\"
)
png(filename = plotFile)
options(scipen = 5)
plot(
  x1,
  y1,
  col = "orange",
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "DS2",
  #xlim = c(-0.16,0.16),
  #ylim = c(,)
)
text(
  x1,
  y1, 
  paste("d", 1:19, sep = ""),
  col = "orange",
  pos = 4
)
points(
  x2,
  y2,
  pch = 2,
  col = "brown"
)
text(
  x2,
  y2,
  rownames(coordImportantTerms),
  col = "brown"
)
legend(
  "topleft",
  legend,
  cex = 0.6,
  text.col = "orange"
)
dev.off()


######
### analiza skupień dokumentów
######


par(mai = c(1,2,1,1))
###eksperyment 1
dist1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(dist1, method = "ward.D2")
plot(hclust1)
barplot(
  hclust1$height, 
  names.arg = 18:1, 
  col = "orange"
)
nClusters1 = 5
clusters1 <- cutree(hclust1, k = nClusters1)
clustersMatrix1 <- matrix(0, 19, nClusters1)
rownames(clustersMatrix1) <- names(clusters1)
for (i in 1:19) {
  clustersMatrix1[i,clusters1[i]] <- 1
}
corrplot(clustersMatrix1)
dendrogram1 <- as.dendrogram(hclust1)
coloredDendrogram1 <- color_branches(dendrogram1, h = 100)
plot(coloredDendrogram1)

###eksperyment 2
dist2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(dist2, method = "ward.D2")
plot(hclust2)
barplot(
  hclust2$height, 
  names.arg = 18:1, 
  col = "orange"
)
nClusters2 = 3
clusters2 <- cutree(hclust2, k = nClusters2)
clustersMatrix2 <- matrix(0, 19, nClusters2)
rownames(clustersMatrix2) <- names(clusters2)
for (i in 1:19) {
  clustersMatrix2[i,clusters2[i]] <- 1
}
corrplot(clustersMatrix2)
dendrogram2 <- as.dendrogram(hclust2)
coloredDendrogram2 <- color_branches(dendrogram2, h = 1.5)
plot(coloredDendrogram2)

###porównanie wyników eksperymentów
Bk_plot(
  dendrogram1,
  dendrogram2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Index Fawlks'a Mallows'a",
  ylab = "Index Fawlks'a Mallows'a"
)

##niehierarchiczna (k-średnich)

###eksperyment 3
nClusters3 <- 3
kmeans3 <- kmeans(dtmTfidfBounds, centers = nClusters3)
clustersMatrix3 <- matrix(0, 19, nClusters3)
rownames(clustersMatrix3) <- names(kmeans3$cluster)
for (i in 1:19) {
  clustersMatrix3[i,kmeans3$cluster[i]] <- 1
}
corrplot(clustersMatrix3)

pattern <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3)

##porównanie wyników klasyfikacji
randEx1Ex3 <- randIndex(clusters1, kmeans3$cluster, F)
randEx1Ex2 <- randIndex(clusters1, clusters2, F)

randEx1Pattern <- randIndex(clusters1, pattern, F)
randEx2Pattern <- randIndex(clusters2, pattern, F)


