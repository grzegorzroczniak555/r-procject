#biblioteki
library(tm)
library(hunspell)
library(stringr)

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
  "pca.png",
  sep = "\\"
)
png(filename = plotFile)
options(scipen = 5)
plot(
  x,
  y,
  col = "orange",
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2"
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
dev.off()
