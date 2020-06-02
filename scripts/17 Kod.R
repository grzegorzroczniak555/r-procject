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
dir.create(preprocessedDir)
writeCorpus(corpus, path = preprocessedDir)