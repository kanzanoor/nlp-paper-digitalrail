library(tm)
#set working directory (modify path as needed)
setwd("C:\\Users\\polly\\Desktop\\Kanz\\Ranalysis\\entitiestxt")
getwd()

filenames <- list.files(getwd(),pattern="*txt")
files <- lapply(filenames,readLines)

#create corpus from vector
docs <- Corpus(VectorSource(files))
 
 
 #inspect a particular document in corpus
  writeLines(as.character(docs[[1]]))
  
  #Transform to lower case
  docs <- tm_map(docs,content_transformer(tolower))
  
  #remove potentiallyy problematic symbols
  toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
  
  
  #remove punctuation
  docs <- tm_map(docs, removePunctuation)
  
  #Strip digits
  docs <- tm_map(docs, removeNumbers)
  #remove stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  #remove whitespace
  docs <- tm_map(docs, stripWhitespace)
  writeLines(as.character(docs[[10]]))
  
  dtm <- DocumentTermMatrix(docs)
  #print a summary
  dtm
  
  #convert dtm to matrix
  
  
  #install.packages  ("bigmemory")
 # library (bigmemory)
  
  m= as.matrix(dtm)
  #write as csv file (optional)
  write.csv(m,file="netGraphdtm.csv")
  
  #Map filenames to matrix row numbers
  #these numbers will be used to reference
  #files in the network graph
  filekey <- cbind(rownames(m),filenames)
  
  #compute cosine similarity between document vectors
  #converting to distance matrix sets diagonal elements to 0
  install.packages('proxy') # Let's be honest, you've never heard of this before.
  library('proxy') # Library of similarity/dissimilarity measures for 'dist()'
  dist(m, method="cosine")
  
  cs <- cosineSim(m)  
 
  write.csv(as.matrix(cs),file="Ngraph.csv")
  #adjacency matrix: set entries below a certain threshold to 0.
  #We choose half the magnitude of the largest element of the matrix
  #as the cutoff. This is an arbitrary choice
  cs[cs < max(cs)/2] <- 0
  cs <- round(cs,3)
  write.csv(as.matrix(cs),file="AdjacencyMatrix.csv")
  