#load text mining library
library(tm)

#set working directory (modify path as needed)
setwd("C:\\Users\\polly\\Desktop\\Kanz\\Ranalysis\\Reportstxt")

getwd()

#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(),pattern="*.txt")


#read files into a character vector
files <- lapply(filenames,readLines)


#create corpus from vector
docs <- Corpus(VectorSource(files))

#inspect a particular document in corpus
writeLines(as.character(docs[[10]]))

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
library (bigmemory)

m=as.big.matrix(x=as.matrix(dtm))#convert the DTM into a bigmemory object using the bigmemory package 
m=as.matrix(m)

#write as csv file (optional)
write.csv(m,file="netGraphdtm.csv")

#Map filenames to matrix row numbers
#these numbers will be used to reference
#files in the network graph
filekey <- cbind(rownames(m),filenames)

#compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
#install.packages ("PANR")
cs <- cosineSim(m)
write.csv(as.matrix(cs),file="Ngraph.csv")
#adjacency matrix: set entries below a certain threshold to 0.
#We choose half the magnitude of the largest element of the matrix
#as the cutoff. This is an arbitrary choice
cs[cs < max(cs)/2] <- 0
cs <- round(cs,3)
write.csv(as.matrix(cs),file="AdjacencyMatrix.csv")