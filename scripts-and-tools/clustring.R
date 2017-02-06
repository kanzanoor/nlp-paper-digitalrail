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
  write.csv(m,file="Clustdtm.csv")
#shorten rownames for display purpose
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)), substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)

#run hierarchical clustering using Ward's method
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)

#cut into 2 subtrees - try 3 and 5
rect.hclust(groups,2)
d <- dist(m)
#k means algorithm, 2 clusters, 100 starting configurations
kfit <- kmeans(d, 2, nstart=100)
#plot - need library cluster
library(cluster)

clusplot(d, diss=TRUE, kfit$cluster, color=TRUE, shade = TRUE, lines=0)




