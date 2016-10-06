

#set the working directory
setwd("D:/Data Science project")


#import the dataset in R
data_NFL <- read.csv("NFL_SocialMedia_sample_data1.csv",head=T)
head(data_NFL)

#load the text mining package
library(tm)

# Build text corpus
txt_corpus <- Corpus(VectorSource(data_NFL$content))

#Inspect the corpus
inspect(txt_corpus[1:5])



#Perform Data Cleaning Operations
clean_corpus <- function(corpus)
  {
  corpus <- tm_map(corpus, stripWhitespace)
  #eliminate whitespace
  corpus <- tm_map(corpus, removePunctuation)
  #remove punctuation
  corpus <- tm_map(corpus, removeNumbers)
  #remove numbers
  corpus <- tm_map(corpus, tolower)
  #Convert to lowercase
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #Remove Stopwords
  return(corpus)
}


mod_corpus <- clean_corpus(txt_corpus)
inspect(mod_corpus[1:5])  #Inspect refined corpus

#Create Document term matrix
dtm <- DocumentTermMatrix(mod_corpus)
inspect(dtm[1:10,1000:1010])

##Determine the TF and tfxidf 
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10, 1000:1010])

#use kmeans 
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

#normalize the vectors
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)

##Cluster the data into 10 clusters
set.seed(100)
km <- kmeans(m_norm, 10)
table(km$cluster)

##Create Cluster_out.csv 
x <- data.frame(Log = data_NFL$content, Cluster = km$cluster)
write.csv(x, file = "D:/Data Science project/ClusterOut.csv", row.names=TRUE)

data1 <- read.csv("D:/Data Science project/ClusterOut.csv")



#Find top 5 words in 10 clusters

for(k in 1:length(km$withinss))
  
{
  wordstodocument <- TermDocumentMatrix(mod_corpus)
  wordsmatrix <-as.matrix(wordstodocument)
  word_freq<- sort(rowSums(wordsmatrix[km$cluster == k, ]),decreasing = TRUE)
  count <- as.vector(unlist(attributes(word_freq))) 
  freqdisp  <-data.frame(word_freq, count, stringsAsFactors = FALSE)

  
  
  if (k == 1)
    {
    x <- data.frame(k, length(which(data1$Cluster == k )), freqdisp$word_freq[1:5], 
                    freqdisp$count[1:5], as.numeric(rownames(x))[1:5])
    colnames(x) = c("Loggroup", "Logcount", "word Count", "Top Words", "Counter")
    
  } 
  
  else
    
    {
    y <- data.frame(k, length(which(data1$Cluster == k )), freqdisp$word_freq[1:5],
                    freqdisp$count[1:5], as.numeric(rownames(x))[1:5]) 
    colnames(y) = c("Loggroup", "Logcount", "word Count", "Top Words", "Counter")
    x <- rbind(x, y)
    }
  
}
  
#output top 5 words in each cluster to Topwords.csv
write.csv(x, file = "D:/Data Science project/Topwords.csv", row.names=FALSE)



#load wordcloud package
library(wordcloud)


##Perform visualization of clusters using wordcloud

  for (k in 1:10)
  {
    print(c("WordCount for Cluster", k), quote=FALSE)
    capture.output( file = "D:/Data Science project/tests.doc", append = TRUE)
    colors <- brewer.pal(8,"Dark2")
    wordcloud(mod_corpus[km$cluster==k],  min.freq=5, 
              max.words=20,random.order=FALSE)
    
  }
    



    



