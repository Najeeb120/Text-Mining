# Text-Mining
Document Clustering on NFL DataSet

******NFL DataSet Project******

Logic Implementation & Explanation


Step1:-   Set the working directory and import the dataset .csv file into the R environment

Step2:-   Load the text mining package for cleaning operations on dataset

                     library(tm)

Step3:-  Create the Corpus of the text
             msg_corpus <- Corpus(VectorSource(data$content))

Step4:-   Perform  Data cleansing operations
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

Step5:-   Inspect the corpus after cleaning operations
     i.e using the inspect function

Step6:- Create a DocumentTermMatrix
            dtm <- DocumentTermMatrix(mod_corpus)

Step7:- Determine the Term Frequency and tfxidf  
    
Step8:-Use kmeans  to do clustering
             m <- as.matrix(dtm_tfxidf)
             rownames(m) <- 1:nrow(m)

Step9:-  Normalize the vectors
            norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
            m_norm <- norm_eucl(m)

Step10:- Cluster the data into 10 clusters
              set.seed(100)
              km <- kmeans(m_norm, 10)
              table(km$cluster)

Step11:- Create Cluster_out.csv 
    x <- data.frame(Log = data_NFL$content, Cluster = km$cluster)
   write.csv(x, file = "D:/Data Science project/ClusterOut.csv", row.names=TRUE)

    data1 <- read.csv("D:/Data Science project/ClusterOut.csv")

Step12:-#Find top 5 words in 10 clusters

 *Craete a for loop to subset for the records belonging to each cluster i.e assign all the records of a particular cluster into avariable

 *apply TermDocumentMatrix on each subset

 *Inspect the elements in it

 *Find thecountof the words

 *Then output the LogGroup,LogCount,Top Words,WordCount,Counter into a file.
  
Below is the code for the same
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

then Bind both data frames using rbind()

Step13:- Output top 5 words in each cluster to Topwords.csv

 write.csv(x, file = "D:/Data Science project/Topwords.csv", row.names=FALSE)

Step14:- Perform Visualization using word cloud package


       library(wordcloud)

      for (k in 1:10)
      {
              print(c("WordCount for Cluster", k), quote=FALSE)
              capture.output( file = "D:/Data Science project/tests.doc", append = TRUE)
              colors <- brewer.pal(8,"Dark2")
              wordcloud(mod_corpus[km$cluster==k],  min.freq=5, 
              max.words=20,random.order=FALSE)
    
                                        }
    
      this displays word count for each cluster.
