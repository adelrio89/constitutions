#**************************************************************************
#                  Constitutions Text Analysis
#**************************************************************************

# install and load text analysis library
# install.packages("tm")
library(tm)

#---------------------------------------------------------------------------
#                  Setting up for processing
#---------------------------------------------------------------------------

#Load text files into the VCorpus
cons_corpus <- VCorpus(DirSource("/Users/andresdelrio/Desktop/TextAnalytics/Constitutions/data"), readerControl=list(language="eng", reader=readPlain))

# inspect corpus
inspect(cons_corpus[[1]])

# visualize meta data of the corpus
meta(cons_corpus[[1]])

#Function that prints information about the corpus
#----input: processing_step : description
#----input: corpus : the corpus to analyse
analyse_corpus <- function(processing_step, corpus){
  
  print("+++++++++++++++++++++++++++++++++++++")
  print(processing_step)
  print("+++++++++++++++++++++++++++++++++++++")
  
  # count the number of words and chars in the corpus. Then print them.
  for(doc in 1:length(corpus)){
    print(paste(corpus[[doc]]$meta$id, 
                "# of words = ",
                lengths(gregexpr("\\W+", corpus[[doc]]$content)) + 1,
                "# of characters = ",
                nchar(corpus[[doc]]$content)))
  }
  
  # print the first document in the corpus
  print("-------------------------------------")
  print(corpus[[1]]$content)
}

for(doc in 1:length(cons_corpus)){
  
  # Make content of every document in the corpus a vector of length 1
  cons_corpus[[doc]]$content <- paste(cons_corpus[[doc]]$content,collapse = " ")
  
}


# Print raw data of the corpus
analyse_corpus("Raw data",cons_corpus)

#---------------------------------------------------------------------------
#                  Cleaning Text
#---------------------------------------------------------------------------

#Convert to lower case
cons_corpus2 <- tm_map(cons_corpus, content_transformer(tolower))
analyse_corpus("Converted to lower case",cons_corpus2)

#Remove punctuation
cons_corpus3 <- tm_map(cons_corpus2, removePunctuation)
analyse_corpus("Punctuation removed",cons_corpus3)

#---------------------------------------------------------------------------
#                  Stopwords, Extra White Spaces, Numbers and Some words Removal
#---------------------------------------------------------------------------

#Remove stopwords
cons_corpus4 <- tm_map(cons_corpus3, removeWords, stopwords("en")) 

cons_corpus4 <- tm_map(cons_corpus4, stripWhitespace) 
cons_corpus4 <- tm_map(cons_corpus4, removeNumbers) 
cons_corpus4 <- tm_map(cons_corpus4, removeWords, c("shall","article","may","will","art","paragraph","subparagraph","chapter","heading","i","ii","iii","iv","v","vi","vii","viii","ix","x","xi","xii")) 

analyse_corpus("Stopwords removed",cons_corpus4) 

#---------------------------------------------------------------------------
#                  Stemming
#---------------------------------------------------------------------------

# install.packages("SnowballC")
#library(SnowballC)

#cons_corpus5 <- tm_map(cons_corpus4, stemDocument)
#analyse_corpus("Stemmed documents",cons_corpus5)

#---------------------------------------------------------------------------
#                  Managing Metadata
#---------------------------------------------------------------------------

for(doc in 1:length(cons_corpus4)){
  
  #Remove the .txt extension from the id
  cons_corpus4[[doc]]$meta$id <- sub('.txt','',cons_corpus4[[doc]]$meta$id)
  
  #Add number of words
  cons_corpus4[[doc]]$meta$words <- lengths(gregexpr("\\W+", cons_corpus4[[doc]])) + 1
  
  #Add new attribute status
  cons_corpus4[[doc]]$meta$status <- 'Cleaned'
  
}

# Print first document's metadata 
cons_corpus4[[3]]$meta

# Convert to dataframe
df_metadata <- data.frame(status=sapply(cons_corpus4,meta,"status"),
                          words=sapply(cons_corpus4,meta,"words"))
#Print the data frame
df_metadata

#Save corpus for future use in next chapters. 
#Note: This wont save meta data. If required,  
#the metadata's dataframe should be persisted seperately 
writeCorpus(cons_corpus4, "/Users/andresdelrio/Desktop/TextAnalytics/Constitutions/CleanedCorpus" )

#---------------------------------------------------------------------------
#                  Generating a Term Frequency Matrix
#---------------------------------------------------------------------------

cons_dtm <- TermDocumentMatrix(cons_corpus4)

#Inspect to TF-IDF
inspect(cons_dtm)

#List of docs in the matriz
Docs(cons_dtm)

#No of docs in the matriz
nDocs(cons_dtm)

#List of terms in the matrix
Terms(cons_dtm)
#No. of terms in the matrix
nTerms(cons_dtm)

#Convert to a matrix
cons_dtm_matrix <- as.matrix(cons_dtm)

#Inspect a specific term
cons_dtm_matrix["president",]

#---------------------------------------------------------------------------
#                  Improving Term Frequency Matrix
#---------------------------------------------------------------------------

#Find terms that have occured atleast 5 times
findFreqTerms(cons_dtm, 5)

#Remove sparse terms - Terms not there in 50% of the documents
dense_cons_dtm <- removeSparseTerms(cons_dtm, 0.2)

inspect(dense_cons_dtm)

#---------------------------------------------------------------------------
#                  Plotting Frequency Data
#---------------------------------------------------------------------------

#Generate a frequency table
cons_dtm_frequency <- sort(rowSums(as.matrix(dense_cons_dtm)), 
                           decreasing=TRUE)


#Print the table (vector)
cons_dtm_frequency


#Convert frequency table to a data frame
cons_dtm_df <- data.frame(word=names(cons_dtm_frequency), 
                          freq=cons_dtm_frequency)

#print the data frame
cons_dtm_df

#install.packages("ggplot2")
#library(ggplot2)

#frequency_plot <- ggplot(subset(cons_dtm_df,freq>100),aes(x=reorder(word,-freq),y=freq)) + geom_bar(stat = "identity", fill = "#FF6666") 

#frequency_plot

#---------------------------------------------------------------------------
#                  Genereting a Word Cloud
#---------------------------------------------------------------------------

#install.packages("wordcloud")
library(wordcloud)

# For reproducibility
set.seed(1234)

# Generate word cloud
wordcloud(words = cons_dtm_df$word, freq = cons_dtm_df$freq,random.order=FALSE,
          colors=brewer.pal(8, "Dark2"),max.words = 300)



#---------------------------------------------------------------------------
#                  Generating TF-IDF
#---------------------------------------------------------------------------

#Generate the TF-IDF 
#cons_tfidf <- DocumentTermMatrix(cons_corpus4, 
                                 #control= list(weighting = weightTfIdf))

#Inspect to TF-IDF
#inspect(cons_tfidf)

