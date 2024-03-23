# Load necessary libraries
install.packages(c("tm", "wordcloud"))  
library(tm)  # Text Mining library for text preprocessing tasks
library(wordcloud)  # Library for creating word clouds

# Function to create word cloud
create_word_cloud <- function(input_texts) {
  # Create a corpus from the input texts
  corpus <- Corpus(VectorSource(input_texts))
  
  # Preprocess the text
  corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
  corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("english"))  # Remove stopwords
  
  # Create a document term matrix
  dtm <- DocumentTermMatrix(corpus)
  
  # Convert document term matrix to a matrix of word frequencies
  word_freq <- colSums(as.matrix(dtm))
  
  # Create a word cloud
  wordcloud(names(word_freq), word_freq, max.words = 50, random.order = FALSE)
}

# Function for sentiment analysis
sentiment_analysis <- function(input_text) {
  # Perform sentiment analysis (simple example)
  # For simplicity, just count the occurrences of positive and negative words
  
  # Sample positive and negative words
  positive_words <- c("good", "great", "happy", "awesome", "amazing")
  negative_words <- c("bad", "terrible", "sad", "awful", "dreadful")
  
  # Count occurrences of positive and negative words
  positive_count <- sum(sapply(positive_words, function(word) sum(grepl(word, input_text))))
  negative_count <- sum(sapply(negative_words, function(word) sum(grepl(word, input_text))))
  
  # Determine sentiment
  sentiment <- ifelse(positive_count > negative_count, "Positive", ifelse(negative_count > positive_count, "Negative", "Neutral"))
  
  return(sentiment)
}

# Function for text classification
text_classification <- function(input_text) {
  # Classify text based on length (simple example)
  text_length <- nchar(input_text)
  classification <- ifelse(text_length > 100, "Long Text", "Short Text")
  return(classification)
}

# Sample text inputs
input_texts <- c("The new dune movie was awesome, best movie eva!",
                 "The new dune movie was terrible, I asked for refund!",
                 "I absolutely loved the new dune film, it blew my mind!",
                 "The new dune flick was amazing, exceeded all my expectations!",
                 "The new dune adaptation was dreadful, waste of time and money!")

# Perform sentiment analysis for each text and display output
for (text in input_texts) {
  sentiment <- sentiment_analysis(text)
  cat("Sentiment for text:", text, "\n")
  cat("Sentiment:", sentiment, "\n\n")
}

# Perform text classification for each text and display output
for (text in input_texts) {
  classification <- text_classification(text)
  cat("Classification for text:", text, "\n")
  cat("Classification:", classification, "\n\n")
}
