# Load libraries that is used in this project.
library(gutenbergr)
library(stringr)
library(dplyr)
library(tm)
library(stopwords)
library(textstem)
library(superml)
library(ggplot2)
library(tidytext)
library(wordcloud)

# Download books.
authors_id = c(53, 113, 118, 37)

# Initialize vector.
book_id <- c()

# Loop to include books ID into the vector.
for (id in authors_id) {
  book_id <- append(book_id, pull(gutenberg_works(gutenberg_author_id == id), gutenberg_id))
}

# Download books.
books <- gutenberg_download(book_id, meta_fields = "author")

# Remove variables that is no longer needed.
rm(authors_id)
rm(book_id)
rm(id)

# Remove unnecessary column.
data <- subset(books, select = -(gutenberg_id))

# Tokenization & removal of stop-words.
testData <- data %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Remove "'s" from data.
testData$word <- gsub("'s", "", testData$word)

# Remove punctuation and white-space from data.
testData$word <- gsub("[^a-zA-Z ]", " ", testData$word)
testData$word <- stripWhitespace(testData$word)
testData$word <- str_trim(testData$word)

# Remove empty rows.
testData <- testData %>%
  filter(word != "")

testData <- testData %>%
  filter(word != " ")

# Lemmatization
testData$word <- lemmatize_strings(testData$word)

# Label encoding
label <- LabelEncoder$new()
testData$author <- label$fit_transform(testData$author)

# Word count for each author
wordCount_0 <- testData %>%
  filter(author == 0) %>%
  count(word) %>%
  arrange(desc(n))

wordCount_1 <- testData %>%
  filter(author == 1) %>%
  count(word) %>%
  arrange(desc(n))

wordCount_2 <- testData %>%
  filter(author == 2) %>%
  count(word) %>%
  arrange(desc(n))

wordCount_3 <- testData %>%
  filter(author == 3) %>%
  count(word) %>%
  arrange(desc(n))

# Print distribution of class
testData %>%
  count(author) %>%
  ggplot(aes(x= reorder(author, -n, sum), y = n)) +
  geom_col(fill="deepskyblue") +
  theme_bw() +
  labs(title = "Author distribution in the dataset") +
  xlab("Author") +
  ylab("Count")

# Wordcloud for author 0
wordcloud(words=wordCount_0$word,
          freq=wordCount_0$n,
          max.words=100,
          scale = c(3.5, 0.2))

# Wordcloud for author 1
wordcloud(words=wordCount_1$word,
          freq=wordCount_1$n,
          max.words=100,
          scale = c(3.5, 0.2))

# Wordcloud for author 2
wordcloud(words=wordCount_2$word,
          freq=wordCount_2$n,
          max.words=100,
          scale = c(3.5, 0.2))

# Wordcloud for author 3
wordcloud(words=wordCount_3$word,
          freq=wordCount_3$n,
          max.words=100,
          scale = c(3.5, 0.2))

