# Library used in this script.
library(dplyr) # slice function
library(tidyr) # separate_rows function
library(stopwords) # stopwords function
library(stringr) # stringr function
library(tm) # stripWhitespace, Corpus, DocumentTermMatrix function   
library(gutenbergr) # gutenberg_works function
library(textstem) # lemmatize_strings function
library(caret) # stratified splitting function
library(naivebayes) # multinominal nb

# Create a pre-processing function to pre-process books.
preprocessing <- function(book, label) {
  # Establish stop words and initialize a list.
  stopwordsList <- stopwords('en')
  list_of_stopwords <- vector(mode="list", length=174)
  
  for (i in 174:1) {
    list_of_stopwords[175-i] <- stopwordsList[i]  
  }
  
  # Establishing stop words
  stopwords_regex <- paste(list_of_stopwords, collapse = '\\b|\\b')
  stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  
  # Rename column
  names(book)[1] <- 'text'
  
  # Replace certain type of punctuation.
  book <- gsub("â€œ", "\"", book$text)
  book <- gsub("â€", "\"", book)
  book <- gsub("â€˜", "\'", book)
  book <- gsub("â€™", "\'", book)
  
  # Convert data frame to string for splitting.
  book <- toString(book)
  
  # Splitting and converting back to data frame for further cleaning.
  book <- as.vector(strsplit(book, split="[.]|[!]|[?]"))
  book <- data.frame(book)
  names(book)[1] <- 'text'
  
  # Remove non-graphic characters
  book$text <- gsub("[^[:graph:]]", " ", book$text)
  
  # Lower-casing
  book$text <- tolower((book$text))
  
  # Remove "'s" and "'ll".
  book$text <- gsub("'s", "", book$text)
  book$text <- gsub("'ll", "", book$text)
  
  # Remove stop words
  book$text <- stringr::str_replace_all(book$text, stopwords_regex, "")
  
  # Remove remaining punctuation, numbers and extra white space in all rows.
  book$text <- gsub("[^a-zA-Z ]", "", book$text)
  book$text <- stripWhitespace(book$text)
  book$text <- str_trim(book$text)
  
  # Remove empty rows.
  book <- filter(book, book$text != "")
  book <- filter(book, book$text != " ")  
  
  # Lemmatization 
  book$text <- lemmatize_strings(book$text)
  
  # Labeling
  book$target <- label
  
  return(book)
}

################## Shakespeare, William ################## 
# View works written by Shakespeare, William
# View(gutenberg_works(author == "Shakespeare, William"))

# A Midsummer Night's Dream by William Shakespeare
book_id <- 1514
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:88)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 1)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- as.data.frame(book[dataIndex, ])
data <- book

# The Comedy of Errors by William Shakespeare
book_id <- 1504
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:60)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 1)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# The Tragedy of Titus Andronicus by William Shakespeare
book_id <- 1507
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:40)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 1)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# The Taming of the Shrew by William Shakespeare
book_id <- 1508
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:98)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 1)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

################## Bedford-Jones, H. (Henry) ################## 
# View works written by Bedford-Jones, H. (Henry)
# View(gutenberg_works(author == "Bedford-Jones, H. (Henry)"))

# Nuala O'Malley by H. Bedford-Jones
book_id <- 30979
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:89)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 2)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1400)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# The Mesa Trail by H. Bedford-Jones
book_id <- 35078
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:50)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 2)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1300)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# The Mardi Gras Mystery by H. Bedford-Jones
book_id <- 39229
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:117)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 2)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1300)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

################## Paterson, A. B. (Andrew Barton) ################## 
# View works written by the author
# View(gutenberg_works(author == "Paterson, A. B. (Andrew Barton)"))

# Three Elephant Power, and Other Stories by A. B. Paterson
book_id <- 307
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:44)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 3)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1400)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# An Outback Marriage: A Story of Australian Life by A. B. Paterson
book_id <- 6119
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:43)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 3)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1400)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# Saltbush Bill, J. P. by A. B. Paterson
book_id <- 1317
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:25)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 3)

# Append to data
#dataIndex <- sample(1:nrow(book), size=600)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# The Man from Snowy River by A. B. Paterson
book_id <- 213
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:42)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 3)

# Append to data
#dataIndex <- sample(1:nrow(book), size=600)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

################## Dickens, Charles ################## 
# View works written by the author
# View(gutenberg_works(author == "Dickens, Charles"))

# A Christmas Carol in Prose; Being a Ghost Story of Christmas by Charles Dickens
book_id <- 46
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:34)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 4)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# A Tale of Two Cities by Charles Dickens
book_id <- 98
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:75)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 4)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# The Mystery of Edwin Drood by Charles Dickens
book_id <- 564
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:41)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 4)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# Barnaby Rudge: A Tale of the Riots of 'Eighty by Charles Dickens
book_id <- 917
book <- gutenberg_download(book_id, meta_fields = "author")

# Remove irrelevant rows and columns
book <- book %>%
  slice(-(1:21)) %>%
  subset(select=-(1))

# Pre-processing
book <- preprocessing(book, 4)

# Append to data
#dataIndex <- sample(1:nrow(book), size=1000)
#data <- rbind(data, book[dataIndex, ])
data <- rbind(data, book)

# Remove variables that are not used anymore
rm(book)
rm(book_id)
#rm(dataIndex)
rm(preprocessing)

################## Training & Testing ################## 
# Create a Corpus 
docs <- Corpus(VectorSource(data$text))

# Bags-of-words
dtm <- DocumentTermMatrix(docs)
dtm <- removeSparseTerms(dtm, 0.999)
data_raw <- data.frame(as.matrix(dtm))
data_raw$target <- data$target

# Splitting data into train and test set
trainIndex <- createDataPartition(data_raw$target,
                                  p = .7,
                                  list = FALSE)

train <- data_raw[ trainIndex,]
test <- data_raw[-trainIndex,]

# Model (Naive Bayes)
mnb <- multinomial_naive_bayes(x = as.matrix(train), 
                               y = as.factor(train$target), 
                               laplace = 1)
summary(mnb)
mnb.predict <- predict(mnb, newdata=as.matrix(test), type="class")  

# Confusion matrix
table(mnb.predict, test$target)
