library(stylo)
library(gutenbergr)
library(dplyr)

stylo()

# setwd("F:/INFO411/Assignments/Group/Corpus"), 
# plain texts will be downloaded inside Corpus folder

b <- gutenberg_works(gutenberg_author_id %in% c(53, 113, 118, 37))
# get author id, title and gutenberg_id for each book
books <- b[c(1,2,3)] 

# download text files for all authors as plain text format, 
# need to manually change sepcial chacaters
for (i in 1:nrow(books)){
  current_path = paste0(getwd(),"/")
  url = paste0("https://www.gutenberg.org/files/",
               books$gutenberg_id[i],
               "/",
               books$gutenberg_id[i],"-0.txt")
  url2 = paste0("https://www.gutenberg.org/files/",
                books$gutenberg_id[i],
                "/",
                books$gutenberg_id[i],".txt")
  
  filename = paste0(books$gutenberg_id[i],"-0.txt")
  filename2 = paste0( books$gutenberg_id[i],".txt")
  
  fullpath = paste0(getwd(),filename)
  fullpath2 = paste0(getwd(),filename2)
  
  try(download.file(url, destfile = filename,
                  quiet = FALSE, mode = "w",
                  cacheOK = TRUE) & file.rename(paste0(current_path, filename), 
                                               paste0(books$author[i], 
                                                      "_", 
                                                      books$title[i],
                                                      ".txt")))
  
  
  try(download.file(url2, destfile = filename2,
                    quiet = FALSE, mode = "w",
                    cacheOK = TRUE) & file.rename(paste0(current_path, filename2), 
                                               paste0(books$author[i], 
                                                      "_", 
                                                      books$title[i],
                                                      ".txt")))
}

# question: 
# 1-gram vs 2 gram which give higher accuracy? 
# does pronunces increase accuracy? 

# load corpus
data <- stylo(path="F:/INFO411/Assignments/Group", 
              gui = FALSE, 
              analyzed.features = "w",
              ngram.size = 1)

data2 <- stylo(path="F:/INFO411/Assignments/Group", 
               gui = FALSE, 
               analyzed.features = "w",
               ngram.size = 2)

summary(data)
summary(data2)

# use frequency table
freq_data <- data$table.with.all.freqs

freq_data_2gram <- data2$table.with.all.freqs

# delete pronounces for 1 gram to improve accuracy
freq_data <- delete.stop.words(freq_data, stop.words = stylo.pronouns(corpus.lang = "English"))

# show all books
rownames(freq_data)
rownames(freq_data_2gram)

# split
training_set = freq_data[-c(250:260, 131, 100:120, 1: 30), 1:100]# ~77%
test_set = freq_data[c(250:260,131,100:120, 1: 30), 1:100] # ~23%

training_set2 = freq_data_2gram[-c(250:260, 131, 100:120, 1: 30), 1:100]# ~77%
test_set2 = freq_data_2gram[c(250:260,131,100:120, 1: 30), 1:100] # ~23%



# training & verify - delta
results_delta = classify(gui = FALSE, 
                   classification.method = "delta",
                   training.frequencies = training_set,
                   test.frequencies = test_set)
summary(results_delta)
# training & verify - delta(2 gram)
results_delta2 = classify(gui = FALSE, 
                    classification.method = "delta",
                    training.frequencies = training_set2,
                    test.frequencies = test_set2)

# training & verify - knn
results_knn = classify(gui = FALSE, 
                       classification.method = "knn",
                       training.frequencies = training_set,
                       test.frequencies = test_set)

# training & verify - knn(2 gram)
results_knn2 = classify(gui = FALSE, 
                    classification.method = "knn",
                    k.value = 3,
                    training.frequencies = training_set2,
                    test.frequencies = test_set2)

# training & verify - svm
results_svm = classify(gui = FALSE, 
                       classification.method = "svm",
                       training.frequencies = training_set,
                       test.frequencies = test_set)

# training & verify - svm(2 gram)
results_svm2 = classify(gui = FALSE, 
                    classification.method = "svm",
                    training.frequencies = training_set2,
                    test.frequencies = test_set2)

# training & verify - naivebayes
results_nb = classify(gui = FALSE, 
                      classification.method = "naivebayes",
                      training.frequencies = training_set,
                      test.frequencies = test_set)

# training & verify - naivebayes(2 gram)
results_nb2 = classify(gui = FALSE, 
                    classification.method = "naivebayes",
                    training.frequencies = training_set2,
                    test.frequencies = test_set2)

# with pronounces 
freq_data_with_pronounces <- data$table.with.all.freqs
training_set_pronounces <-  freq_data_with_pronounces[-c(250:260, 131, 100:120, 1: 30), 1:100]# ~77%
test_set_pronounces  <-  freq_data_with_pronounces[c(250:260,131,100:120, 1: 30), 1:100] # ~23%
results_delta3 = classify(gui = FALSE, 
                         classification.method = "delta",
                         training.frequencies = training_set_pronounces,
                         test.frequencies = test_set_pronounces)
results_knn3 = classify(gui = FALSE, 
                       classification.method = "knn",
                       training.frequencies = training_set_pronounces,
                       test.frequencies = test_set_pronounces)
results_svm3 = classify(gui = FALSE, 
                        classification.method = "svm",
                        training.frequencies = training_set_pronounces,
                        test.frequencies = test_set_pronounces)
results_nb3 = classify(gui = FALSE, 
                      classification.method = "naivebayes",
                      training.frequencies = training_set_pronounces,
                      test.frequencies = test_set_pronounces)


# building table for comparison. 1 gram vs 2 gram? pronounces or not pronounces? 
c_names <- c("delta", "knn", "svm", "naivebayes")

one_gram <- c(results_delta$success.rate, 
              results_knn$success.rate,
              results_svm$success.rate,
              results_nb$success.rate)

two_gram <- c(results_delta2$success.rate, 
              results_knn2$success.rate,
              results_svm2$success.rate,
              results_nb2$success.rate)

one_gram_with_pronounce <- c(results_delta3$success.rate, 
                  results_knn3$success.rate,
                  results_svm3$success.rate,
                  results_nb3$success.rate)

df_grams <- data.frame(c_names, one_gram, two_gram)
view(df_grams)

df_pronunce <- data.frame(c_names, one_gram, one_gram_with_pronounce)
view(df_pronunce)

results_svm$features
table(results_knn$expected, results_knn$predicted)


