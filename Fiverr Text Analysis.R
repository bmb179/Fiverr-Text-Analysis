library(tidyverse)
#setwd("C:/DIRECTORY")
#https://www.kaggle.com/datasets/kirilspiridonov/freelancers-offers-on-fiverr

fiverr <- read_csv("fiverr_clean.csv",
                   col_names = TRUE,
                   col_types = NULL,
                   col_select = NULL,
                   trim_ws = TRUE,
                   locale = locale(encoding="latin1"))

stopwords <- read_csv("stopwords.csv",
                      col_names = TRUE,
                      col_types = NULL,
                      col_select = NULL,
                      trim_ws = TRUE)

#cleaning
fiverr <- fiverr %>% mutate(stars_num = as.numeric(replace(fiverr$stars, fiverr$stars == "nul", 0)))
fiverr <- fiverr %>% rename("category" = "ï..Category")
fiverr$votes <- replace(fiverr$votes, fiverr$votes == "1k+", 1000)
fiverr$votes <- replace(fiverr$votes, fiverr$votes == "nul", 0)
fiverr <- fiverr %>% mutate(votes_num = as.numeric(fiverr$votes))
fiverr$name_clean <- tolower(fiverr$name)
fiverr$name_clean <- gsub("[[:punct:]]", "", fiverr$name_clean)

#text analysis of whole data
fiverr_words <- strsplit(fiverr$name_clean, "\\s+")#Split the text into individual words
fiverr_words_cleaned <- unlist(fiverr_words)[!(unlist(fiverr_words) %in% stopwords$stopwords)]#removing common words
fiverr_word_count <- table(fiverr_words_cleaned)#Count the frequency of each word
fiverr_word_count <- sort(fiverr_word_count, decreasing = TRUE)#Sort the word counts in descending order
head(fiverr_word_count, 20)#Top 100 words used with count
head(prop.table(fiverr_word_count)*100, 20)#Top 20 shown as a percentage of all significant words used

#text analysis of filtered data
data_pros <- filter(fiverr, votes_num >= 10 & (Subcat == "Data Analysis & Reports" | Subcat == "Databases"))
#creating the filtered dataset from the original, only searching data professionals with at least 10 reviews
#sample size = 56
#repeating previous work with the filtered dataset
data_pros_words <- strsplit(data_pros$name_clean, "\\s+")
data_pros_words_cleaned <- unlist(data_pros_words)[!(unlist(data_pros_words) %in% stopwords$stopwords)]
data_pros_word_count <- table(data_pros_words_cleaned)
data_pros_word_count <- sort(data_pros_word_count, decreasing = TRUE)
head(data_pros_word_count, 20)
head(prop.table(data_pros_word_count)*100, 20)

#creating exports for the frequency tables
write.csv(as.data.frame(fiverr_word_count), "export_fiverr_word_count.csv")
write.csv(as.data.frame(data_pros_word_count), "export_data_pros_word_count.csv")
