data = read.delim("Restaurant_Reviews.tsv",quote = '',
                  stringsAsFactors = FALSE)

library(tm)
library(SnowballC)

#cleaning the text
corpus = VCorpus(VectorSource(data$Review))

#lower cases
corpus = tm_map(corpus,content_transformer(tolower))

#remove all the numbers
corpus = tm_map(corpus,removeNumbers)

#remove punctuations
corpus = tm_map(corpus,removePunctuation)

#remove stop words
corpus = tm_map(corpus,removeWords,stopwords())

#stemming -> root of each words
corpus = tm_map(corpus,stemDocument)

#remove extra spaces
corpus = tm_map(corpus,stripWhitespace)
