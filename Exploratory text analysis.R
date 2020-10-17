# Step 1: load the required libraries and set up the work environment.

library(tm)
library(RWeka)
library(tokenizers)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(formattable)

# Step 2: load the required files and statistics calculations

blogs <- readLines("SwiftKeyDataset/en_US.blogs.txt", warn=FALSE, encoding="UTF-8")
twits <- readLines("SwiftKeyDataset/en_US.twitter.txt", warn=FALSE, encoding="UTF-8")
news <- readLines("SwiftKeyDataset/en_US.news.txt", warn=FALSE, encoding="UTF-8")


len_blogs <- length(blogs)
len_twits <- length(twits)
len_news <- length(news)

size_blogs <- object.size(blogs)
size_twits <- object.size(twits)
size_news <- object.size(news)

max_line_blogs <- max(nchar(blogs))
max_line_twits <- max(nchar(twits))
max_line_news <- max(nchar(news))

avg_line_blogs <- mean(nchar(blogs))
avg_line_twits <- mean(nchar(twits))
avg_line_news <- mean(nchar(news))

# Summary table

data_metrics <- data.frame(file_name = c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"),
                           size = c(format(size_blogs, units = "auto"), 
                                    format(size_news, units = "auto"), 
                                    format(size_twits, units = "auto")),
                           lines = c(format(len_blogs, big.mark=","),
                                     format(len_news, big.mark=","),
                                     format(len_twits, big.mark=",")),
                           Average_line_length = c(round(avg_line_blogs,0), 
                                                   round(avg_line_news,0), 
                                                   round(avg_line_twits,0)),
                           max_line_length = c(format(max_line_blogs, big.mark=","), 
                                               format(max_line_news, big.mark=","), 
                                               format(max_line_twits, big.mark=","))
)

colnames(data_metrics) <- c('File Name', 'File Size', 'Number of Lines', 'Average Length of Lines', 'Maximun Length of a line') 
formattable(data_metrics)

# Step 3: Sampling the dataset 
# with 10% of every file, a corpus over 1 GB is created, which is inefficient, 
# so is selected a 1% 

set.seed(4532)

twits_sample <- sample(twits, length(twits)*.01)
news_sample <- sample(news, length(news)*.01)
blogs_sample <- sample(blogs, length(blogs)*.01)

combined_sample <- c(twits_sample, blogs_sample, news_sample)
combined_sample <- iconv(combined_sample, "UTF-8","ASCII", sub="")
combined_sample <- gsub("\\b(\\w+) \\1\\b", "\\1" , combined_sample)
combined_sample <- gsub("(.)\\1{2,}" ,"\\1", combined_sample)

# Lets see the number of lines 
length(combined_sample)

# Release memory

rm(twits,news, blogs,twits_sample,news_sample, blogs_sample)

# Step 4: Building the Corpus and cleaning data

#Ensure the data is all ASCII. The text will be all in lowecase.
# Digits, puntuation, stopwords, white spaces will be removed.

corpus <- VCorpus(VectorSource(combined_sample))

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation, mc.cores=1)
corpus <- tm_map(corpus, removeNumbers, mc.cores=1)
corpus <- tm_map(corpus, PlainTextDocument, mc.cores=1)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Step 5: Tokenize

# 1-grams

unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

unigram_tdm <- TermDocumentMatrix(corpus, control = list(tokenize = unigram))
uni_tdm_sparse <- removeSparseTerms(unigram_tdm, sparse=0.99)
unigram_freqTerm <- findFreqTerms(uni_tdm_sparse,lowfreq = 40)
unigram_freq <- rowSums(as.matrix(uni_tdm_sparse[unigram_freqTerm,]))
unigram_ord <- order(unigram_freq, decreasing = TRUE)
unigram_freq <- data.frame(word=names(unigram_freq[unigram_ord]), frequency=unigram_freq[unigram_ord])


# 2-grams

bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram_tdm <- TermDocumentMatrix(corpus, control = list(tokenize = bigram))
#bi_dtm_sparse <- removeSparseTerms(bigram_tdm, sparse=0.99)
bigram_freqTerm <- findFreqTerms(bigram_tdm,lowfreq = 40)
bigram_freq <- rowSums(as.matrix(bigram_tdm[bigram_freqTerm,]))
bigram_ord <- order(bigram_freq, decreasing = TRUE)
bigram_freq <- data.frame(word=names(bigram_freq[bigram_ord]), frequency=bigram_freq[bigram_ord])

# 3-grams

trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram_tdm <- TermDocumentMatrix(corpus, control = list(tokenize = trigram))
trigram_freqTerm <- findFreqTerms(trigram_tdm,lowfreq=10)
trigram_freq <- rowSums(as.matrix(trigram_tdm[trigram_freqTerm,]))
trigram_ord <- order(trigram_freq, decreasing = TRUE)
trigram_freq <- data.frame(word=names(trigram_freq[trigram_ord]), frequency=trigram_freq[trigram_ord])


# Step 6: Plot the most frequents n-grams

ggplot(unigram_freq[1:10,], aes(factor(word, levels = unique(word)), frequency)) +
  geom_bar(stat = 'identity')+
  theme(axis.text.x=element_text(angle=90))+
  xlab('1-gram')+
  ylab('Frequency')

ggplot(bigram_freq[1:20,], aes(factor(word, levels = unique(word)), frequency)) +
  geom_bar(stat = 'identity')+
  theme(axis.text.x=element_text(angle=90))+
  xlab('2-grams')+
  ylab('Frequency')

ggplot(trigram_freq, aes(factor(word, levels = unique(word)), frequency)) +
  geom_bar(stat = 'identity')+ theme(axis.text.x=element_text(angle=90))+
  xlab('3-grams')+  ylab('Frequency')

# Plot the most frequents n-grams in a word cloud

wordcloud(bigram_freq$word, bigram_freq$frequency, max.words=30, colors=brewer.pal(7,"YlOrRd"))

wordcloud(unigram_freq$word, unigram_freq$frequency, max.words=50, colors=brewer.pal(7,"YlOrRd"), scale=c(4,0.5), random.order=FALSE,rot.per=0.25)

wordcloud(trigram_freq$word, trigram_freq$frequency,  colors=brewer.pal(8,"YlOrRd"))




