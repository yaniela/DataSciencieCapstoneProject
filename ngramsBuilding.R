# N-grams build

library(tm)
library(qdap)
library(tokenizers)
library(ngram)
library(quanteda)

#Sample the files and combine to build the corpus


blogs <- readLines("SwiftKeyDataset/en_US.blogs.txt", warn=FALSE, encoding="UTF-8")
twits <- readLines("SwiftKeyDataset/en_US.twitter.txt", warn=FALSE, encoding="UTF-8")
news <- readLines("SwiftKeyDataset/en_US.news.txt", warn=FALSE, encoding="UTF-8")

set.seed(4532)

twits_sample<- sample(twits, length(twits)*.05)
news_sample<- sample(news, length(news)*.1)
blogs_sample<- sample(blogs, length(blogs)*.1)


combined_sample <- c(twits_sample, blogs_sample, news_sample)
combined_sample <- iconv(combined_sample, "UTF-8","ASCII", sub="")

# pattern to empty
combined_sample <- gsub("\\b(\\w+) \\1\\b", "\\1" , combined_sample)
# pattern to duplicate words
combined_sample <- gsub("(.)\\1{2,}" ,"\\1", combined_sample)

# turns ? and ! and . into an end of sentence identifier EEOSS. The purpose is that when forming ngrams we 
# discard ngrams that are formed on the edges of sentences and combine words from more than one sentence

combined_sample <- gsub("\\? |\\?$|\\! |\\!$", " EEOSS ", combined_sample)
combined_sample <-gsub("\\. |\\.$", " EEOSS ", combined_sample)

# turns abbreviations as H.S.B.C. into an identifier AABRR. The purpose is that when forming ngrams we 
# discard ngrams that are formed on the edges of such abbreviations as these ngrams are incorrect 
# from information perspective

combined_sample<- gsub("[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\. ", " AABRR ", combined_sample)

# turns numbers into an identifier NNUMM. The purpose is that when forming ngrams we 
# discard ngrams that are formed on the edges of such numbers as these ngrams are incorrect 
# from information perspective. A common approach is the 'remove numbers' with tm package.
# However, this will lead to formation of incorrect ngrams later on

combined_sample<-gsub("[0-9]+"," NNUMM ",combined_sample)

# Building the corpus with quanteda package

corpus <- corpus(combined_sample)

# Buildings tokens to make the ngrams (1,2,3 and 4 grams)

toks <- tokens(corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols=TRUE, remove_url=TRUE)

unigrams<-tokens_ngrams(toks, n = 1)
unigramsTable<-textstat_frequency(unigrams%>%dfm(stem = TRUE))


unigramsTable <- unigramsTable[!grepl("_",unigramsTable$feature),]
unigramsTable <- unigramsTable[!grepl("nnumm",unigramsTable$feature),]
unigramsTable <- unigramsTable[!grepl("eeoss",unigramsTable$feature),]
unigramsTable <- unigramsTable[!grepl("aabrr",unigramsTable$feature),]
unigramsTable <- unigramsTable[!grepl("-",unigramsTable$feature),]
unigramsTable <- unigramsTable[!grepl("#",unigramsTable$feature),]
unigramsTable <- unigramsTable[!grepl("@",unigramsTable$feature),]

write.csv(unigramsTable,"SampleSwiftKeyDataset/unigrams.csv")


bigrams<-tokens_ngrams(toks, n = 2, concatenator = " ")
bigramsTable<-textstat_frequency(bigrams%>%dfm(stem = TRUE))

bigramsTable <- bigramsTable[!grepl("_",bigramsTable$feature),]
bigramsTable <- bigramsTable[!grepl("nnumm",bigramsTable$feature),]
bigramsTable <- bigramsTable[!grepl("eeoss",bigramsTable$feature),]
bigramsTable <- bigramsTable[!grepl("aabrr",bigramsTable$feature),]
bigramsTable <- bigramsTable[!grepl("-",bigramsTable$feature),]
bigramsTable <- bigramsTable[!grepl("#",bigramsTable$feature),]
bigramsTable <- bigramsTable[!grepl("@",bigramsTable$feature),]
write.csv(bigramsTable,"SampleSwiftKeyDataset/bigrams.csv")

trigrams<-tokens_ngrams(toks, n = 3, concatenator = " ")
trigramsTable<-textstat_frequency(trigrams%>%dfm(stem = TRUE))

trigramsTable <- trigramsTable[!grepl("_",trigramsTable$feature),]
trigramsTable <- trigramsTable[!grepl("nnumm",trigramsTable$feature),]
trigramsTable <- trigramsTable[!grepl("eeoss",trigramsTable$feature),]
trigramsTable <- trigramsTable[!grepl("aabrr",trigramsTable$feature),]
trigramsTable <- trigramsTable[!grepl("-",trigramsTable$feature),]
trigramsTable <- trigramsTable[!grepl("#",trigramsTable$feature),]
trigramsTable <- trigramsTable[!grepl("@",trigramsTable$feature),]

write.csv(trigramsTable,"SampleSwiftKeyDataset/trigrams.csv")

quadgrams<-tokens_ngrams(toks, n = 4, concatenator = " ")
quadgramsTable<-textstat_frequency(quadgrams%>%dfm(stem = TRUE))

quadgramsTable <- quadgramsTable[!grepl("_",quadgramsTable$feature),]
quadgramsTable <- quadgramsTable[!grepl("nnumm",quadgramsTable$feature),]
quadgramsTable <- quadgramsTable[!grepl("eeoss",quadgramsTable$feature),]
quadgramsTable <- quadgramsTable[!grepl("aabrr",quadgramsTable$feature),]
quadgramsTable <- quadgramsTable[!grepl("-",quadgramsTable$feature),]
quadgramsTable <- quadgramsTable[!grepl("#",quadgramsTable$feature),]
quadgramsTable <- quadgramsTable[!grepl("@",quadgramsTable$feature),]

write.csv(quadgramsTable,"SampleSwiftKeyDataset/quadgrams.csv")


