library(tm)
library(RWeka)
#library(stringi)
#library(stringr)
#library(SnowballC)
library(ggplot2)
library(dplyr)
#library(wordcloud)


#Loading files
file0 <- file("./data/profanities.txt", "rb")
profanities <- readLines(file0, encoding="UTF-8")
close(file0)

file1 <- file("./final/en_US/en_US.blogs.txt", "rb")
blogs <- readLines(file1, encoding="UTF-8")
close(file1)

file2 <- file("./final/en_US/en_US.news.txt", "rb")
news <- readLines(file2, encoding="UTF-8")
close(file2)

file3 <- file("./final/en_US/en_US.twitter.txt", "rb")
twitter <- readLines(file3, encoding="UTF-8")
close(file3)


words_blogs <- stri_count_words(blogs)
words_news <- stri_count_words(news)
words_twitter <- stri_count_words(twitter)
size_blogs <- file.info("final/en_US/en_US.blogs.txt")$size/1024^2
size_news <- file.info("final/en_US/en_US.news.txt")$size/1024^2
size_twitter <- file.info("final/en_US/en_US.twitter.txt")$size/1024^2
summary_table <- data.frame(filename = c("blogs","news","twitter"),
                            file_size_MB = c(size_blogs, size_news, size_twitter),
                            num_lines = c(length(blogs),length(news),length(twitter)),
                            num_words = c(sum(words_blogs),sum(words_news),sum(words_twitter)),
                            mean_num_words = c(mean(words_blogs),mean(words_news),mean(words_twitter)))
summary_table


set.seed(1)
blogsSample <- sample(blogs, length(blogs)*0.01)
newsSample <- sample(news, length(news)*0.01)
twitterSample <- sample(twitter, length(twitter)*0.01)
twitterSample <- sapply(twitterSample, function(row) iconv(row, "latin1", "ASCII", sub=""))

text_sample  <- c(blogsSample,newsSample,twitterSample)
length(text_sample) #no of lines
sum(stri_count_words(text_sample))


toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
preprocessCorpus <- function(corpus){
    # Helper function to preprocess corpus
    corpus <- tm_map(corpus, toSpace, "/|@|\\|")
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeWords, profanities)
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
}

freq_frame <- function(tdm){
    m <- sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v)
    freq_frame <- data.frame(word=rownames(tdm), freq=rowSums(m))
    return(freq_frame)
}

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))


text_corpus <- VCorpus(VectorSource(text_sample))
text_corpus <- preprocessCorpus2(text_corpus)

tdm1gramT <- TermDocumentMatrix(text_corpus)
tdm1gram <- removeSparseTerms(tdm1gramT, 0.99)
freq1gram <- freq_frame(tdm1gram)

tdm2gramT <- TermDocumentMatrix(text_corpus, control=list(tokenize=BigramTokenizer))
tdm2gram <- removeSparseTerms(tdm2gramT, 0.999)
freq2gram <- freq_frame(tdm2gram)

tdm3gramT <- TermDocumentMatrix(text_corpus, control=list(tokenize=TrigramTokenizer))
tdm3gram <- removeSparseTerms(tdm3gramT, 0.9999)
freq3gram <- freq_frame(tdm3gram)

tdm4gramT <- TermDocumentMatrix(text_corpus, control=list(tokenize=QuadgramTokenizer))
tdm4gram <- removeSparseTerms(tdm4gramT, 0.9999)
freq4gram <- freq_frame(tdm4gram)


# Plots
n <- 15
freq1_top15 <- freq1gram[1:n,]
freq2_top15 <- freq2gram[1:n,]
freq3_top15 <- freq3gram[1:n,]
freq4_top15 <- freq4gram

wordcloud(freq1gram$word, freq1gram$freq, min.freq=200)

p1 <- ggplot(freq1_top15, aes(x=reorder(word,freq), y=freq, fill=freq)) +
    geom_bar(stat="identity") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y="Frequency", title="Most frequent words in blogs")
p1

p2 <- ggplot(freq2_top15, aes(x=reorder(word,freq), y=freq, fill=freq)) +
    geom_bar(stat="identity") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y="Frequency", title="Most frequent bigrams in blogs")
p2

p3 <- ggplot(freq3_top15, aes(x=reorder(word,freq), y=freq, fill=freq)) +
    geom_bar(stat="identity") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y="Frequency", title="Most frequent trigrams in blogs")
p3

p4 <- ggplot(freq4_top15, aes(x=reorder(word,freq), y=freq, fill=freq)) +
    geom_bar(stat="identity") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y="Frequency", title="Most frequent quadgrams in blogs")
p4
