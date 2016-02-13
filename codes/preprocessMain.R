library(tm)
library(RWeka)

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

###########################################################################################
###########################################################################################

# Helper functions
last4 <- content_transformer(function(x) findLast4Words(x))

#toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
preprocessCorpus2 <- function(corpus){
    #corpus <- tm_map(corpus, toSpace, "/|@|\\|")
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, PlainTextDocument)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeWords, profanities)
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
}

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

findLast4Words <- function(x){
    y <- unlist(strsplit(as.character(x), " "))
    n <- length(y)
    if(n<5){
        z <- y
    }else{
        z <- y[(n-3):n]
    }
    return(paste(z, collapse=" "))
}

###########################################################################################
# Preprocess

id <- 330000+(1:10000)
for(i in 4:10){
    print(i)
    text <- blogs[id]
    text <- sapply(text, function(row) iconv(row, "latin1", "ASCII", sub=""))
    corpus <- VCorpus(VectorSource(text))
    corpus <- preprocessCorpus2(corpus)
    print('2gram')
    tdm2gram <- TermDocumentMatrix(corpus, control=list(tokenize=BigramTokenizer))
    print('3gram')
    tdm3gram <- TermDocumentMatrix(corpus, control=list(tokenize=TrigramTokenizer))
    print('4gram')
    corpus2 <- tm_map(corpus, last4)
    tdm4gram <- TermDocumentMatrix(corpus2, control=list(tokenize=QuadgramTokenizer))
    if(i==1){
        tdm2gram_blogs <- tdm2gram
        tdm3gram_blogs <- tdm3gram
        tdm4gram_blogs <- tdm4gram
    }else{
        tdm2gram_blogs <- c(tdm2gram_blogs, tdm2gram)
        tdm3gram_blogs <- c(tdm3gram_blogs, tdm3gram)
        tdm4gram_blogs <- c(tdm4gram_blogs, tdm4gram)
    }
    id <- 10000+id
}

save(tdm2gram_blogs, tdm3gram_blogs, tdm4gram_blogs, file="tdmblogs3.RData")


rm(profanities)
rm(blogs)
rm(text)
rm(corpus)
rm(corpus2)
rm(tdm2gram)
rm(tdm3gram)
rm(tdm4gram)
rm(tdm2gram_blogs)
rm(tdm3gram_blogs)
rm(tdm4gram_blogs)
