library(tm)
library(Matrix)
#library(hashFunction)
#library(stringr)

freq_frame <- function(tdm){
    m <- sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v)
    freq_frame <- data.frame(word=rownames(tdm), freq=rowSums(m))
    return(freq_frame)
}

findFirstWord <- function(x){    
    y <- unlist(strsplit(as.character(x), " "))
    if(length(y)>0){
        return(paste(y[1:length(y)-1], collapse=" "))
    }else{
        return("")
    }
}

findLastWord <- function(x){
    y <- unlist(strsplit(as.character(x), " "))
    if(length(y)>0){
        return(y[length(y)])
    }else{
        return("")
    }
}

createFreqTable <- function(freqNgram){
    firstword <- unlist(lapply(freqNgram$word, findFirstWord))
    lastword <- unlist(lapply(freqNgram$word, findLastWord))
    #firstword <- sapply(firstword, hash_fx)
    #lastword <- sapply(lastword, hash_fx)
    ngrams <- data.frame(firstword=firstword, lastword=lastword, freq=freqNgram$freq)
    return(ngrams)
}

#hash_fx <- function(x) as.numeric(spooky.32(x))

###########################################################################################

load("./data/tdmNews1.RData")
tdm2gram <- tdm2gram_news
tdm3gram <- tdm3gram_news
tdm4gram <- tdm4gram_news
rm(tdm2gram_news)
rm(tdm3gram_news)
rm(tdm4gram_news)

load("./data/tdmNews4.RData")
tdm2gram <- c(tdm2gram, tdm2gram_news)
tdm3gram <- c(tdm3gram, tdm3gram_news)
tdm4gram <- c(tdm4gram, tdm4gram_news)
rm(tdm2gram_news)
rm(tdm3gram_news)
rm(tdm4gram_news)

tdm2gram_news <- tdm2gram
tdm3gram_news <- tdm3gram
tdm4gram_news <- tdm4gram
rm(tdm2gram)
rm(tdm3gram)
rm(tdm4gram)
save(tdm2gram_news, tdm3gram_news, tdm4gram_news, file="tdmNews.RData")


load("./data/tdmTwitter1.RData")
tdm2gram <- tdm2gram_twitter
tdm3gram <- tdm3gram_twitter
tdm4gram <- tdm4gram_twitter
rm(tdm2gram_twitter)
rm(tdm3gram_twitter)
rm(tdm4gram_twitter)

load("./data/tdmTwitter24.RData")
tdm2gram <- c(tdm2gram, tdm2gram_twitter)
tdm3gram <- c(tdm3gram, tdm3gram_twitter)
tdm4gram <- c(tdm4gram, tdm4gram_twitter)
rm(tdm2gram_twitter)
rm(tdm3gram_twitter)
rm(tdm4gram_twitter)

tdm2gram_twitter <- tdm2gram
tdm3gram_twitter <- tdm3gram
tdm4gram_twitter <- tdm4gram
rm(tdm2gram)
rm(tdm3gram)
rm(tdm4gram)
save(tdm2gram_twitter, tdm3gram_twitter, tdm4gram_twitter, file="tdmTwitter.RData")


load("./data/tdmblogs0.RData")
tdm2gram <- tdm2gram_blogs
tdm3gram <- tdm3gram_blogs
tdm4gram <- tdm4gram_blogs
rm(tdm2gram_blogs)
rm(tdm3gram_blogs)
rm(tdm4gram_blogs)

load("./data/tdmblogs8.RData")
tdm2gram <- c(tdm2gram, tdm2gram_blogs)
tdm3gram <- c(tdm3gram, tdm3gram_blogs)
tdm4gram <- c(tdm4gram, tdm4gram_blogs)
rm(tdm2gram_blogs)
rm(tdm3gram_blogs)
rm(tdm4gram_blogs)

tdm2gram_blogs <- tdm2gram
tdm3gram_blogs <- tdm3gram
tdm4gram_blogs <- tdm4gram
rm(tdm2gram)
rm(tdm3gram)
rm(tdm4gram)
save(tdm2gram_blogs, tdm3gram_blogs, tdm4gram_blogs, file="tdmBlogs.RData")


###########################################################################################

load("tdmNews.Rdata")
load("tdmTwitter.Rdata")
load("tdmBlogs.Rdata")
save(tdm2gram_news, tdm2gram_twitter, tdm2gram_blogs, file="tdm2.RData")
save(tdm3gram_news, tdm3gram_twitter, tdm3gram_blogs, file="tdm3.RData")
save(tdm4gram_news, tdm4gram_twitter, tdm4gram_blogs, file="tdm4.RData")
save(tdm2gram_news, file="tdm2gram_news.RData")
save(tdm3gram_news, file="tdm3gram_news.RData")
save(tdm2gram_twitter, file="tdm2gram_twitter.RData")
save(tdm3gram_twitter, file="tdm3gram_twitter.RData")
save(tdm2gram_blogs, file="tdm2gram_blogs.RData")
save(tdm3gram_blogs, file="tdm3gram_blogs.RData")

###########################################################################################

load("tdm2.Rdata")
tdm2gramT_news <- removeSparseTerms(tdm2gram_news, 0.99999)
save(tdm2gramT_news, file="tdm2gramT_news.RData")
tdm2gramT_twitter <- removeSparseTerms(tdm2gram_twitter, 0.99999)
save(tdm2gramT_twitter, file="tdm2gramT_twitter.RData")
tdm2gramT_blogs <- removeSparseTerms(tdm2gram_blogs, 0.99999)
save(tdm2gramT_blogs, file="tdm2gramT_blogs.RData")
tdm2gram <- c(tdm2gramT_news, tdm2gramT_twitter, tdm2gramT_blogs)
freq2gram <- freq_frame(tdm2gram)
tab2gram <- createFreqTable(freq2gram)
save(tab2gram, file="tab2gram.RData")
rm(tdm2gram_news)
rm(tdm2gram_twitter)
rm(tdm2gram_blogs)
rm(tdm2gramT_news)
rm(tdm2gramT_twitter)
rm(tdm2gramT_blogs)
rm(tdm2gram)

load("tdm3.Rdata")
tdm3gramT_news <- removeSparseTerms(tdm3gram_news, 0.99999)
save(tdm3gramT_news, file="tdm3gramT_news.RData")
tdm3gramT_twitter <- removeSparseTerms(tdm3gram_twitter, 0.99999)
save(tdm3gramT_twitter, file="tdm3gramT_twitter.RData")
tdm3gramT_blogs <- removeSparseTerms(tdm3gram_blogs, 0.99999)
save(tdm3gramT_blogs, file="tdm3gramT_blogs.RData")
tdm3gram <- c(tdm3gramT_news, tdm3gramT_twitter, tdm3gramT_blogs)
freq3gram <- freq_frame(tdm3gram)
tab3gram <- createFreqTable(freq3gram)
save(tab3gram, file="tab3gram.RData")
rm(tdm3gram_news)
rm(tdm3gram_twitter)
rm(tdm3gram_blogs)
rm(tdm3gramT_news)
rm(tdm3gramT_twitter)
rm(tdm3gramT_blogs)
rm(tdm3gram)

load("tdm4.Rdata")
#tdm4gramT_news <- removeSparseTerms(tdm4gram_news, 0.99999)
#tdm4gramT_twitter <- removeSparseTerms(tdm4gram_twitter, 0.99999)
#tdm4gramT_blogs <- removeSparseTerms(tdm4gram_blogs, 0.99999)
#tdm4gram <- c(tdm4gramT_news, tdm4gramT_twitter, tdm4gramT_blogs)
tdm4gram <- c(tdm4gram_news, tdm4gram_twitter, tdm4gram_blogs)
freq4gram <- freq_frame(tdm4gram)
freq4gramT <- freq4gram[freq4gram$freq>1,]
tab4gram <- createFreqTable(freq4gramT)
save(tab4gram, file="tab4gram.RData")
rm(tdm4gram_news)
rm(tdm4gram_twitter)
rm(tdm4gram_blogs)
rm(tdm4gram)
