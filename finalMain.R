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
    ngrams <- data.frame(firstword=firstword, lastword=lastword, freq=freqNgram$freq)
    return(ngrams)
}

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

process_input <- function(input_text){
    input_corpus <- VCorpus(VectorSource(input_text))
    input_corpus <- preprocessCorpus2(input_corpus)
    input <- unlist(input_corpus)[1]
    output <- unlist(strsplit(input, " ", fixed=TRUE))
    return(as.character(output))
}

predict_ngrams <- function(input, tabEnd, tab3gram, tab2gram){
    n <- length(input)
    if(n==0){
        prediction <- NULL
    }else{
        pred1 <- NULL
        pred2 <- NULL
        pred3 <- NULL
        if(n>=3){
            input1 <- paste(input[(n-2):n], collapse=" ")
            match <- tabEnd[tabEnd$firstword==input1,]
            pred3 <- match[order(match$freq, decreasing=TRUE),]$lastword 
        }
        if(n>=2){
            input1 <- paste(input[(n-1):n], collapse=" ")
            match <- tab3gram[tab3gram$firstword==input1,]
            pred2 <- match[order(match$freq, decreasing=TRUE),]$lastword    
        }
        if(n>=1){
            input1 <- input[length(input)]
            match <- tab2gram[tab2gram$firstword==input1,]
            pred1 <- match[order(match$freq, decreasing=TRUE),]$lastword
        }
        prediction <- c(as.character(pred3), as.character(pred2), as.character(pred1))
    }
    if(length(prediction)==0)
        return(c("the","of","an","in","and"))
    else if(length(prediction)>15){
        return(prediction[1:15])
    }else{
        return(prediction)
    }
}


