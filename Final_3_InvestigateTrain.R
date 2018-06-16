#install.packages tm
#install.packages("tm")
library(tm)

setkeyv(train, c("SentenceId", "PhraseId"))

train <- train[, sent_start:=min(PhraseId), by=.(SentenceId)]

senTrain <- train[ sent_start == PhraseId][, sent_start := NULL]

#text mining - remove fluff and stop words 
senTrain.tm<- copy(senTrain)
setnames(senTrain.tm, "SentenceId", "doc_id")
setnames(senTrain.tm, "Phrase", "text")

reviewCorpus <- Corpus(DataframeSource(senTrain.tm))
inspect(reviewCorpus[[1]])

reviewCorpus <- tm_map(reviewCorpus, content_transformer(tolower))

# see what are stopwords 
stopwords("english")

# exclude some stopwords 
exceptions<- c('but','only','too','not','nor','most','again','because')
my_stopwords <- setdiff(stopwords("english"), exceptions)

skipWords <- function(x) removeWords(x, my_stopwords)
funcs <- list(removePunctuation, removeNumbers, stripWhitespace, skipWords)
cleanText <- tm_map(reviewCorpus, FUN = tm_reduce, tmFuns = funcs)
inspect(cleanText[[1]])

# finding most frequent words 3-20 characters
freqMatrix <- TermDocumentMatrix(cleanText, control = list(wordLengths = c(3,20)))
inspect(freqMatrix)
#findFreqTerms(freqMatrix, 50)
#frequentwords <- findFreqTerms(freqMatrix, 40)

# pick only 200 most frequent words
freqMatrix1 <- as.matrix(freqMatrix)
freqMatrix.sort <- sort(rowSums(freqMatrix1), decreasing=TRUE)
words.200 <- head(names(freqMatrix.sort), 202)

# convert good phrase without fluff back into data.fram
#goodphrase <- data.frame(text = sapply(a, as.character), stringsAsFactors = FALSE)
#goodphrase <- data.table(goodphrase)
#goodphrase <- goodphrase[, SentenceId := sequence(.N)]

#merge new text back to senTrain
#senWords1 <- merge(senTrain, goodphrase, by = "SentenceId", all = TRUE)
#setnames(senWords1, "text", "Short.phrase")

# remove ,()[].; from Phrase somehow, make the text to lower case 
senTrain$Phrase <- tolower(senTrain$Phrase)
senWords <- senTrain[, strsplit(Phrase,' ', fixed = T), by=.(SentenceId, Sentiment)]

setnames(senWords, "V1", "word")

wordBySent <- senWords[, (cnt=.N), by=.(word, Sentiment)]

setnames(wordBySent, "V1", "frequency")

# create density variable
wordBySent1 <- copy(wordBySent)
wordBySent1$Sentiment <- as.factor(wordBySent1$Sentiment)
wordBySent1 <- merge(wordBySent1, data.frame(table(Sentiment = wordBySent1$Sentiment)), by = c("Sentiment"))
setnames(wordBySent1, "Freq", "class.width")

wordBySent1 <- wordBySent1[, density := (frequency/class.width)]

# subset the whole density table with only the "good" frequentwords from tm package
densitytable.long <- wordBySent1[wordBySent1$word %chin% words.200]

densitytable <- dcast(densitytable.long, word ~ Sentiment, value.var = "density")
setnames(densitytable, c("0", "1", "2", "3", "4"), c("SN", "N", "NE", "P", "SP"))

# now pick the sentiment with the highest density for each words 
#densitytable[densitytable[, .I[pt == max(pt)], by=Subject]$V1]

bestSent <- colnames(densitytable)[apply(densitytable,1,which.max)]
densitytable <- cbind(densitytable, bestSent)

densitytable$bestSent <- as.numeric(factor(densitytable$bestSent, 
                 levels = c("SN", "N", "NE", "P", "SP"), 
                 ordered = TRUE))


# take a look at each sentiment category 
SN <- densitytable[grep("1", densitytable$bestSent),]
N <- densitytable[grep("2", densitytable$bestSent),]
NE <- densitytable[grep("3", densitytable$bestSent),]
P <- densitytable[grep("4", densitytable$bestSent),]
SP <- densitytable[grep("5", densitytable$bestSent),]

table(densitytable$bestSent)

new <- densitytable[get("bestSent") == 1, eval("bestSent") := -5]
new <- new[get("bestSent") == 2, eval("bestSent") := -1]
new <- new[get("bestSent") == 3, eval("bestSent") := 0]
new <- new[get("bestSent") == 4, eval("bestSent") := 1]

#new <- densitytable[get("bestSent") == 1, eval("bestSent") := -10]
#new <- new[get("bestSent") == 2, eval("bestSent") := -1]
#new <- new[get("bestSent") == 3, eval("bestSent") := 0]
#new <- new[get("bestSent") == 4, eval("bestSent") := 1]
#new <- new[get("bestSent") == 5, eval("bestSent") := 10]



removewords <- c('acting','audience','back','care','come'
                 ,'dialogue','get','goes','making','minutes','movie'
                 ,'script','time','watch','thing', 'action','around'
                 ,'comes','first','gets','going','know','material'
                 ,'place','plays','plot','say','scenes','screenplay'
                 ,'things','think','watching', 'american','bit','can'
                 ,'character','director','film','find','give','gives'
                 ,'keep','kids','made','manages','may','something'
                 ,'sometimes','still','take','takes','two','war','way'
                 ,'will','without','women', 'actors','cast','films'
                 ,'makes','movies','one','piece','screen','see','seen'
                 ,'work','year','years', "lrb", "rrb")

new <- new[!new$word %chin% removewords]

# investigare rrb and lrb 
senTrainsubset <- senTrain[ grep("lrb", senTrain$Phrase), ]

senTrainsubset[1]









