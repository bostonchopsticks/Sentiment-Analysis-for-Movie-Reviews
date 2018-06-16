
library(data.table)
library(readr)

# import the data 
trf <- paste0(RawData,"/train.tsv")
tr <- fread(trf)

# split training data
splitCut <- 0.7
set.seed(123)
trows <- nrow(tr)
tr[, tmp_rnd := runif(trows)]
train <- tr[tmp_rnd <= splitCut][, tmp_rnd := NULL]
test <- tr[tmp_rnd > splitCut][, tmp_rnd := NULL]

# prepare test data 
setkeyv(test, c("SentenceId", "PhraseId"))

test <- test[, sent_start:=min(PhraseId), by=.(SentenceId)]

senTest <- test[ sent_start == PhraseId][, sent_start := NULL]


# 
  plist.test <- senTest[, c("Phrase","SentenceId", "Sentiment")] 
  setnames(plist.test,c("Phrase","SentenceId", "Sentiment"),c("phrase","phraseId", "sentiment"))
  
  # create phrase-word incidence (frequency) list
  plistw.test <- plist.test[, strsplit(phrase, ' '), by=list(phraseId, sentiment)] 
  setnames(plistw.test, "V1", "word")
  plistw.test <- plistw.test[, list(word_cnt=.N), by=list(phraseId, sentiment, word)]
  
  # match list words on the phrase
  plistw.test2 <- merge(plistw.test, wlist, by=c("word"), all.y = TRUE) # inner join
  length(which(is.na(plistw.test2)))
  plistw.test3 <- plistw.test2[,.(phraseId, sentiment, word_cnt, wid, weight)]
  length(which(is.na(plistw.test3)))
  plistw.test3$phraseId[is.na(plistw.test3$phraseId)] <- 0
  plistw.test3$sentiment[is.na(plistw.test3$sentiment)] <- 2
  plistw.test3$word_cnt[is.na(plistw.test3$word_cnt)] <- 0
  
  
  # choice!!!!  compute total weight based on word frequency and its frequency in a phrase
  #plistw.test3[, wcnt := word_cnt * weight]
  #plistw.test3 <- plistw.test3[,.(phraseId, sentiment, wid, wcnt)]
  #plistwide.test <- reshape(plistw.test3, idvar = c("phraseId", "sentiment"), timevar= "wid",direction = "wide" ,v.names = "wcnt")
  
  plistw.test3 <- plistw.test3[,.(phraseId, sentiment, wid, weight)]
  plistwide.test <- reshape(plistw.test3,idvar = c("phraseId", "sentiment"), timevar= "wid",direction = "wide" ,v.names = "weight")
  
  # fix names
  tn <- names(plistwide.test)
  #tn <- gsub("wcnt.","",tn, fixed= T)
  tn <- gsub("weight.","",tn, fixed= T)
  names(plistwide.test) <- tn
  for(tcol in names(plistwide.test)){
    plistwide.test[is.na(get(tcol)), eval(tcol):=0]
  }
  
  plistwide.test<- plistwide.test[!(plistwide.test$phraseId == 0),]
  realSent <- plistwide.test[ , c("phraseId", "sentiment")]
  
#write.csv(plistwide.test, file = "test.csv")
  
  
  
  plistwide.test[, sentiment := as.integer(sentiment)]
  
  plistwide.test[, sentiment := as.factor(sentiment)]
  

