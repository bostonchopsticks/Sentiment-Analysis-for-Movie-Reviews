# prepare data to train model 
wlist <- data.table(word=new$word, weight=new$bestSent)
# create a word id
wlist[, temp_ord := .I] # maybe you already have some order
wlist[, wid := paste0("w",sprintf("%04d",temp_ord))]
wlist[, temp_ord := NULL]

# a set of phrases
plist <- senTrain[, c("Phrase","SentenceId", "Sentiment")] 
setnames(plist,c("Phrase","SentenceId", "Sentiment"),c("phrase","phraseId", "sentiment"))

# create phrase-word incidence (frequency) list
plistw <- plist[, strsplit(phrase, ' '), by=list(phraseId, sentiment)] 
setnames(plistw, "V1", "word")
plistw <- plistw[, list(word_cnt=.N), by=list(phraseId, sentiment, word)]
# match list words on the phrase
plistw2 <- merge(plistw, wlist, by=c("word")) # inner join
plistw3 <- plistw2[,.(phraseId, sentiment, word_cnt, wid, weight)]

# choice!!!!  compute total weight based on word frequency and its frequency in a phrase
#plistw3[, wcnt := word_cnt * weight]
#plistw3 <- plistw3[,.(phraseId, sentiment, wid, wcnt)]
#plistwide <- reshape(plistw3,idvar = c("phraseId", "sentiment"), timevar= "wid",direction = "wide" ,v.names = "wcnt")
plistw3 <- plistw3[,.(phraseId, sentiment, wid, weight)]
plistwide <- reshape(plistw3,idvar = c("phraseId", "sentiment"), timevar= "wid",direction = "wide" ,v.names = "weight")

# fix names
tn <- names(plistwide)
#tn <- gsub("wcnt.","",tn, fixed= T)
tn <- gsub("weight.","",tn, fixed= T)
names(plistwide) <- tn
for(tcol in names(plistwide)){
  plistwide[is.na(get(tcol)), eval(tcol):=0]
}
