install.packages("streamR")
install.packages("tm")
install.packages("wordcloud")
#install.packages("Rstem")
install.packages("stringr")
install.packages("plyr")
install.packages("dplyr")
library(dplyr)
library(streamR)
library(tm)
library(wordcloud)
#library(Rstem)
library(stringr)
library(plyr)

x <- c("streamR", "tm", "stringr", "plyr", "dplyr")
install.packages(x) 
lapply(x, library, character.only = TRUE)
setwd("~/Desktop/Assign4")

# basic text mining in R: https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# http://bl.ocks.org/mbostock/3887051 group bar chart

tweets_316 <- parseTweets("tweets.03.16.2016.summary.json")

#combine the tweets from ten days into one file for the first two questions
tweets <- rbind(tweets_315, tweets_316, tweets_317, tweets_318, tweets_319, tweets_320, tweets_321, tweets_322, tweets_323, tweets_324, tweets_325)
#'Topic List: Economy, Immigration, Health Care, Military, Gun Control, China, Trade, Race, Climate Change, Religion
#' create a csv to store keywords for each topic
#' economy = ("job", "crisis", "stock", "growth", "tax")

#' Two static visualizations comparing the levels of the usage of each of the lexicon:
#' Parse the tweets by candidates:
tweets$text <- sapply(tweets$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

#Combine similar keywords for the four candidates
tweets$text <- gsub("Hillary Clinton", "Clinton", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("hillaryclinton", "Clinton", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("HillaryClinton", "Clinton", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("hillary clinton", "Clinton", tweets$text, ignore.case = TRUE)

tweets$text <- gsub("Bernie Sanders", "Sanders", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("berniesanders", "Sanders", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("BernieSanders", "Sanders", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("bernie sanders", "Sanders", tweets$text, ignore.case = TRUE)

tweets$text <- gsub("Donald Trump", "Trump", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("donaldtrump", "Trump", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("DonaldTrump", "Trump", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("donald trump", "Trump", tweets$text, ignore.case = TRUE)

tweets$text <- gsub("Ted Cruz", "Cruz", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("tedcruz", "Cruz", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("TedCruz", "Cruz", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("ted cruz", "Cruz", tweets$text, ignore.case = TRUE)

tweets$text <- gsub("Democrat", "democrat", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("Republican", "republican", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("Democratic Party", "democrat", tweets$text, ignore.case = TRUE)
tweets$text <- gsub("Republican Party", "republican", tweets$text, ignore.case = TRUE)

#divide the tweets by candidates
#create the divisions for each candidate
clinton <- grep("Clinton", tweets$text, ignore.case = TRUE)
sanders <- grep("Sanders", tweets$text, ignore.case = TRUE)
trump <- grep("Trump", tweets$text, ignore.case = TRUE)
cruz <- grep("Cruz", tweets$text, ignore.case = TRUE)

tweets$index <- seq.int(nrow(tweets))
tweet_clinton <- filter(tweets, index %in% clinton)
tweet_cruz <- filter(tweets, index %in% cruz)
tweet_trump <- filter(tweets, index %in% trump)
tweet_sanders <- filter(tweets, index %in% sanders)

#divide the tweets by political parties
Democrat <- grep("democrat", tweets$text, ignore.case = TRUE)
Republican <- grep("republican", tweets$text, ignore.case = TRUE)

tweet_democrat <- filter(tweets, index %in% Democrat)
tweet_republican <- filter(tweets, index %in% Republican)
#Now I have all the ready to visualize tweets! Yeah!

#' borrow from text mining tutorial but does not seem to work
#dtm <- DocumentTermMatrix(TweetCorpus)
#dtm
#test_freq <- colSums(as.matrix(dtm))
#length(test_freq)
#ord_test <- order(test_freq)
#test_freq[head(ord_test)]
#test_freq

#Clean up text to make tweet corpus. Might want to divide by candidates and party first before doing that (otherwise get error msg)? (but text is not modified by the following, should not affect)
#for Clinton
TweetCorpus_clinton <- paste(unlist(tweet_clinton$text), collapse =" ") #to get all of the tweets together
TweetCorpus_clinton <- Corpus(VectorSource(TweetCorpus_clinton))
TweetCorpus_clinton <- tm_map(TweetCorpus_clinton, PlainTextDocument)
TweetCorpus_clinton <- tm_map(TweetCorpus_clinton, removePunctuation)
TweetCorpus_clinton <- tm_map(TweetCorpus_clinton, removeWords, stopwords('english'))
#TweetCorpus <- tm_map(TweetCorpus, stemDocument) % No stemming for now!
TweetCorpus_clinton <- tm_map(TweetCorpus_clinton, content_transformer(tolower),lazy=TRUE) #convert all words to lower case
TweetCorpus_clinton <- tm_map(TweetCorpus_clinton, PlainTextDocument)
#wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)
text_clinton <- strsplit(TweetCorpus_clinton[[1]]$content, " ")[[1]]

#for Cruz
TweetCorpus_Cruz <- paste(unlist(tweet_cruz$text), collapse =" ") #to get all of the tweets together
TweetCorpus_Cruz <- Corpus(VectorSource(TweetCorpus_Cruz))
TweetCorpus_Cruz <- tm_map(TweetCorpus_Cruz, PlainTextDocument)
TweetCorpus_Cruz <- tm_map(TweetCorpus_Cruz, removePunctuation)
TweetCorpus_Cruz <- tm_map(TweetCorpus_Cruz, removeWords, stopwords('english'))
#TweetCorpus <- tm_map(TweetCorpus, stemDocument) % No stemming for now!
TweetCorpus_Cruz <- tm_map(TweetCorpus_Cruz, content_transformer(tolower),lazy=TRUE) #convert all words to lower case
TweetCorpus_Cruz <- tm_map(TweetCorpus_Cruz, PlainTextDocument)
#wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)
text_cruz <- strsplit(TweetCorpus_Cruz[[1]]$content, " ")[[1]]

#for Sanders
TweetCorpus_Sanders <- paste(unlist(tweet_sanders$text), collapse =" ") #to get all of the tweets together
TweetCorpus_Sanders <- Corpus(VectorSource(TweetCorpus_Sanders))
TweetCorpus_Sanders <- tm_map(TweetCorpus_Sanders, PlainTextDocument)
TweetCorpus_Sanders <- tm_map(TweetCorpus_Sanders, removePunctuation)
TweetCorpus_Sanders <- tm_map(TweetCorpus_Sanders, removeWords, stopwords('english'))
#TweetCorpus <- tm_map(TweetCorpus, stemDocument) % No stemming for now!
TweetCorpus_Sanders <- tm_map(TweetCorpus_Sanders, content_transformer(tolower),lazy=TRUE) #convert all words to lower case
TweetCorpus_Sanders <- tm_map(TweetCorpus_Sanders, PlainTextDocument)
#wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)
text_sanders <- strsplit(TweetCorpus_Sanders[[1]]$content, " ")[[1]]

#for Trump
TweetCorpus_Trump <- paste(unlist(tweet_trump$text), collapse =" ") #to get all of the tweets together
TweetCorpus_Trump <- Corpus(VectorSource(TweetCorpus_Trump))
TweetCorpus_Trump <- tm_map(TweetCorpus_Trump, PlainTextDocument)
TweetCorpus_Trump <- tm_map(TweetCorpus_Trump, removePunctuation)
TweetCorpus_Trump <- tm_map(TweetCorpus_Trump, removeWords, stopwords('english'))
#TweetCorpus <- tm_map(TweetCorpus, stemDocument) % No stemming for now!
TweetCorpus_Trump <- tm_map(TweetCorpus_Trump, content_transformer(tolower),lazy=TRUE) #convert all words to lower case
TweetCorpus_Trump <- tm_map(TweetCorpus_Trump, PlainTextDocument)
#wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)
text_trump <- strsplit(TweetCorpus_Trump[[1]]$content, " ")[[1]]

#for Democratic
TweetCorpus_democrat <- paste(unlist(tweet_democrat$text), collapse =" ") #to get all of the tweets together
TweetCorpus_democrat <- Corpus(VectorSource(TweetCorpus_democrat))
TweetCorpus_democrat <- tm_map(TweetCorpus_democrat, PlainTextDocument)
TweetCorpus_democrat <- tm_map(TweetCorpus_democrat, removePunctuation)
TweetCorpus_democrat <- tm_map(TweetCorpus_democrat, removeWords, stopwords('english'))
#TweetCorpus <- tm_map(TweetCorpus, stemDocument) % No stemming for now!
TweetCorpus_democrat <- tm_map(TweetCorpus_democrat, content_transformer(tolower),lazy=TRUE) #convert all words to lower case
TweetCorpus_democrat <- tm_map(TweetCorpus_democrat, PlainTextDocument)
#wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)
text_democrat <- strsplit(TweetCorpus_democrat[[1]]$content, " ")[[1]]

#for Republican
TweetCorpus_republican <- paste(unlist(tweet_republican$text), collapse =" ") #to get all of the tweets together
TweetCorpus_republican <- Corpus(VectorSource(TweetCorpus_republican))
TweetCorpus_republican <- tm_map(TweetCorpus_republican, PlainTextDocument)
TweetCorpus_republican <- tm_map(TweetCorpus_republican, removePunctuation)
TweetCorpus_republican <- tm_map(TweetCorpus_republican, removeWords, stopwords('english'))
#TweetCorpus <- tm_map(TweetCorpus, stemDocument) % No stemming for now!
TweetCorpus_republican <- tm_map(TweetCorpus_republican, content_transformer(tolower),lazy=TRUE) #convert all words to lower case
TweetCorpus_republican <- tm_map(TweetCorpus_republican, PlainTextDocument)
#wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)
text_republican <- strsplit(TweetCorpus_republican[[1]]$content, " ")[[1]]

#find the frequency of keywords within each topic
#lexicon and keywords definition
lexicon <- read.csv("lexicon.csv", stringsAsFactors = F) #import the lexicon file
eco.words <- lexicon$word[lexicon$topic=="economy"]
immig.words <- lexicon$word[lexicon$topic=="immigration"] ##workspace saved up to here
health.words <- lexicon$word[lexicon$topic=="healthcare"]
mili.words <- lexicon$word[lexicon$topic=="military"]
gun.words <- lexicon$word[lexicon$topic=="guncontrol"]
china.words <- lexicon$word[lexicon$topic=="china"]
trade.words <- lexicon$word[lexicon$topic=="trade"]
race.words <- lexicon$word[lexicon$topic=="race"]
climate.words <- lexicon$word[lexicon$topic=="climatechange"]
religion.words <- lexicon$word[lexicon$topic=="religion"]

# a function to classify individual tweets
classify <- function(TweetWords, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words){
  # count number of keyword matches for each topic
  economy <- sum(TweetWords %in% eco.words)
  immigration <- sum(TweetWords %in% immig.words)
  health <- sum(TweetWords %in% health.words)
  military <- sum(TweetWords %in% mili.words)
  immigration <- sum(TweetWords %in% immig.words)
  gun <- sum(TweetWords %in% gun.words)
  china <- sum(TweetWords %in% china.words)
  trade <- sum(TweetWords %in% trade.words)
  race <- sum(TweetWords %in% race.words)
  climate <- sum(TweetWords %in% climate.words)
  religion <- sum(TweetWords %in% religion.words)
  topic <- c("economy", "immigration", "health_care", "military", "gun_control", "china", "trade", "race", "climate_change", "religion")
  count_tweet <- c(economy,immigration, health, military, gun, china, trade, race, climate, religion)
  count.df = data.frame(topic, count_tweet)
  return(count.df)
}

#Use the above function to obtain frequency data for each candidate
classify(text_clinton, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq1 <- classify(text_clinton, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq1 <- data.frame(freq1)
freq_clinton <- write.table(freq1, file = "freq_Clinton.tsv",  sep = '\t', col.names = NA)

freq2 <- classify(text_cruz, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq2 <- data.frame(freq2)
freq_cruz <- write.table(freq2, file = "freq_Cruz.tsv",  sep = '\t', col.names = NA)

freq3 <- classify(text_sanders, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq3 <- data.frame(freq3)
freq_sanders <- write.table(freq3, file = "freq_Sanders.tsv",  sep = '\t', col.names = NA)

freq4 <- classify(text_trump, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq4 <- data.frame(freq4)
freq_trump <- write.table(freq4, file = "freq_Trump.tsv",  sep = '\t', col.names = NA)

#combine the candidates for question 1
freq_sanders_clinton <- merge(freq1, freq3, by = "topic")
names(freq_sanders_clinton)[c(1,2,3)] <- c("Topics", "Hilary Clinton", "Bernie Sanders")
write.table(freq_sanders_clinton, file='freq_sanders_clinton.tsv', sep='\t')
write.csv(freq_sanders_clinton, file="freq_sanders_clinton.csv", row.names=F)

freq_trump_cruz <- merge(freq2, freq4, by = "topic")
names(freq_trump_cruz)[c(1,2,3)] <- c("Topics", "Ted Cruz", "Donald Trump")
write.table(freq_trump_cruz, file='freq_trump_cruz.tsv', sep='\t')
write.csv(freq_trump_cruz, file="freq_trump_cruz.csv", row.names=F)

#Question 2: by party
classify(text_democrat, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq_D <- classify(text_democrat, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq_D <- data.frame(freq_D)
freq_democrat <- write.table(freq_D, file = "freq_democrat.tsv",  sep = '\t', col.names = NA)
names(freq_D) <- c("Topics", "Count")

classify(text_republican, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq_R <- classify(text_republican, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)
freq_R <- data.frame(freq_R)
freq_republican <- write.table(freq_R, file = "freq_republican.tsv",  sep = '\t', col.names = NA)
names(freq_R) <- c("Topics", "Count")

freq_party <- merge(freq_D, freq_R, by = "topic")
names(freq_party)[c(1,2,3)] <- c("Topics", "Democrat", "Republican")
write.table(freq_party, file='freq_party.tsv', sep='\t')
write.csv(freq_party, file="freq_party.csv", row.names=F)

# since the count for parties are not much, I also merge the candidates into them. Count = count for party + count for the two candidates
freq_D_all <- merge(freq_D, freq_sanders_clinton, by = "Topics")
freq_R_all <- merge(freq_R, freq_trump_cruz, by = "Topics")

numD <- rowSums(freq_D_all[,-1]) # sum up the three columns by topics
topic <- c("economy", "immigration", "health_care", "military", "gun_control", "china", "trade", "race", "climate_change", "religion")
freq_all_D <- data.frame(topic, numD)
freq_all_R <- data.frame(topic, rowSums(freq_R_all[, -1]))

freq_party_all <- merge(freq_all_R, freq_all_D, by = "topic")
names(freq_party_all)[c(1,2,3)] <- c("Topics", "Republican", "Democrat")
write.table(freq_party_all, file='freq_party_all.tsv', sep='\t')
write.csv(freq_party_all, file="freq_party_all.csv", row.names=F)

#Question 3: need to generate tweet of each candidate for each day. Process the daily tweet first and then merge by candidate
#Want to get the tsv file with rownames = Row and colnames = date, eco, china...(all 10 topics)

#function that divide the tweets by candidate
clinton_div <- function(tweet) { #input is the original tweets!
  tweet$text <- gsub("Hillary Clinton", "Clinton", tweet$text, ignore.case = TRUE)
  tweet$text <- gsub("hillaryclinton", "Clinton", tweet$text, ignore.case = TRUE)
  tweet$text <- gsub("HillaryClinton", "Clinton", tweet$text, ignore.case = TRUE)
  tweet$text <- gsub("hillary clinton", "Clinton", tweet$text, ignore.case = TRUE)
  
  clinton <- grep("Clinton", tweet$text, ignore.case = TRUE)
  tweet$index <- seq.int(nrow(tweet))
  tweet_clinton <- filter(tweet, index %in% clinton)
  return(tweet_clinton)
}

cruz_div <- function(tweets) {
  tweets$text <- gsub("Ted Cruz", "Cruz", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("tedcruz", "Cruz", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("TedCruz", "Cruz", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("ted cruz", "Cruz", tweets$text, ignore.case = TRUE)
  
  cruz <- grep("Cruz", tweets$text, ignore.case = TRUE)
  tweets$index <- seq.int(nrow(tweets))
  tweet_cruz <- filter(tweets, index %in% cruz)
  return(tweet_cruz)
}

sanders_div <- function(tweets) {
  tweets$text <- gsub("Bernie Sanders", "Sanders", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("berniesanders", "Sanders", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("BernieSanders", "Sanders", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("bernie sanders", "Sanders", tweets$text, ignore.case = TRUE)
  
  sanders <- grep("Sanders", tweets$text, ignore.case = TRUE)
  tweets$index <- seq.int(nrow(tweets))
  tweet_sanders <- filter(tweets, index %in% sanders)
  return(tweet_sanders)
}

trump_div <- function(tweets) {
  tweets$text <- gsub("Donald Trump", "Trump", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("donaldtrump", "Trump", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("DonaldTrump", "Trump", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("donald trump", "Trump", tweets$text, ignore.case = TRUE)
  
  trump <- grep("trump", tweets$text, ignore.case = TRUE)
  tweets$index <- seq.int(nrow(tweets))
  tweet_trump <- filter(tweets, index %in% trump)
  return(tweet_trump)
}
#Clean the daily tweets into corpus

clean_function <- function(text){ #input is the text part of the tweets!!
  text <- sapply(text, function(row) iconv(row, "latin1", "ASCII", sub=""))
  Corpus <- paste(unlist(text), collapse =" ")
  Corpus <- Corpus(VectorSource(Corpus))
  Corpus <- tm_map(Corpus, PlainTextDocument)
  Corpus <- tm_map(Corpus, removePunctuation)
  Corpus <- tm_map(Corpus, removeWords, stopwords('english'))
  Corpus <- tm_map(Corpus, content_transformer(tolower),lazy=TRUE) #convert all words to lower case
  Corpus <- tm_map(Corpus, PlainTextDocument)
  TweetWord <- strsplit(Corpus[[1]]$content, " ")[[1]]
  return(TweetWord)
}

#have all the functions, yeah! now get the daily topic count for each candidate: div_function - clean_function - classify
#Clinton
cl316 <- clinton_div(tweets_316)
cl316_text <- clean_function(cl316$text)
freq_cl316 <- classify(cl316_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl317 <- clinton_div(tweets_317)
cl317_text <- clean_function(cl317$text)
freq_cl317 <- classify(cl317_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl318 <- clinton_div(tweets_318)
cl318_text <- clean_function(cl318$text)
freq_cl318 <- classify(cl318_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl319 <- clinton_div(tweets_319)
cl319_text <- clean_function(cl319$text)
freq_cl319 <- classify(cl319_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl320 <- clinton_div(tweets_320)
cl320_text <- clean_function(cl320$text)
freq_cl320 <- classify(cl320_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl321 <- clinton_div(tweets_321)
cl321_text <- clean_function(cl321$text)
freq_cl321 <- classify(cl321_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl322 <- clinton_div(tweets_322)
cl322_text <- clean_function(cl322$text)
freq_cl322 <- classify(cl322_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl323 <- clinton_div(tweets_323)
cl323_text <- clean_function(cl323$text)
freq_cl323 <- classify(cl323_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl324 <- clinton_div(tweets_324)
cl324_text <- clean_function(cl324$text)
freq_cl324 <- classify(cl324_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl325 <- clinton_div(tweets_325)
cl325_text <- clean_function(cl325$text)
freq_cl325 <- classify(cl325_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

cl1 <- merge(freq_cl316, freq_cl317, by = "topic")
cl2 <- merge(cl1, freq_cl318, by = "topic")
cl3 <- merge(cl2, freq_cl319, by = "topic")
cl4 <- merge(cl3, freq_cl320, by = "topic")
cl5 <- merge(cl4, freq_cl321, by = "topic")
cl6 <- merge(cl5, freq_cl322, by = "topic")
cl7 <- merge(cl6, freq_cl323, by = "topic")
cl8 <- merge(cl7, freq_cl324, by = "topic")
cl9 <- merge(cl8, freq_cl325, by = "topic")
names(cl9)[c(1,2,3,4,5,6,7,8,9,10,11)] <- c("topic", "316", "317", "318", "319", "320", "321", "322", "323", "324", "325", "326")
cl <- t(cl9)
write.table(data.frame(cl), file='cl.tsv', sep='\t')
write.csv(data.frame(cl), file="cl.csv", row.names=F)

#in the same way, get the tsv files for the other three candidates.

#For Democrat
democrat_div <- function(tweets) {
  
  tweets$text <- gsub("Hillary Clinton", "Clinton", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("hillaryclinton", "Clinton", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("HillaryClinton", "Clinton", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("hillary clinton", "Clinton", tweets$text, ignore.case = TRUE)
  
  tweets$text <- gsub("Bernie Sanders", "Sanders", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("berniesanders", "Sanders", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("BernieSanders", "Sanders", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("bernie sanders", "Sanders", tweets$text, ignore.case = TRUE)
  
  tweets$text <- gsub("Democrat", "democrat", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("Republican", "republican", tweets$text, ignore.case = TRUE)

  clinton <- grep("Clinton", tweets$text, ignore.case = TRUE)
  sanders <- grep("Sanders", tweets$text, ignore.case = TRUE)
  Democrat <- grep("democrat", tweets$text, ignore.case = TRUE)
  all1 <- c(clinton, sanders, Democrat)
  tweets$index <- seq.int(nrow(tweets))
  tweet_democrat <- filter(tweets, index %in% all1)
  return(tweet_democrat)
}

democrat316 <- democrat_div(tweets_316)
democrat316_text <- clean_function(democrat316$text)
freq_democrat316 <- classify(democrat316_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat317 <- democrat_div(tweets_317)
democrat317_text <- clean_function(democrat317$text)
freq_democrat317 <- classify(democrat317_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat318 <- democrat_div(tweets_318)
democrat318_text <- clean_function(democrat318$text)
freq_democrat318 <- classify(democrat318_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat319 <- democrat_div(tweets_319)
democrat319_text <- clean_function(democrat319$text)
freq_democrat319 <- classify(democrat319_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat320 <- democrat_div(tweets_320)
democrat320_text <- clean_function(democrat320$text)
freq_democrat320 <- classify(democrat320_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat321 <- democrat_div(tweets_321)
democrat321_text <- clean_function(democrat321$text)
freq_democrat321 <- classify(democrat321_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat322 <- democrat_div(tweets_322)
democrat322_text <- clean_function(democrat322$text)
freq_democrat322 <- classify(democrat322_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat323 <- democrat_div(tweets_323)
democrat323_text <- clean_function(democrat323$text)
freq_democrat323 <- classify(democrat323_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat324 <- democrat_div(tweets_324)
democrat324_text <- clean_function(democrat324$text)
freq_democrat324 <- classify(democrat324_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat325 <- democrat_div(tweets_325)
democrat325_text <- clean_function(democrat325$text)
freq_democrat325 <- classify(democrat325_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

democrat1 <- merge(freq_democrat316, freq_democrat317, by = "topic")
democrat2 <- merge(democrat1, freq_democrat318, by = "topic")
democrat3 <- merge(democrat2, freq_democrat319, by = "topic")
democrat4 <- merge(democrat3, freq_democrat320, by = "topic")
democrat5 <- merge(democrat4, freq_democrat321, by = "topic")
democrat6 <- merge(democrat5, freq_democrat322, by = "topic")
democrat7 <- merge(democrat6, freq_democrat323, by = "topic")
democrat8 <- merge(democrat7, freq_democrat324, by = "topic")
democrat9 <- merge(democrat8, freq_democrat325, by = "topic")
names(democrat9)[c(1,2,3,4,5,6,7,8,9,10,11)] <- c("topic", "316", "317", "318", "319", "320", "321", "322", "323", "324", "325", "326")
democrat <- t(democrat9)
write.table(data.frame(democrat), file='democrat.tsv', sep='\t')

#For Republican
republican_div <- function(tweets) {
  
  tweets$text <- gsub("Donald Trump", "Trump", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("donaldtrump", "Trump", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("DonaldTrump", "Trump", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("donald trump", "Trump", tweets$text, ignore.case = TRUE)
  
  tweets$text <- gsub("Ted Cruz", "Cruz", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("tedcruz", "Cruz", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("TedCruz", "Cruz", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("ted cruz", "Cruz", tweets$text, ignore.case = TRUE)
  
  tweets$text <- gsub("Republican", "republican", tweets$text, ignore.case = TRUE)
  tweets$text <- gsub("Republican Party", "republican", tweets$text, ignore.case = TRUE)
  
  trump <- grep("Trump", tweets$text, ignore.case = TRUE)
  cruz <- grep("Cruz", tweets$text, ignore.case = TRUE)
  Republican <- grep("republican", tweets$text, ignore.case = TRUE)
  all2 <- c(trump, cruz, Republican)
  tweets$index <- seq.int(nrow(tweets))
  tweet_republican <- filter(tweets, index %in% all2)
  return(tweet_republican)
}

republican316 <- republican_div(tweets_316)
republican316_text <- clean_function(republican316$text)
freq_republican316 <- classify(republican316_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican317 <- republican_div(tweets_317)
republican317_text <- clean_function(republican317$text)
freq_republican317 <- classify(republican317_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican318 <- republican_div(tweets_318)
republican318_text <- clean_function(republican318$text)
freq_republican318 <- classify(republican318_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican319 <- republican_div(tweets_319)
republican319_text <- clean_function(republican319$text)
freq_republican319 <- classify(republican319_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican320 <- republican_div(tweets_320)
republican320_text <- clean_function(republican320$text)
freq_republican320 <- classify(republican320_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican321 <- republican_div(tweets_321)
republican321_text <- clean_function(republican321$text)
freq_republican321 <- classify(republican321_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican322 <- republican_div(tweets_322)
republican322_text <- clean_function(republican322$text)
freq_republican322 <- classify(republican322_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican323 <- republican_div(tweets_323)
republican323_text <- clean_function(republican323$text)
freq_republican323 <- classify(republican323_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican324 <- republican_div(tweets_324)
republican324_text <- clean_function(republican324$text)
freq_republican324 <- classify(republican324_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican325 <- republican_div(tweets_325)
republican325_text <- clean_function(republican325$text)
freq_republican325 <- classify(republican325_text, eco.words, immig.words, health.words, mili.words, gun.words, china.words, trade.words, race.words, climate.words, religion.words)

republican1 <- merge(freq_republican316, freq_republican317, by = "topic")
republican2 <- merge(republican1, freq_republican318, by = "topic")
republican3 <- merge(republican2, freq_republican319, by = "topic")
republican4 <- merge(republican3, freq_republican320, by = "topic")
republican5 <- merge(republican4, freq_republican321, by = "topic")
republican6 <- merge(republican5, freq_republican322, by = "topic")
republican7 <- merge(republican6, freq_republican323, by = "topic")
republican8 <- merge(republican7, freq_republican324, by = "topic")
republican9 <- merge(republican8, freq_republican325, by = "topic")
names(republican9)[c(1,2,3,4,5,6,7,8,9,10,11)] <- c("topic", "316", "317", "318", "319", "320", "321", "322", "323", "324", "325", "326")
republican <- t(republican9)
write.table(data.frame(republican), file='republican.tsv', sep='\t')
write.csv(data.frame(republican), file="republican.csv", row.names=F)