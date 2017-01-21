load.image("visfinal2.RData")


t316 <- read.csv("tweets.03.16.2016.summary.csv")
t317 <- read.csv("tweets.03.17.2016.summary.csv")
t318 <- read.csv("tweets.03.18.2016.summary.csv")
t319 <- read.csv("tweets.03.19.2016.summary.csv")
t320 <- read.csv("tweets.03.20.2016.summary.csv")
t321 <- read.csv("tweets.03.21.2016.summary.csv")
t322 <- read.csv("tweets.03.22.2016.summary.csv")
t323 <- read.csv("tweets.03.23.2016.summary.csv")
t324 <- read.csv("tweets.03.24.2016.summary.csv")
t325 <- read.csv("tweets.03.25.2016.summary.csv")
t326 <- read.csv("tweets.03.26.2016.summary.csv")
t327 <- read.csv("tweets.03.27.2016.summary.csv")
t328 <- read.csv("tweets.03.28.2016.summary.csv")
t329 <- read.csv("tweets.03.29.2016.summary.csv")
t330 <- read.csv("tweets.03.30.2016.summary.csv")


df_names <- ls(pattern = "t")
t1 <- do.call(rbind, mget(df_names))

s316 <- t316[sample(nrow(t316), 5000), ]
s317 <- t317[sample(nrow(t317), 5000), ]
s318 <- t318[sample(nrow(t318), 5000), ]
s319 <- t319[sample(nrow(t319), 5000), ]
s320 <- t320[sample(nrow(t320), 5000), ]
s321 <- t321[sample(nrow(t321), 5000), ]
s322 <- t322[sample(nrow(t322), 5000), ]
s323 <- t323[sample(nrow(t323), 5000), ]
s324 <- t324[sample(nrow(t324), 5000), ]
s325 <- t325[sample(nrow(t325), 5000), ]
s326 <- t326[sample(nrow(t326), 5000), ]
s327 <- t327[sample(nrow(t327), 5000), ]
s328 <- t328[sample(nrow(t328), 5000), ]
s329 <- t329[sample(nrow(t329), 5000), ]
s330 <- t330[sample(nrow(t330), 5000), ]
s401 <- t401[sample(nrow(t401), 5000), ]
s402 <- t402[sample(nrow(t402), 5000), ]
s403 <- t403[sample(nrow(t403), 5000), ]
s404 <- t404[sample(nrow(t404), 5000), ]
s405 <- t405[sample(nrow(t405), 5000), ]
s406 <- t406[sample(nrow(t406), 5000), ]
s407 <- t407[sample(nrow(t407), 5000), ]
s408 <- t408[sample(nrow(t408), 5000), ]
s409 <- t409[sample(nrow(t409), 5000), ]
s410 <- t410[sample(nrow(t410), 5000), ]
s411 <- t411[sample(nrow(t411), 5000), ]
s412 <- t412[sample(nrow(t412), 5000), ]
s413 <- t413[sample(nrow(t413), 5000), ]
s414 <- t414[sample(nrow(t414), 5000), ]
s415 <- t415[sample(nrow(t415), 5000), ]


load("apriltweets.RData")

load("417.RData")


slist = lapply(ls(pattern = "s3"), get)
s <- do.call("rbind", slist)
slist2 = lapply(ls(pattern = "s4"), get)




vars <- c("location","text","created_at")
for (i in 1:15){
  slist2[[i]] <- slist2[[i]][vars]
}
s2 <- do.call("rbind", slist2)

s315415 <- rbind(s,s2)


s2$text <- sapply(s2$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

s1 <- read.csv("s316330.csv")

#gsub
s2$text <- gsub("Bernie Sanders","Sanders", s2$text, ignore.case = T)
s2$text <- gsub("Berniesanders","Sanders", s2$text, ignore.case = T)
s2$text <- gsub("Bernie","Sanders", s2$text, ignore.case = T)
s2$text <- gsub("Hillary Clinton","Clinton", s2$text, ignore.case = T)
s2$text <- gsub("HillaryClinton","Clinton", s2$text, ignore.case = T)
s2$text <- gsub("Hillary","Clinton", s2$text, ignore.case = T)
s2$text <- gsub("Ted Cruz","Cruz", s2$text, ignore.case = T)
s2$text <- gsub("TedCruz","Cruz", s2$text, ignore.case = T)
s2$text <- gsub("Ted","Cruz", s2$text, ignore.case = T)
s2$text <- gsub("Donald Trump","Trump", s2$text, ignore.case = T)
s2$text <- gsub("DonaldTrump","Trump", s2$text, ignore.case = T)
s2$text <- gsub("Donald","Trump", s2$text, ignore.case = T)


patternAll <- "Cruz|Trump|Clinton|Sanders"
s2 <- s2[grep(patternAll,s2$text,ignore.case = T),]


#candidate variable
s2$candidate <-   ifelse(grepl("Clinton",s2$text) == T , "Clinton", 
                        ifelse(grepl("Sanders",s2$text) == T, "Sanders",
                               ifelse(grepl("Trump",s2$text) == T, "Trump",
                                      ifelse(grepl("Cruz",s2$text) == T, "Cruz", "NA"))))

patternD <- "Clinton|Sanders"


#partyid
s2$partyid <- ifelse(grepl(patternD,s2$text) == T , "Democrat", "Republican")

#topic
#military: military, army, soldier, war, troop
pMilitary <- "military|army|soldier|war|troop"
#Economy
pEconomy <- "economy|job|wage|tax|income|debt|loan|employment|trade|import|export|tpp|tpa|business|economic|financial|finance"
#Immigration
pImmi <- "immigrant|legislation|refugee|border|citizen|immigration|citizenship|resident|deport"
#Health Care
pHealth <- "insurance|medical|obamacare|afford|health|medicare|hospital|affordable|medicine"
#Gun control: gun, second amendment, arms, constitution, weapons
pGun <- "gun|amendment|arms|constitution|weapon|violence"
#Race/Ethnicity: African Americans, Hispanic, Middle Eastern, Black, race
pRace <- "African|Hispanic|Middle Eastern|Black|race|white|Latinos|racist|racism|Jew|Jewish|#compaignzero|#blacklivesmatter"
#Gender/Sex
pGender <- "gender|sex|women|sexist|sexism|gay|homosexual|women's rights|same sex|samesex|misogyny|misogynist|chauvinist"
#Climate change: global warming, climate change, environment, sea level, emissions, fossil
pClimate <- "warming|climate|environment|sea|emission|fossil"
#religion: islam, islamic, christian, muslim, God
pReligion <- "islam|religion|christian|muslim|God|islamic|abortion"
#International politics
pInt <- "Arabic|isarel|palestine|foreign|syria|Arab|Africa|Asian|Taiwan|African|Germany|France|German|French|Brussels|Belgium|China|Asia|Chinese|human rights|cyber|beijing|European|Europe|Middle East|Iran|North Korea|ISIS|Iraq|afghanistan"
#verbal attack
pVer <- "idiot|fraud|dumbass|puppet|ignorant|dickhead|dick|suck|fake|disgusting|criminal|crook|sociopath|insance|mad|stupid|fuck|damn|bitch|fucking|ass|retard|hate|hell|loser|asshole|scum|sexist|sexism|puke"
#Election News
pElec <- "USA Today|supertuesday|CBS|hillaryemails|election2016|compaign|wikileaks|turnout|vote|republican|democrat|voter|partisan|nytimes|newsroom|nomination|delegate|TIME magazine|beat|speech|endorse|poll|predictions|caucus|primary|primaries|CNN|result|win|lose|lost|loses|wins|won|election|debate|Bloomberg|NY Times|Fox|New York Times"
#show support
pSupport <- "feelthebern|nevertrump|bernie2016|hillary2016|neverclinton|neverhillary|neversanders|wethepeople|bern|makeamericagreatagain|unitewithcruz|congratulation|politicalrevolution|we just won|alwaystrump|hillary2016|birdiesanders|alwaysclinton|neverclinton|keepmovingforward|sanders2016|trump2016|clinton2016|cruz2016|notcruz|notclinton|nottrump|notsanders|politicalstreetart|stillsanders|sandersorbust"

#by topic
s2$topic <- ifelse(grepl(pMilitary,s2$text, ignore.case = T) == T , "military", 
                       ifelse(grepl(pSupport,s2$text,ignore.case = T) == T, "support",
                              ifelse(grepl(pInt,s2$text,ignore.case = T) == T, "international",
                  ifelse(grepl(pEconomy,s2$text,ignore.case = T) == T, "economy",
                         ifelse(grepl(pVer,s2$text,ignore.case = T) == T, "verbalattack",
                                ifelse(grepl(pElec,s2$text,ignore.case = T) == T, "election",
                         ifelse(grepl(pReligion,s2$text,ignore.case = T) == T, "religion",
                                ifelse(grepl(pImmi,s2$text,ignore.case = T) == T, "immigration",
                                       ifelse(grepl(pHealth,s2$text,ignore.case = T) == T, "healthcare",
                                              ifelse(grepl(pGun,s2$text,ignore.case = T) == T, "guncontrol",
                                                     ifelse(grepl(pGender,s2$text,ignore.case = T) == T, "gender",
                                                            
                                                                   ifelse(grepl(pRace,s2$text,ignore.case = T) == T, "race",
                                                                          ifelse(grepl(pClimate,s2$text,ignore.case = T) == T, "climatechange","NA")))))))))))))



s315415$date <-  as.POSIXct(s315415$created_at, format="%a %b %d %H:%M:%S %z %Y")
s315415$date <- gsub("2016-","",s315415$date)
s315415$date <- gsub("-","",s315415$date)
s315415$date <- gsub(":","",s315415$date)
s315415$date <- gsub(" ","",s315415$date)
s315415$date <- substr(s315415$date, 1, nchar(s315415$date)-1)
s315415$date <- substr(s315415$date, 1, nchar(s315415$date)-1)




s2$military <- ifelse(grepl("military",s2$topic) == T , 1,0)
s2$religion <- ifelse(grepl("religion",s2$topic) == T , 1,0)

s315415$location[which(s315415$location == "Seattle")] = "Washington"
s315415$location[which(s315415$location == "Baltimore  MD")] = "Maryland"
s315415$location[which(s315415$location == "Republic of Texas")] = "Texas"
s315415$location[which(s315415$location == "NJ")] = "New Jersey"
s315415$location[which(s315415$location == "Orlando  FL")] = "Florida"
s315415$location[which(s315415$location == "Columbus OH")] = "Ohio"
s315415$location[which(s315415$location == "Los Angeles  CA")] = "California"
s315415$location[which(s315415$location == "Chicago  IL")] = "Illinois"
s315415$location[which(s315415$location == "Los Angeles")] = "California"
s315415$location[which(s315415$location == "Houston  TX")] = "Texas"
s315415$location[which(s315415$location == "NYC")] = "New York"
s315415$location[which(s315415$location == "NC")] = "North Carolina"
s315415$location[which(s315415$location == "New York  NY")] = "New York"
s315415$location[which(s315415$location == "Atlanta  GA")] = "Georgia"
s315415$location[which(s315415$location == "San Diego  USA")] = "California"
s315415$location[which(s315415$location == "Los Angeles  California")] = "California"
s315415$location[which(s315415$location == "Raleigh  NC")] = "North Carolina"
s315415$location[which(s315415$location == "Austin  Texas")] = "Texas"
s315415$location[which(s315415$location == "Detroit  MI")] = "Michigan"
s315415$location[which(s315415$location == "Tuscon  AZ")] = "Arizona"
s315415$location[which(s315415$location == "Minneapolis  MN")] = "Minnesota"
s315415$location[which(s315415$location == "Cleveland  OH")] = "Ohio"
s315415$location[which(s315415$location == "Las Vegas")] = "Nevada"
s315415$location[which(s315415$location == "Columbus  OH")] = "Ohio"
s315415$location[which(s315415$location == "Dallas TX")] = "Texas"
s315415$location[which(s315415$location == "Memphis  TN")] = "Tennessee"

s315415$location[which(s315415$location == "North Carolina  USA")] = "North Carolina"
s315415$location[which(s315415$location == "New Jersey  USA")] = "New Jersey"
s315415$location[which(s315415$location == "Alabama  USA")] = "Alabama"
s315415$location[which(s315415$location == "California  USA")] = "California"
s315415$location[which(s315415$location == "Florida  USA")] = "Florida"
s315415$location[which(s315415$location == "Kentucky  USA")] = "Kentucky"
s315415$location[which(s315415$location == "Texas  USA")] = "Texas"
s315415$location[which(s315415$location == "New York  USA")] = "New York"
s315415$location[which(s315415$location == "Pennsylvania  USA")] = "Pennsylvania"
s315415$location[which(s315415$location == "New Hampshire  USA")] = "New Hampshire"
s315415$location[which(s315415$location == "Tennessee  USA")] = "Tennessee"
s315415$location[which(s315415$location == "Georgia  USA")] = "Georgia"
s315415$location[which(s315415$location == "Ohio  USA")] = "Ohio"
s315415$location[which(s315415$location == "Michigan  USA")] = "Michigan"
s315415$location[which(s315415$location == "Louisiana  USA")] = "Louisiana"
s315415$location[which(s315415$location == "Arizona USA")] = "Arizona"
s315415$location[which(s315415$location == "Arkansas USA")] = "Arkansas"
s315415$location[which(s315415$location == "Connecticut USA")] = "Connecticut"
s315415$location[which(s315415$location == "Illinois  USA")] = "Illinois"
s315415$location[which(s315415$location == "Kansas  USA")] = "Kansas"
s315415$location[which(s315415$location == "Minnesota  USA")] = "Minnesota"
s315415$location[which(s315415$location == "Montana  USA")] = "Montana"
s315415$location[which(s315415$location == "Nebraska  USA")] = "Nebraska"
s315415$location[which(s315415$location == "North Dakota  USA")] = "North Dakota"
s315415$location[which(s315415$location == "Philadelphia  PA")] = "Pennsylvania"
s315415$location[which(s315415$location == "San Francisco  CA")] = "California"
s315415$location[which(s315415$location == "South Dakota  USA")] = "South Dakota"
s315415$location[which(s315415$location == "Utah  USA")] = "Utah"
s315415$location[which(s315415$location == "West Virginia  USA")] = "West Virginia"
s315415$location[which(s315415$location == "Mississipi  USA")] = "Mississipi"
s315415$location[which(s315415$location == "Iowa  USA")] = "Iowa"
s315415$location[which(s315415$location == "Virginia  USA")] = "Virginia"
s315415$location[which(s315415$location == "Maine  USA")] = "Maine"
s315415$location[which(s315415$location == "Idaho  USA")] = "Idaho"
s315415$location[which(s315415$location == "Vermont  USA")] = "Vermont"
s315415$location[which(s315415$location == "Delaware  USA")] = "Delaware"
s315415$location[which(s315415$location == "South Carolina  USA")] = "South Carolina"
s315415$location[which(s315415$location == "New Mexico  USA")] = "New Mexico"
s315415$location[which(s315415$location == "Colorado  USA")] = "Colorado"
s315415$location[which(s315415$location == "Oregon  USA")] = "Oregon"
s315415$location[which(s315415$location == "Nevada  USA")] = "Nevada"
s315415$location[which(s315415$location == "Wyoming  USA")] = "Wyoming"
s315415$location[which(s315415$location == "Oklahoma  USA")] = "Oklahoma"
s315415$location[which(s315415$location == "Iowa  USA")] = "Iowa"
s315415$location[which(s315415$location == "Maryland  USA")] = "Maryland"
s315415$location[which(s315415$location == "Massachusetts  USA")] = "Massachusetts"
s315415$location[which(s315415$location == "Alaska  USA")] = "Alaska"
s315415$location[which(s315415$location == "Hawaii  USA")] = "Hawaii"
s315415$location[which(s315415$location == "Indiana  USA")] = "Indiana"
s315415$location[which(s315415$location == "Washington  USA")] = "Washington"
s315415$location[which(s315415$location == "Wisconsin  USA")] = "Wisconsin"





write.csv(s2,"s2.csv")


s1 <- read.csv("s316330.csv")
s2 <- read.csv("s2.csv")
s315415 <- rbind(s1,s2)

write.csv(s315415, "s315415.csv")


test <- test[which(complete.cases(test$locatio)),]

test <- s315415[sample(nrow(s315415), 300), ]




library(tm)
library(wordcloud)
NAs <- sample[which(s2$topic=="NA"),]
wcNA <- paste(unlist(NAs$text), collapse =" ")
wcNA <- Corpus(VectorSource(wcNA))
wcNA<- tm_map(wcNA, PlainTextDocument)
wcNA<- tm_map(wcNA, removePunctuation)
wcNA<- tm_map(wcNA, removeWords, c(stopwords('english'),"https","http","httpst","can","this","htt","hes","its","the","it","he","she"))
wcNA <- tm_map(wcNA, content_transformer(tolower),lazy=TRUE)
png("wcNA.png", width=12,height=8, units='in', res=500)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(wcNA, color = pal2, max.words = 500, scale = c(4, 0.2),random.order = FALSE)
dev.off()
