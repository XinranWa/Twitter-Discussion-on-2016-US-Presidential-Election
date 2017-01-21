x <- c("ggmap", "rgdal", "rgeos", "maptools", "plyr", "dplyr", "tidyr", "tmap", "GISTools")
install.packages(x) 
lapply(x, library, character.only = TRUE)
install.packages("streamR")
library(streamR)
library(GISTools)

#Parse the summary tweets using streamR library command
t1 <- parseTweets("tweets.02.09.2016.summary.json") #each has between 100,000-300,000 tweets, 43 variables
t2 <- parseTweets("tweets.02.20.2016.summary.json")
t3 <- parseTweets("tweets.02.23.2016.summary.json")
t4 <- parseTweets("tweets.02.27.2016.summary.json")
t5 <- parseTweets("tweets.03.01.2016.summary.json")
t6 <- parseTweets("tweets.03.05.2016.summary.json")
t7 <- parseTweets("tweets.03.06.2016.summary.json")
t8 <- parseTweets("tweets.03.08.2016.summary.json")
t9 <- parseTweets("tweets.03.15.2016.summary.json")

#Dividing by candidates
t1$text <- sapply(t1$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

#Combine similar keywords
t1$text <- gsub("Hillary Clinton", "Clinton", t1$text, ignore.case = TRUE)
t1$text <- gsub("hillaryclinton", "Clinton", t1$text, ignore.case = TRUE)
t1$text <- gsub("HillaryClinton", "Clinton", t1$text, ignore.case = TRUE)
t1$text <- gsub("hillary clinton", "Clinton", t1$text, ignore.case = TRUE)

t1$text <- gsub("Bernie Sanders", "Sanders", t1$text, ignore.case = TRUE)
t1$text <- gsub("berniesanders", "Sanders", t1$text, ignore.case = TRUE)
t1$text <- gsub("BernieSanders", "Sanders", t1$text, ignore.case = TRUE)
t1$text <- gsub("bernie sanders", "Sanders", t1$text, ignore.case = TRUE)

t1$text <- gsub("Donald Trump", "Trump", t1$text, ignore.case = TRUE)
t1$text <- gsub("donaldtrump", "Trump", t1$text, ignore.case = TRUE)
t1$text <- gsub("DonaldTrump", "Trump", t1$text, ignore.case = TRUE)
t1$text <- gsub("donald trump", "Trump", t1$text, ignore.case = TRUE)

t1$text <- gsub("Ted Cruz", "Cruz", t1$text, ignore.case = TRUE)
t1$text <- gsub("tedcruz", "Cruz", t1$text, ignore.case = TRUE)
t1$text <- gsub("TedCruz", "Cruz", t1$text, ignore.case = TRUE)
t1$text <- gsub("ted cruz", "Cruz", t1$text, ignore.case = TRUE)

t1$text <- gsub("Marco Rubio", "Rubio", t1$text, ignore.case = TRUE)
t1$text <- gsub("marcorubio", "Rubio", t1$text, ignore.case = TRUE)
t1$text <- gsub("MarcoRubio", "Rubio", t1$text, ignore.case = TRUE)
t1$text <- gsub("marco rubio", "Rubio", t1$text, ignore.case = TRUE)

#parse the tweets by candidates
clinton <- grep("Cruz", t1$text, ignore.case = TRUE)
t1$index <- seq.int(nrow(t1))
freq_clinton1 <- filter(t1, index %in% clinton)

#combine the tweets of nine days for each candidate
freq_clinton <- rbind(freq_clinton1, freq_clinton2, freq_clinton3, freq_clinton4, freq_clinton5, freq_clinton6, freq_clinton7, freq_clinton8, freq_clinton9)
freq_clinton <- as.data.frame(freq_clinton) #187747 obs of 43 variables

freq_cruz <- rbind(freq_cruz1, freq_cruz2, freq_cruz3, freq_cruz4, freq_cruz5, freq_cruz6, freq_cruz7, freq_cruz8, freq_cruz9)
freq_cruz <- as.data.frame(freq_cruz) #226911 obs of 43 variables

freq_rubio <- rbind(freq_rubio1, freq_rubio2, freq_rubio3, freq_rubio4, freq_rubio5, freq_rubio6, freq_rubio7, freq_rubio8, freq_rubio9)
freq_rubio <- as.data.frame(freq_rubio) #143984 obs of 43 variables

freq_sanders <- rbind(freq_sanders1, freq_sanders2, freq_sanders3, freq_sanders4, freq_sanders5, freq_sanders6, freq_sanders7, freq_sanders8, freq_sanders9)
freq_sanders <- as.data.frame(freq_sanders) #226911 obs of 43 variables

freq_trump <- rbind(freq_trump1, freq_trump2, freq_trump3, freq_trump4, freq_trump5, freq_trump6, freq_trump7, freq_trump8, freq_trump9)
freq_trump <- as.data.frame(freq_trump) #678720 obs of 43 variables

#make the plot that shows the origins of the tweets
all_states <- map_data("state")
plot(all_states)
ggplot(all_states) + geom_map(aes(map_id = region), map = all_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = all_states$long, y = all_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = freq_clinton, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(all_states) + geom_map(aes(map_id = region), map = all_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = all_states$long, y = all_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = freq_cruz, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(all_states) + geom_map(aes(map_id = region), map = all_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = all_states$long, y = all_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = freq_rubio, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(all_states) + geom_map(aes(map_id = region), map = all_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = all_states$long, y = all_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = freq_sanders, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(all_states) + geom_map(aes(map_id = region), map = all_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = all_states$long, y = all_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = freq_trump, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")

#make the color code map by proportion of tweets for question 2
tw_coordinates_clinton<- cbind(freq_clinton$place_lon, freq_clinton$place_lat)
tw_coordinates_clinton2 <- na.omit(tw_coordinates_clinton)
tw_points_clinton <- SpatialPoints(tw_coordinates_clinton2) #convert to SpatialPoints
plot(tw_points_clinton)

#extract U.S map into a spatialpolygon object
all_states <- map_data("state")
plot(all_states)

require(maps)
usa <- map("state", fill = TRUE)

require(sp)
require(maptools)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84")) #usa is a class of SpatialPolygon

count_clinton <- poly.counts(tw_points_clinton, usa) # present the tweet frequency in each state in a table (for question 2)
prop_clinton <- prop.table(count_clinton) # convert that to proportion

#color code map
usa$valueclinton <- prop_clinton
cols_clinton <- brewer.pal(n = 9, name = "GnBu") #available colors are BuGn, BuPu, GnBu, OrRd...
lcols_clinton <- cut(usa$valueclinton, breaks = 9, labels = cols_clinton)
plot(usa, col = as.character(lcols_clinton), xlab = "Color Map for Clinton")

#Cruz
tw_coordinates_cruz<- cbind(freq_cruz$place_lon, freq_cruz$place_lat)
tw_coordinates_cruz2 <- na.omit(tw_coordinates_cruz)
tw_points_cruz <- SpatialPoints(tw_coordinates_cruz2) #convert to SpatialPoints
plot(tw_points_cruz)

count_cruz <- poly.counts(tw_points_cruz, usa) # present the tweet frequency in each state in a table (for question 2)
prop_cruz <- prop.table(count_cruz) 

usa$valuecruz <- prop_cruz
cols_cruz <- brewer.pal(n = 9, name = "RdPu") #available colors are BuGn, BuPu, GnBu, OrRd...
lcols_cruz <- cut(usa$valuecruz, breaks = 9, labels = cols_cruz)
plot(usa, col = as.character(lcols_cruz), xlab = "Color Map for Cruz")

#Rubio
tw_coordinates_rubio<- cbind(freq_rubio$place_lon, freq_rubio$place_lat)
tw_coordinates_rubio2 <- na.omit(tw_coordinates_rubio)
tw_points_rubio <- SpatialPoints(tw_coordinates_rubio2) #convert to SpatialPoints
plot(tw_points_rubio)

count_rubio <- poly.counts(tw_points_rubio, usa) # present the tweet frequency in each state in a table (for question 2)
prop_rubio <- prop.table(count_rubio) 

usa$valuerubio <- prop_rubio
cols_rubio <- brewer.pal(n = 9, name = "Reds") #available colors are BuGn, BuPu, GnBu, OrRd...
lcols_rubio <- cut(usa$valuerubio, breaks = 9, labels = cols_rubio)
plot(usa, col = as.character(lcols_rubio), xlab = "Color Map for Rubio")

#Sanders
tw_coordinates_sanders<- cbind(freq_sanders$place_lon, freq_sanders$place_lat)
tw_coordinates_sanders2 <- na.omit(tw_coordinates_sanders)
tw_points_sanders <- SpatialPoints(tw_coordinates_sanders2) #convert to SpatialPoints
plot(tw_points_sanders)

count_sanders <- poly.counts(tw_points_sanders, usa) # present the tweet frequency in each state in a table (for question 2)
prop_sanders <- prop.table(count_sanders) 

usa$valuesanders <- prop_sanders
cols_sanders <- brewer.pal(n = 9, name = "PuBuGn") #available colors are BuGn, BuPu, GnBu, OrRd...
lcols_sanders <- cut(usa$valuesanders, breaks = 9, labels = cols_sanders)
plot(usa, col = as.character(lcols_sanders), xlab = "Color Map for Sanders")

#Trump
tw_coordinates_trump<- cbind(freq_trump$place_lon, freq_trump$place_lat)
tw_coordinates_trump2 <- na.omit(tw_coordinates_trump)
tw_points_trump <- SpatialPoints(tw_coordinates_trump2) #convert to SpatialPoints
plot(tw_points_trump)

count_trump <- poly.counts(tw_points_trump, usa) # present the tweet frequency in each state in a table (for question 2)
prop_trump <- prop.table(count_trump) 

usa$valuetrump <- prop_trump
cols_trump <- brewer.pal(n = 9, name = "OrRd") #available colors are BuGn, BuPu, GnBu, OrRd...
lcols_trump <- cut(usa$valuetrump, breaks = 9, labels = cols_trump)
plot(usa, col = as.character(lcols_trump), xlab = "Color Map for Trump")

#display overall distribution of all tweets
freq <- rbind(freq_clinton, freq_trump, freq_cruz, freq_rubio, freq_sanders)

tw_coordinates<- cbind(freq$place_lon, freq$place_lat)
tw_coordinates2 <- na.omit(tw_coordinates)
tw_points <- SpatialPoints(tw_coordinates2) #convert to SpatialPoints
plot(tw_points)

counts <- poly.counts(tw_points, usa) # present the tweet frequency in each state in a table (for question 2)
proportion <- prop.table(counts) 

usa$value <- proportion
cols <- brewer.pal(n = 9, name = "Greens") #available colors are BuGn, BuPu, GnBu, OrRd...
lcols <- cut(usa$value, breaks = 9, labels = cols)
plot(usa, col = as.character(lcols), xlab = "Color Map for All Tweets")
