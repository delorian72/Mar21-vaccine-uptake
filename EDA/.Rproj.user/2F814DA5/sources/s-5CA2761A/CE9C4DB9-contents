# DATA CLEANING
# Get rid of NAs

tt_data <- na.omit(tt_data)

# Remove all URLs

tt_data$no_url <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", tt_data$full_text)

# Remove all \n and \r

tt_data$no_url <- gsub("[\r\n]", " ", tt_data$no_url)

# Remove some of the hashtags at the end of the tweet

tt_data$no_hash <- sub("\\s*\\B#\\w+(?:\\s*#\\w+)*\\s*$", "",  tt_data$no_url)
tt_data$no_hash <- sub("\\s*\\B#\\w+(?:\\s*#\\w+)*\\s*$", "", tt_data$no_hash)

tt_data$clean <- gsub("COVID-19","COVID19 ", tt_data$no_hash)
tt_data$clean <- gsub("COVID ","COVID19 ",tt_data$clean)
tt_data$clean <- gsub("COVIDー19","COVID19 ",tt_data$clean)
tt_data$clean <- gsub("#COVID19","COVID19 ",tt_data$clean)
tt_data$clean <- gsub("#COVID19","COVID19 ",tt_data$clean)

write.csv(tt_data,"twitter_text_clean.csv")

