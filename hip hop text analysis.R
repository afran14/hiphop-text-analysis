library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
remove.packages("plyr")

#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams

#Define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")
hiphop_data <- read.csv('hiphophistory-lyrics.csv', stringsAsFactors = FALSE, row.names = 1)
glimpse(hiphop_data)

undesirable_words <- c("chorus", "repeat", "lyrics",
                       "yeah", "alright", "wanna", "gonna", "baby",
                       "alright", "verse", "hey", "yo", "verse", "gotta", "make", "hook", "niggas", "nigga")
#Cleans up lyrics
tidy_lyrics <- hiphop_data %>%
  unnest_tokens(word, lyrics) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stop_words) #Data provided by the tidytext package

#Most used words in entire dataset
tidy_lyrics %>%
  count(word, sort = TRUE)

#Distinct words per year
word_summary <- tidy_lyrics %>%
  group_by(Year, Song) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(Song, Released = Year, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup()

install.packages("yarrr")
library(yarrr)
pirateplot(formula =  word_count ~ Released, #Formula
           data = word_summary, #Data frame
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Year", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size

#Distinct words by artist (This doens't really work yet)
word_summary <- tidy_lyrics %>%
  group_by(Location, Song) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(Song, Artist = Artist, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup()

pirateplot(formula =  word_count ~ Artist, #Formula
           data = word_summary, #Data frame
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Artist Lexical Diversity", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size

#Rough sentiment analysis
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_lyrics %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

nrc_sadness <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")

tidy_lyrics %>%
  inner_join(nrc_sadness) %>%
  count(word, sort = TRUE)


tidy_lyrics %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds
