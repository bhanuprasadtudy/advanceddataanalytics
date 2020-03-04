library(tidytext)
library(ggplot2)
library(tm)
library(RColorBrewer)
library(sqldf)
library(wordcloud)
library(syuzhet)
library(plyr)
library(magrittr)
library(janeaustenr)
library(dplyr)

listing_summary <- read.csv("listings_summary.csv", stringsAsFactors = FALSE)
review_summary <- read.csv("reviews_summary.csv", stringsAsFactors = FALSE)
head(listing_summary)
head(review_summary)
#aggregating comments on listing id
reviews_collapse <- aggregate(comments ~ listing_id, data = review_summary, FUN = paste, collapse = ",")
head(reviews_collapse)
names(reviews_collapse) <- c("id", "comments")

#creating a df with summary, space, description, neighbourhood overview and comments as columns
df_comments_listing <- sqldf("SELECT summary, space, description, neighborhood_overview, id FROM listing_summary JOIN reviews_collapse USING(id)")
df_comments_listing <- merge(df_comments_listing, reviews_collapse, by = "id")
head(df_comments_listing)
names(df_comments_listing)

set.seed(100)
df_comments_listing <- df_comments_listing[sample(nrow(df_comments_listing), 1000), ]

#Creating a vector corpus of comments column
comments_corpus <- Corpus(VectorSource(as.vector(df_comments_listing$comments)))
comments_corpus

#removing stopwords, punctuations, numbers, converting to lower, removing whitespace
comments_corpus <- tm_map(comments_corpus, removeWords, c(stopwords("en"),'ist','die','the','und', 'great', 'place', 'recommend', 'the', 'would', 'visit', 'everyth', 'within', 'definit', 'time', 'need', 'everything', 'also', 'well', 'highly', 'really', 'check', 'just', 'area', 'needed', 'staying', 'definitely', 'will', 'close', 'back', 'get', 'made', 'like', 'super', 'even'))
comments_corpus <- tm_map(comments_corpus, content_transformer(removePunctuation))
comments_corpus <- tm_map(comments_corpus, content_transformer(removeNumbers))
comments_corpus <- tm_map(comments_corpus,  content_transformer(tolower)) 
comments_corpus <- tm_map(comments_corpus, content_transformer(stripWhitespace))

#Tokenizing each word to form a term document matrix
comment_tdm <- TermDocumentMatrix(comments_corpus,control = list(tokenize = ""))
comment_tdm_mat <- as.matrix(comment_tdm)
comment_freq <- rowSums(comment_tdm_mat)

#creating a word cloud based on the frequencies of 30 words 
wordcloud(names(comment_freq), comment_freq, colors = brewer.pal(6,"Dark2"), max.words = 30)


#denver_listings_1 <- denver_listings[complete.cases(denver_listings$price_num), ]
#denver_listings_1 <- denver_listings_1[complete.cases(denver_listings_1$review_scores_rating), ]
#denver_listings_1$price_num <- as.numeric(sub(pattern = "$", replace = "", denver_listings_1$price, fixed = TRUE))
#cor_rating_review <- cor(as.numeric(denver_listings_1$price), as.numeric(denver_listings_1$review_scores_rating))

#Using a similar procedure as above to create a word cloud of neighbourhood overview
neigh_corpus <- Corpus(VectorSource(as.vector(df_comments_listing$neighborhood_overview)))
neigh_corpus

neigh_corpus <- tm_map(neigh_corpus, removeWords, c(stopwords("english"),'ist','die','the','und', 'the', 'minute', 'minutes', 'one', 'neighborhood', 'always', 'area', 'next', 'also', 'away', 'distance', 'mile', 'close', 'can', 'many', 'just', 'located', 'within', 'denver', 'will', 'around', 'easy', 'north', 'well', 'access', 'short', 'cherry', 'rino', 'center', 'best', 'lohi', 'new', 'there', 'the'))
neigh_corpus <- tm_map(neigh_corpus, content_transformer(removePunctuation))
neigh_corpus <- tm_map(neigh_corpus, content_transformer(removeNumbers))
neigh_corpus <- tm_map(neigh_corpus,  content_transformer(tolower)) 
neigh_corpus <- tm_map(neigh_corpus, content_transformer(stripWhitespace))
neigh_tdm <- TermDocumentMatrix(neigh_corpus,control = list(tokenize = ""))
neigh_tdm_mat <- as.matrix(neigh_tdm)
neigh_freq <- rowSums(neigh_tdm_mat)

wordcloud(names(neigh_freq), neigh_freq, colors = brewer.pal(6,"Dark2"), max.words = 30)

#SENTIMENT ANALYSIS

#Preprocessing the comments data to remove special characters, names, punctuations, numbers
text = as.character(df_comments_listing$comments) 
some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)
some_txt<-gsub("@\\w+","",some_txt)
some_txt<-gsub("[[:punct:]]"," ",some_txt)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)

#Passing the cleaned text to the sentiment analysis function to generate 
mysentiment<-get_nrc_sentiment((some_txt))

#Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")			
yRange <- range(0,yAxis) + 1000
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional valence", ylab = "Score", main = "Sentiment Analysis for Reviews", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")
colSums(mysentiment)

#CREATING SENTIMENT WORDCLOUDS
#cleaning the comments column by removing the punctuations and the numbers
comments_df <- df_comments_listing[, "comments", drop = FALSE]
comments_df$comments = gsub("[[:punct:]]", "", comments_df$comments)
comments_df$comments = gsub("[[:digit:]]", "", comments_df$comments)

#Tokeniize the words using tidytext package 
tidy_comments <- comments_df %>%
  unnest_tokens(word, comments)

#arranging the words using word counts
tidy_comments %>%
  count(word) %>%
  arrange(desc(n))	

#removing stopwords	
data("stop_words")
tidy_comments <- tidy_comments %>%
  anti_join(stop_words)

tidy_comments %>%
  count(word) %>%
  arrange(desc(n))

#creating a dataframe with word and the corresponding frequencies
tidy_comments <- sqldf("SELECT word, COUNT(*) as Freq
       FROM tidy_comments
       GROUP BY word")

tidy_comments_filter <- tidy_comments#[(tidy_comments$Freq > 3000) & (tidy_comments$Freq < 100000), ]       

#joining the words df with bing sentiment library
bing_word_count <- tidy_comments_filter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, Freq, sort = TRUE) 
bing_word_count <- as.data.frame(bing_word_count)
head(bing_word_count)

#creating a df with positive and negative words
bing_positive <- bing_word_count[(bing_word_count$sentiment == "positive"), ]
bing_negative <- bing_word_count[(bing_word_count$sentiment == "negative"), ]

tidy_comments_filter %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors = brewer.pal(6,"Dark2"), scale = c(3,.5)
  ))

#wordcloud with positive words
bing_positive %>%
  count(word) %>%
  with(wordcloud(bing_positive$word, bing_positive$Freq, max.words = 30, colors = brewer.pal(6,"Dark2"), scale = c(3,.2)
  ))


#wordcloud with negative words
bing_negative %>%
  count(word) %>%
  with(wordcloud(bing_negative$word, bing_negative$Freq, max.words = 30, colors = brewer.pal(6,"Dark2"), scale = c(3,.2)
  ))
