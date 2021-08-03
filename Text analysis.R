library(tidyverse)
install.packages("tidytext")
library(tidytext)
library(readr)
library(ggplot2)
tweets_1_=read_csv("http://vicpena.github.io/R/tweets.csv")
View(tweets_1_)

words=tweets_1_ %>% unnest_tokens(word,tweet_text)
words_clean= words %>% anti_join(stop_words)

# 1. Create a bar plot (geom_col) that shows the top 10 most frequent words 
#for "verified" and "unverified" users, separately. 
#Sort the bars according to their frequency (i.e. put the most
#frequent word on top, and the 10th most frequent word at the bottom). 

user_verified = words_clean %>% filter(usr_verified=="TRUE")

word_verified = user_verified %>% count(word)

top10_verified = word_verified %>% slice_max(n, n=10)
top10_verified

ggplot(top10_verified)+
  aes(y=fct_reorder(word,n),x=n)+
  geom_col()+
  ylab("Top 10 words")+
  ggtitle("Top 10 words for verified users")

# ["unverified" users]
user_unverified= words_clean %>% filter(usr_verified=="FALSE")
user_unverified
word_unverified = user_unverified %>% count(word)

top10_unverified = word_unverified %>% slice_max(n, n=10)
top10_unverified

ggplot(top10_unverified)+
  aes(y=fct_reorder(word,n),x=n)+
  geom_col()+
  ylab("Top 10 words")+
  ggtitle("Top 10 words for unverified users")

# 2. Create a bar plot that shows the top 10 most frequent words for tweets 
# that are classified as complaints and those that are not, separately. 

bar_complaint = words_clean %>% filter(complaint_label=="Complaint")

tweet_complaint = bar_complaint %>% count(word)

top10_complaint = tweet_complaint %>% slice_max(n, n=10)
top10_complaint

ggplot(top10_complaint)+
  aes(y=fct_reorder(word,n),x=n)+
  geom_col()+
  ylab("Top 10 complaints")+
  ggtitle("Top 10 words for complaints ")


# [Non-complaints] #

bar_noncomplaint = words_clean %>% filter(complaint_label=="Non-Complaint")

tweet_noncomplaint = bar_noncomplaint %>% count(word)

top10_noncomplaint = tweet_noncomplaint %>% slice_max(n, n=10)
top10_noncomplaint

ggplot(top10_noncomplaint)+
  aes(y=fct_reorder(word,n),x=n)+
  geom_col()+
  ylab("Top 10 Non-complaints words")+
  ggtitle("Top 10 words for Non-complaints ")









# 3. Find a word cloud with the 200 most frequent words in the data. 
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
word_data=words_clean %>% count(word)
words_clean %>% count(word)
wordcloud(words=word_data$word,
          freq=word_data$n,
          max.words=200,
          colors="dodgerblue")


# 4. Find a word cloud with the 25 most frequent words for tweets that 
#are complaints and another one for those that aren’t.
words_clean %>% count(complaint_label)
# "filter" with [complaint]
complaint = words_clean %>% filter(complaint_label=="Complaint")
complaint %>% count(complaint_label)
word_complaint=complaint %>% count(word)
wordcloud(words=word_data$word,
          freq=word_data$n,
          max.words=25,
          colors="cadetblue")

# "filter" with [Non-Complaint]
non_complaint = words_clean %>% filter(complaint_label=="Non-Complaint")
non_complaint %>% count(complaint_label)
word_complaint=non_complaint %>% count(word)
wordcloud(words=word_data$word,
          freq=word_data$n,
          max.words=25,
          colors="goldenrod")









