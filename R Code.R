library(tidyverse)
library(tidytext)
library(dplyr)
library(pdftools) 
library(tm)
library(stringr)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(ggraph)
library(igraph)
library(magrittr)
library(rvest)
library(scales)

setwd("/Users/estelle_eteki/Documents/Hult Business School/Spring Semester/Text Analytics/Individual Assignment/Pdf")
files <- list.files(pattern = "pdf$")
opinions <- lapply(files, pdf_text)

corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

lapply(opinions, length)

my_df <- tidy(corp)

junk_list <- data.frame(
  word = c("1","iufld","wkh","ri","ulvlv","dqg","5hsruw","2017", "2018", "dphurrq",
           "lq", "qjorskrqh", "uhqfk", "3djh", "2019", "dydlodeoh", "6", "0d", "ruphu",
           "dovr", "urxs", "wr", "ulhilqj", "8", "ruhljq", "dw", "iulfd", "wzr", "urvvurdgv",
           "7donv", "rz", "ig", "glylvlrq", "xjxvw", "hw"),
  lexicon = "junk"
)

junk_list2 <- data.frame(
  word = c("cameroon","cameroon's", "anglophone", "crisis", "government", "october",
           "january", "december"),
  lexicon = "junk"
)
my_df_token <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(junk_list) %>%
  count(word, sort = TRUE)

icg17 <- my_df %>%
  unnest_tokens(word, text) %>%
  filter(id == "icg_2017.pdf") %>%
  anti_join(stop_words) %>%
  anti_join(junk_list) %>%
  count(word, sort = TRUE)

icg19 <- my_df %>%
  unnest_tokens(word, text) %>%
  filter(id == "icg_2019.pdf") %>%
  anti_join(stop_words) %>%
  anti_join(junk_list) %>%
  count(word, sort = TRUE)

my_df2 <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(junk_list) %>%
  group_by(id)

my_df_freq <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(junk_list) %>%
  count(word, sort = TRUE)

ggplot(my_df_freq %>% filter(n>=40),aes(x=reorder(word, n),y=n))+
  geom_col()+
  geom_bar(stat="identity")+
  xlab(NULL)+
  coord_flip()

ggplot(my_df_freq %>% filter(n>=50),aes(x=reorder(word, n),y=n))+
  geom_col()+
  geom_bar(stat="identity")+
  xlab(NULL)+
  coord_flip()

my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(junk_list) %>%
  anti_join(junk_list2) %>%
  group_by(id) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup %>%
  ggplot(aes(x=reorder(word, n),y=n, fill=id))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="Frequency Per Article")+
  facet_wrap(~id, ncol=2, scales="free")+
  coord_flip()

# Tf_idf

my_df_token <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(junk_list) %>%
  group_by(id) %>%
  count(word, sort = TRUE)

total_words <- my_df_token %>%
  group_by(id) %>%
  summarize(total=sum(n))

my_df_join <- left_join(my_df_token, total_words)

my_df_tf_idf <- my_df_join %>%
  bind_tf_idf(word, id, n)

my_df_tf_idf <- my_df_tf_idf %>%
  arrange(desc(tf_idf))

my_df_tf_idf17 <- my_df_tf_idf %>%
  filter(id == "icg_2017.pdf") %>%
  arrange(desc(tf_idf))
  
my_df_tf_idf19 <- my_df_tf_idf %>%
    filter(id == "icg_2019.pdf") %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
my_df_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(id) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=id))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="Tf-idf Per Article")+
  facet_wrap(~id, ncol=2, scales="free")+
  coord_flip()

# Sentiment analysis

frequencies_token_afinn <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(junk_list)%>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(mean(value))

frequencies_token_bing <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(junk_list)%>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment)%>%
  count(sentiment)

frequencies_token_nrc <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(junk_list)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word,sentiment)%>%
  count(sentiment)


# visualising setiments
icg17 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=200, fixed.asp=TRUE, scale=c(0.8,0.8), title.size=1, rot.per=0.25)

icg19 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=200, fixed.asp=TRUE, scale=c(0.8,0.8), title.size=1, rot.per=0.25)
my_df_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=200, fixed.asp=TRUE, scale=c(0.8,0.8), title.size=1, rot.per=0.25)

  