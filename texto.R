rm(list = ls())
library(RSQLite)
pasta="C:\\Users\\USER\\Desktop\\Brisk - TJ"
setwd(pasta)
m=dbDriver('SQLite')
con=dbConnect(m,dbname="myteste")
data1=dbGetQuery(con,"SELECT * FROM noticia")
data2=data1[-c(3,10,12),1:28]

install.packages("tidyverse")
install.packages("lubridate")
install.packages("quanteda")
install.packages("wordcloud2")
install.packages("stringr")
install.packages("tidytext")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("tm")
install.packages("dplyr")
install.packages("widyr")
install.packages("ggraph")
install.packages("igraph")
install.packages("topicmodels")
install.packages("textdata")

library(tm)
library(tidyverse)
library(lubridate)
library(quanteda)
library(wordcloud2)
library(stringr)
library(tidytext)
library(SnowballC)
library(wordcloud)
library(widyr)
library(ggraph)
library(igraph)
library(topicmodels)
library(textdata)

glimpse(data2)
attach(data2)
str_length(body)


body[1]
body[1]=str_replace(body[1],"\n\n<nitf:p class=\"page_topic\">"," ")
body[1]=str_replace(body[1],"<nitf:p class=\"head_kicker\"/><nitf:p class=\"head_topic\"/>\n\n","")
body[1]=str_replace_all(body[1],"\n"," ")
body[1]=str_replace(body[1],"-"," ")
body[1]=tolower(body[1])
body[1]=removePunctuation(body[1])
body[1]=removeNumbers(body[1])
body[1]=stripWhitespace(body[1])
stopwords("en")
body[1]=removeWords(body[1],stopwords("en"))
body[1]
wordcloud(body[1])

body[2]
body[2]=tolower(body[2])
body[2]=removePunctuation(body[2])
body[2]=removeNumbers(body[2])
body[2]=stripWhitespace(body[2])
body[2]
wordcloud(body[2])

body[3]
body[3]=str_replace_all(body[3],"\n"," ")
body[3]=str_replace_all(body[3],"-"," ")
body[3]=tolower(body[3])
body[3]=removePunctuation(body[3])
body[3]=removeNumbers(body[3])
body[3]=stripWhitespace(body[3])
body[3]
wordcloud(body[3])

body[4]
body[4]=str_replace_all(body[4],"\n"," ")
body[4]=str_replace_all(body[4],"-"," ")
body[4]=tolower(body[4])
body[4]=removePunctuation(body[4])
body[4]=removeNumbers(body[4])
body[4]=stripWhitespace(body[4])
body[4]=str_replace(body[4],"ouvidadorrit","ouvida dorrit")
body[4]
wordcloud(body[4])

body[5]
body[5]=str_replace_all(body[5],"\n"," ")
body[5]=str_replace_all(body[5],"-"," ")
body[5]=tolower(body[5])
body[5]=removePunctuation(body[5])
body[5]=removeNumbers(body[5])
body[5]=stripWhitespace(body[5])
body[5]

body[6]
body[6]=str_replace_all(body[6],"\n"," ")
body[6]=str_replace_all(body[6],"-"," ")
body[6]=tolower(body[6])
body[6]=removePunctuation(body[6])
body[6]=removeNumbers(body[6])
body[6]=stripWhitespace(body[6])
body[6]

body[7]
body[7]=str_replace_all(body[7],"\n"," ")
body[7]=str_replace_all(body[7],"-"," ")
body[7]=tolower(body[7])
body[7]=removePunctuation(body[7])
body[7]=removeNumbers(body[7])
body[7]=stripWhitespace(body[7])
body[7]

body[8]
body[8]=str_replace_all(body[8],"\n"," ")
body[8]=str_replace_all(body[8],"-"," ")
body[8]=tolower(body[8])
body[8]=removePunctuation(body[8])
body[8]=removeNumbers(body[8])
body[8]=stripWhitespace(body[8])
body[8]

body[9]
body[9]=str_replace_all(body[9],"\n"," ")
body[9]=str_replace_all(body[9],"-"," ")
body[9]=tolower(body[9])
body[9]=removePunctuation(body[9])
body[9]=removeNumbers(body[9])
body[9]=stripWhitespace(body[9])
body[9]

body[10]
body[10]=str_replace_all(body[10],"\n"," ")
body[10]=str_replace_all(body[10],"-"," ")
body[10]=tolower(body[10])
body[10]=removePunctuation(body[10])
body[10]=removeNumbers(body[10])
body[10]=stripWhitespace(body[10])
body[10]

body[11]
body[11]=str_replace_all(body[11],"\n"," ")
body[11]=str_replace_all(body[11],"-"," ")
body[11]=tolower(body[11])
body[11]=removePunctuation(body[11])
body[11]=removeNumbers(body[11])
body[11]=str_replace_all(body[11],"km"," ")
body[11]=str_replace_all(body[11],"°c"," ")
body[11]=stripWhitespace(body[11])
body[11]

body[12]
body[12]=str_replace_all(body[12],"\n"," ")
body[12]=str_replace_all(body[12],"-"," ")
body[12]=tolower(body[12])
body[12]=removePunctuation(body[12])
body[12]=removeNumbers(body[12])
body[12]=str_replace_all(body[12]," h "," ")
body[12]=stripWhitespace(body[12])
body[12]

body[13]
body[13]=str_replace_all(body[13],"\n"," ")
body[13]=str_replace_all(body[13],"-"," ")
body[13]=tolower(body[13])
body[13]=removePunctuation(body[13])
body[13]=removeNumbers(body[13])
body[13]=str_replace_all(body[13]," h "," ")
body[13]=str_replace_all(body[13]," hm "," ")
body[13]=str_replace_all(body[13]," r "," ")
body[13]=stripWhitespace(body[13])
body[13]

body[14]
body[14]=str_replace_all(body[14],"\n"," ")
body[14]=str_replace_all(body[14],"-"," ")
body[14]=tolower(body[14])
body[14]=removePunctuation(body[14])
body[14]=removeNumbers(body[14])
body[14]


data_fim <- data2 %>%
  unnest_tokens(word, body) %>%
  filter(str_detect(word, "[a-z']$"),
  !word %in% stop_words$word)

data_fim %>%
  count(word, sort = TRUE)

data_fim_newsgroup <- data_fim %>%
  count(journal_id, word, sort = TRUE) %>%
  ungroup()

tf_idf <- data_fim_newsgroup  %>%
  bind_tf_idf(word, journal_id, n) %>%
  arrange(desc(tf_idf))

tf_idf

tf_idf %>%
  group_by(journal_id) %>%
  slice_max(tf_idf, n = 12) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = journal_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ journal_id, scales = "free") +
  labs(x = "tf-idf", y = NULL)

newsgroup_cors <- data_fim_newsgroup %>%
  pairwise_cor(journal_id, word, n, sort = TRUE)

newsgroup_cors


set.seed(2017)

newsgroup_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#Modelagem de Tópico

word_sci_newsgroups <- data_fim %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 50)

sci_dtm <- word_sci_newsgroups %>%
  unite(document, journal_id, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)


sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2016))
sci_lda %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

sci_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("journal_id", "id"), sep = "_") %>%
  mutate(journal_id = reorder(journal_id, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ journal_id) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")

#Análise de Sentimento


newsgroup_sentiments <- data_fim_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(journal_id) %>%
  summarize(value = sum(value * n) / sum(n))

newsgroup_sentiments %>%
  mutate(journal_id = reorder(journal_id, value)) %>%
  ggplot(aes(value, journal_id, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Average sentiment value", y = NULL)



https://p4husp.github.io/material/tutorial11/

 
