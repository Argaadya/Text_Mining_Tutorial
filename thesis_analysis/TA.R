library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(widyr)
library(igraph)
library(ggraph)
library(reshape2)

setwd("D:/R/text_mining/TA/")

#WARNING: THE CORPUS IS IN INDONESIAN -------------------------------------------------

#Read PDF
TA <- Corpus(URISource("D:/tugas akhir/tugas akhir/draft/b16.pdf",mode = "text"),
             readerControl = list(reader=readPDF))

#Unnest Token from Corpus to extract word -------------------------------------------------
tidy_TA <- tidy(TA)
tidy_TA <- tidy_TA %>% unnest_tokens(word,text)

#filter word
tidy_TA <- tidy_TA %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
  filter(is.na(word)==F) %>% select(id,word)

#Count word
count_TA <- tidy_TA %>% count(word) %>% arrange(desc(n))

#wordcloud -------------------------------------------------
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud1.png", 
    width = 1980, height = 1980, units = 'px', res = 300)  
count_TA %>%
  with(wordcloud(word, n, max.words = 100,colors = pal2))
dev.off()

#classify term by the chapter (bab) -------------------------------------------------
##indicate at what row the word "bab" truly mean the start of the chapter
rowbab <- which(tidy_book$word=="bab")

tidy_book <- tidy_TA %>%
  mutate(linenumber = row_number(),
         chapter = case_when(linenumber < 2331~1,
                             linenumber > 2330 & linenumber < 7924~2,
                             linenumber > 7923 & linenumber < 12731~3,
                             linenumber > 12730 & linenumber < 16741~4,
                             linenumber > 16740 & linenumber < 26882~5,
                             linenumber > 26881~6))
  
#find unique term in each chapter -------------------------------------------------
tidy_book <- tidy_book %>% count(word,chapter,sort = T)
chapter_words <- tidy_book %>%
  bind_tf_idf(word, chapter, n) %>% arrange(desc(tf_idf)) %>% 
  mutate(word = reorder(word,tf_idf))

png("TF_IDF.png", 
    width = 2160, height = 2160, units = 'px', res = 300)  
chapter_words %>% group_by(chapter) %>% top_n(10,tf_idf) %>% ungroup() %>% 
  ggplot()+geom_col(aes(word,tf_idf,fill=chapter),show.legend = F)+
  coord_flip()+
  scale_fill_viridis_c(option = "B")+
  facet_wrap(~chapter,scales = "free_y")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "lightyellow"),
        strip.background = element_rect(fill = "pink"),
        strip.text = element_text(colour = "#1D2024",size=12),
        legend.background = element_blank(),
        legend.text = element_text(colour = "lightyellow"))+
  labs(title = "Unique Word in Each Chapter",
       subtitle = "Higher score mean the word(s) is unique to the chapter",
       y = "Term Frequency-Inverse Document Frequency", x = "Word")
dev.off()

#Most frequent word in each chapter -------------------------------------------------
png("frequent.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
tidy_book %>% group_by(chapter) %>% top_n(15,n) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot()+geom_col(aes(word,n,fill=n))+
  coord_flip()+
  facet_wrap(~chapter,scales = "free_y")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "lightyellow"),
        strip.background = element_rect(fill = "pink"),
        strip.text = element_text(colour = "#1D2024",size=12),
        legend.background = element_blank(),
        legend.text = element_text(colour = "lightyellow"))+
  labs(title = "Most Frequent Word in Each Chapter",
       subtitle = "Chapter 5 is rich in words",
       y = "Count", x = "Word", fill = "Count")
dev.off()

#Most frequent tri-grams in each chapter -------------------------------------------------
##Unnest token for tri-gram
tidy_TA2 <- tidy(TA)
tidy_TA2 <- tidy_TA2 %>% unnest_tokens(trigram,text,token = "ngrams",n=3)

#filter word
tidy_TA2 <- tidy_TA2 %>% filter(is.na(trigram)==F) %>% select(id,trigram)

##Divide by chapter
rowbab <- which(str_detect(tidy_TA2$trigram,"bab "))
tidy_book2 <- tidy_TA2 %>%
  mutate(linenumber = row_number(),
         chapter = case_when(linenumber < 2331~1,
                             linenumber > 2330 & linenumber < 8629~2,
                             linenumber > 8628 & linenumber < 14219~3,
                             linenumber > 14218 & linenumber < 20319~4,
                             linenumber > 20318 & linenumber < 31548~5,
                             linenumber > 31547~6))

##visualize
png("trigram.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
tidy_book2 %>% count(trigram,chapter) %>% group_by(chapter) %>% 
  top_n(10,trigram) %>% arrange(desc(n)) %>% filter(n>1) %>% 
  ggplot()+
  geom_col(aes(trigram,n,fill=n))+
  coord_flip()+
  facet_wrap(~chapter,scales = "free_y")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "lightyellow"),
        strip.background = element_rect(fill = "pink"),
        strip.text = element_text(colour = "#1D2024",size=12),
        legend.background = element_blank(),
        legend.text = element_text(colour = "lightyellow"))+
  labs(title = "Most Frequent Trigram in Each Chapter",
       subtitle = "Trigram with more than 1 count",
       y = "Count", x = "Word", fill = "Count")
dev.off()

#Phi-Correlation in words -------------------------------------------------
TA_section_words <- tidy_TA %>%
  mutate(section = row_number() %/% 10) 

word_cor <- TA_section_words %>% group_by(word) %>% filter(n() >= 20) %>% 
  pairwise_cor(word,section,sort=T)

set.seed(2016)

png("word_correlation.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
word_cor %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()+
  labs(title = "Correlation > 0.4 between words in the same section")

dev.off()

#Sentiment Analysis ----------------------------------------------
sentimen_pos <- read.csv("D:/R/datasets/sentiment-lexicons/positive_words_id.txt",header = F)
sentimen_pos <- data.frame(word = sentimen_pos, sentiment = "positive")
colnames(sentimen_pos) <- c("word","sentiment")
sentimen_neg <- read.csv("D:/R/datasets/sentiment-lexicons/negative_words_id.txt",header = F)
sentimen_neg <- data.frame(word = sentimen_neg, sentiment = "negative")
colnames(sentimen_neg) <- c("word","sentiment")
sentiment <- bind_rows(sentimen_pos,sentimen_neg)

sent_TA <- tidy_TA %>% count(word,sort = T) %>% 
  inner_join(sentiment) %>% filter(is.na(sentiment)==F)

##sentiment wordcloud 
png("wordcloud2.png", 
    width = 1980, height = 1980, units = 'px', res = 300)  
sent_TA %>% 
  acast(word~sentiment, value.var = "n",fill = 0) %>% 
  comparison.cloud(colors = c("red","blue"),max.words = 150,title.bg.colors = "white")
dev.off()

##sentiment in each section
TA_section_words <- tidy_TA %>%
  mutate(section = row_number() %/% 80) %>% 
  count(word,section,sort = T) %>% 
  inner_join(sentiment) %>% filter(is.na(sentiment)==F) 

##group by section to find the sentiment in each section
png("sentiment.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
TA_section_words %>% count(word,section,sentiment,sort = T) %>% 
  mutate(n = ifelse(sentiment=="negative",-1,1)) %>% 
  group_by(section) %>% summarise(sentiment = sum(n)) %>% 
  ggplot()+
  geom_hline(aes(yintercept=0),color="gray")+
  geom_point(aes(section,sentiment,color=sentiment),size=3,show.legend = F,alpha=.7)+
  geom_segment(aes(x=section,xend=section,y=0,yend=sentiment),alpha=.5,color="gray")+
  scale_x_continuous(breaks = seq(0,340,20))+
  scale_y_continuous(breaks = seq(-5,10,1))+
  scale_color_gradient2(low = "red",mid = "lightyellow",high = "blue")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "lightyellow"),
        strip.background = element_rect(fill = "pink"),
        strip.text = element_text(colour = "#1D2024",size=12),
        legend.background = element_blank(),
        legend.text = element_text(colour = "lightyellow"))+
  labs(title = "Section-by-Section Sentiment",
       subtitle = "1 section consists of 80 words
The document is dominated by positive sentiment, though there are some sections with 0 sentiment",
       y = "Sentiment", x = "Section", fill = "Sentiment")
dev.off()
