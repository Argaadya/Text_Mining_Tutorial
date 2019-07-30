library(tidyverse)
library(tidytext)
library(gutenbergr)
library(janeaustenr)
library(ggraph)
library(widyr)
library(igraph)

#construct a bigram
austen_bigram <- austen_books() %>% unnest_tokens(bigram, text, token = "ngrams",n=2) 

#count the bigram
austen_bigram %>% count(bigram,sort = T)

#separate bigram to filter stop-words
bigram_separate <- austen_bigram %>% separate(bigram,c("word1","word2"),sep = " ")
bigram_filter <- bigram_separate %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)
bigram_count <- bigram_filter %>% count(word1,word2,sort = T)

#unite the bigram
austen_bigram <- bigram_filter %>% unite(bigram,word1,word2,sep = " ")

#using 3-grams
trigram <- austen_books() %>% unnest_tokens(trigram, text, token = "ngrams",n=3) %>% 
  separate(trigram,c("word1","word2","word3"),sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) 

trigram %>% count(word1,word2,word3,sort = T)

#check the most common street in the books
bigram_filter %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

#bind_tf_idf
bigram_tf <- austen_bigram %>% count(book,bigram) %>% 
  bind_tf_idf(bigram,book,n) %>% arrange(desc(tf_idf)) 
  
bigram_tf %>% group_by(book) %>% top_n(15,tf_idf) %>% ungroup() %>% 
  mutate(bigram = factor(bigram,levels = rev(unique(bigram)))) %>% 
  ggplot()+
  geom_col(aes(bigram,tf_idf,fill=book),show.legend = F)+
  facet_wrap(~book,scales = "free")+
  coord_flip()

#see phrase with a negation (no!) 
bigram_separate %>% filter(word1 == "not") %>% count(word1,word2,sort = T)

not_words <- bigram_separate %>% filter(word1=="not") %>% 
  inner_join(get_sentiments("afinn"),by = c(word2 = "word")) %>% 
  count(word2,value,sort = T)

#check the contribution of each words
not_words %>% mutate(contribution = n*value) %>% 
  arrange(desc(abs(contribution))) %>% head(20) %>% 
  mutate(word2 = reorder(word2,contribution)) %>% 
  ggplot()+
  geom_col(aes(word2,contribution,fill=contribution>0))+
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

#negation words
negation_words <- c("not", "no", "never", "without")

negated_word <- bigram_separate %>% filter(word1 %in% negation_words) %>% 
  inner_join(get_sentiments("afinn"),by = c(word2 = "word")) %>% 
  count(word1,word2,value,sort = T) %>% mutate(contribution = n*value)

negated_word %>% group_by(word1) %>% 
  top_n(15,abs(contribution)) %>% ungroup() %>% 
  mutate(word2 = reorder(word2,abs(contribution))) %>% 
  ggplot()+
  geom_bar(aes(word2,contribution,fill=contribution>0),stat = "identity")+
  facet_wrap(~word1,scales = "free")+
  coord_flip()

#Network of bigrams
bigram_graph <- bigram_count %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = T,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#counting and correlating among sections
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

word_pair <- austen_section_words %>% pairwise_count(word,section,sort=T)

#find the words that most often occur with Darcy
word_pair %>% filter(item1=="darcy")

#pind phi-correlation
word_cor <- austen_section_words %>% group_by(word) %>% filter(n() >= 20) %>% 
  pairwise_cor(word,section,sort=T)

word_cor %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

word_cor %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


#EXCERCISE ON DARWIN'S BOOKS --------------------------------------------------
darwin <- gutenberg_download(c(1228),meta_fields = "title")

darwin_bigram <- darwin %>% unnest_tokens(bigram,text,token = "ngrams",n=2) %>% 
  separate(bigram,c("word1","word2")) 

#see negative word contribution
negation_words <- c("not", "no", "never", "without")

darwin_negate <- darwin_bigram %>% filter(word1 %in% negation_words) %>% 
  inner_join(get_sentiments("afinn"),by = c(word2="word")) %>% 
  count(word1,word2,value,sort = T) %>% mutate(contribution = n*value)

png("D:/R/tutorial/text_mining/chap4_negate1.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
darwin_negate %>% group_by(word1) %>% top_n(10,abs(contribution)) %>% ungroup() %>% 
  mutate(word2 = reorder(word2,abs(contribution))) %>% 
  ggplot(aes(word2,contribution,fill=contribution>0))+
  geom_bar(stat = "identity")+
  facet_wrap(~word1,scales = "free")+
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences")+
  labs(title = "Words preceded by negation words in Darwin's On The Origin Of Species",
       subtitle = "Negation words (like never doubt) can misrepresent the sentiment score in text")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow"),
        strip.background = element_blank(),
        strip.text = element_text(colour = "lightyellow",size=12),
        legend.background = element_rect(fill = "#1D2024"),
        legend.text = element_text(colour = "white"))+
  coord_flip()
dev.off()

#bind_tf_idf
darwin <- gutenberg_download(c(1228,2300,944,1227,3620),meta_fields = "title")
darwin_bigram <- darwin %>% unnest_tokens(bigram,text,token = "ngrams",n=2) %>% 
  separate(bigram,c("word1","word2")) 
darwin_tf <- darwin_bigram %>% unite(bigram,word1,word2,sep = " ") %>% 
  count(title,bigram,sort = T) %>% 
  bind_tf_idf(bigram,title,n) %>% arrange(desc(tf_idf)) 
custom_stop <- data.frame(bigram = c("vol i", "vol ii", "g b", "m d"))
darwin_tf <- darwin_tf %>% anti_join(custom_stop)

png("D:/R/tutorial/text_mining/chap4_bigramtf1.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
darwin_tf %>% group_by(title) %>% top_n(10,tf_idf) %>% ungroup() %>% 
  mutate(bigram = factor(bigram,levels = rev(unique(bigram))),
         title = factor(title,levels = rev(unique(title)))) %>% 
  ggplot()+
  geom_col(aes(bigram,tf_idf,fill=tf_idf),show.legend = F)+
  facet_wrap(~title,scales = "free_y",ncol = 2)+
  coord_flip()+
  scale_fill_viridis_c(option = "B")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "lightyellow"),
        strip.background = element_blank(),
        strip.text = element_text(colour = "lightyellow",size=12),
        legend.background = element_rect(fill = "#1D2024"),
        legend.text = element_text(colour = "white"))+
  labs(x=NULL,y=NULL,fill="tf-idf",
       title = "Most Important Bigrams (2 consecutive words) in Each of Darwin's Book",
       subtitle = "Using the score of term frequency (tf)- inverse document frequency (idf)")
dev.off()

#bigrams network for origin of species 
darwin <- gutenberg_download(c(1228),meta_fields = "title")
darwin_bigram <- darwin %>% unnest_tokens(bigram,text,token = "ngrams",n=2) %>% 
  separate(bigram,c("word1","word2")) 
darwin_network <- darwin_bigram %>% count(word1,word2,sort = T) %>% 
  filter(n > 75) %>% 
  graph_from_data_frame()

set.seed(2017)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

png("D:/R/tutorial/text_mining/chap4_bigram2.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
ggraph(darwin_network, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = T,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()
dev.off()

#bigram correlation in the same section
darwin_section_words <- darwin %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

word_cor <- darwin_section_words %>% group_by(word) %>% filter(n() >= 20) %>% 
  pairwise_cor(word,section,sort=T)

set.seed(2016)

png("D:/R/tutorial/text_mining/chap4_bigram3.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
word_cor %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()+
  labs(title = "Correlation > 0.3 between words in the same section")
  
dev.off()
