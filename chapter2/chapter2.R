library(tidytext)
library(tidyverse)
library(janeaustenr)
library(wordcloud)
library(reshape2)
library(gutenbergr)

sentiments

#Three type of lexicon
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("loughran")

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#Count positive sentiment -------------------------------------------------------
sent_positive <- get_sentiments(lexicon = "bing") %>% filter(sentiment == "positive")
sent1 <- tidy_books %>% filter(book == "Emma") %>% inner_join(sent_positive) %>% 
  anti_join(stop_words) %>% count(word,sort = T)

#See the progress of sentiment throughout each books ----------------------------
sent2 <- tidy_books %>% inner_join(get_sentiments("bing")) %>% anti_join(stop_words) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

sent2 %>% ggplot()+ geom_col(aes(index,sentiment,fill = sentiment))+
  facet_wrap(facets = ~book,scales = "free_x")+scale_fill_viridis_c()

#Compare 3 sentiment dictionary in Pride and Prejudice ------------------------
pride <- tidy_books %>% filter(book == "Pride & Prejudice")
pride_afinn <- pride %>% inner_join(get_sentiments("afinn")) %>% anti_join(stop_words) %>% 
  group_by(index = linenumber %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")
pride_bing_nrc <- bind_rows(pride %>% inner_join(get_sentiments("bing")) %>% 
                              anti_join(stop_words) %>% mutate(method = "Bing"),
                            pride %>% inner_join(get_sentiments("loughran") %>% 
                                      filter(sentiment %in% c("positive","negative"))) %>% 
                              anti_join(stop_words) %>% mutate(method = "Loughran"))
pride_bing_nrc <- pride_bing_nrc %>% count(method, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)
pride_sentimen <- bind_rows(pride_afinn,pride_bing_nrc)

pride_sentimen %>% ggplot()+ geom_col(aes(index, sentiment, fill = sentiment))+
  facet_wrap(facets = ~method,nrow = 3)+
  scale_fill_gradient2(low = "red",mid = "gray",high = "blue")+
  scale_x_continuous(breaks = seq(0,165,10))

#see what word contribute to the sentiment -------------------------------
bing_sent <- tidy_books %>% inner_join(get_sentiments("bing")) %>% 
              count(word, sentiment,sort = T) 
bing_sent %>% group_by(sentiment) %>% top_n(15) %>% ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot()+geom_col(aes(word,n,fill = sentiment),show.legend = F)+coord_flip()+
  facet_wrap(facets = ~sentiment,scales = "free_y")+
  labs(x = "Contribution to Sentiment", y = "Word")

#make word "miss" as custom stop word due to anomaly of word usage -----------------
custom_stop <- bind_rows(tibble(word = c("miss"), lexicon = c("custom")), stop_words)

#wordcloud ------------------------------------------------------------------
dev.new(width = 1980, height = 1080, unit = "px")
tidy_books %>% anti_join(stop_words) %>% count(word,sort = T) %>% 
  with(wordcloud(word,n,max.words = 100,scale = c(4,.5)))

tidy_books %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort = T) %>% 
  acast(word~sentiment, value.var = "n",fill = 0) %>% 
  comparison.cloud(colors = c("gray20","gray80"),max.words = 100)

#tokenize in sentence ---------------------------------------------------------
PandP_sentence <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>% filter(sentiment=="negative")
word_count <- tidy_books %>% group_by(book,chapter) %>% summarize(words= n())

tidy_books %>% semi_join(bingnegative) %>% group_by(book,chapter) %>% 
  summarise(negative_word = n()) %>% left_join(word_count, by = c("book","chapter")) %>% 
  mutate(ratio = negative_word/words) %>% filter(chapter != 0) %>% 
  top_n(1) %>% ungroup()

#Excercise, Sentiment Analysis in Darwin's on the origin of the species -----------
darwin <- gutenberg_download(1228)
darwin_tidy <- darwin %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>% select(-gutenberg_id)

#Comparison of 3 lexicon per chapter
darwin_afinn <- darwin_tidy %>% inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% summarise(sentiment = sum(value)) %>% 
  mutate(method = "Afinn")

darwin_bing <- darwin_tidy %>% inner_join(get_sentiments("bing")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  count(index, sentiment) %>% 
  mutate(method = "Bing") %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive-negative)

darwin_loughran <- darwin_tidy %>% 
  inner_join(get_sentiments("loughran") %>% 
               filter(sentiment %in% c("positive","negative"))) %>% 
  group_by(index = linenumber %/% 80) %>% 
  count(index, sentiment) %>% 
  mutate(method = "Loughran") %>% 
  spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive-negative)
darwin_sent <- bind_rows(darwin_afinn,darwin_bing,darwin_loughran)

png("D:/R/tutorial/text_mining/chap2_sentiment1.png", 
    width = 4240, height = 2160, units = 'px', res = 300)  
darwin_sent %>% 
  ggplot()+geom_col(aes(index,sentiment, fill =sentiment))+
  facet_wrap(~method,nrow = 3,scales = "free_y")+
  scale_fill_gradient2(low = "darkred",mid = "gray80",high = "navy")+
  theme(strip.text = element_text(size = 12),
        text = element_text(size = 13))+
  labs(x = "Index", y="Sentiment", 
       title = "Sentiment Analysis of Darwin's On the Origin of Species",
       subtitle = "The book mostly consists of words with positive sentiment",
       caption = "Index divide the text of the book equally by 80 lines each")
dev.off()
  
#sentiment wordcloud
png("D:/R/tutorial/text_mining/chap2_wordcloud1.png", 
    width = 2160, height = 2160, units = 'px', res = 300)  
darwin_tidy %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort = T) %>% 
  acast(word~sentiment, value.var = "n",fill = 0) %>% 
  comparison.cloud(colors = c("blue","red"),max.words = 150,title.bg.colors = "white")
dev.off()
