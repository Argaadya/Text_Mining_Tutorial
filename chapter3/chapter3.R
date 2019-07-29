library(tidytext)
library(tidyverse)
library(janeaustenr)
library(gutenbergr)
library(scales)
options(scipen = 10)

#count the number of word in each book
book_word <- austen_books() %>% unnest_tokens(word,text) %>% 
  count(book, word, sort = T)

total_word <- book_word %>% group_by(book) %>% summarise(total = sum(n))
book_word <- book_word %>% left_join(total_word) %>% mutate(ratio = n/total)

book_word %>% 
  ggplot()+geom_histogram(aes(ratio,fill=book),show.legend = F)+xlim(NA,0.0009)+
  facet_wrap(~book,scales = "free_y")

#ranking the word in the book ------------------------------------------
freq_by_rank <- book_word %>% group_by(book) %>% mutate(rank = row_number())

freq_by_rank %>% ggplot()+
  geom_line(aes(rank,ratio,color=book),size=1.1,alpha=0.5,show.legend = F)+
  scale_x_log10()+
  scale_y_log10()
  
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

#fit model
lm(log10(ratio) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, ratio, color = book)) + 
  geom_abline(intercept = -0.6226, slope = -1.125, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#bind_tf_idf
book_words <- book_word %>%
  bind_tf_idf(word, book, n)
book_words <- book_words %>% select(-ratio,-total) %>% arrange(desc(tf_idf))

book_words %>% arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word,levels = rev(unique(word)))) %>% 
  group_by(book) %>% top_n(15) %>% ungroup() %>% 
  ggplot()+geom_col(aes(word,tf_idf,fill=book))+coord_flip()+
  facet_wrap(~book,scales = "free")
  

#try bind_tf_idf on non-fiction literature
physics <- gutenberg_download(c(37729, 14725, 13476), 
                              meta_fields = "author") 
physics_word <- physics %>% 
  unnest_tokens(word, text) %>% count(author,word,sort = T) %>% 
  bind_tf_idf(word,author,n) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola")))
mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm"))
physics_word <- anti_join(physics_word, mystopwords, by = "word")

physics_word %>% group_by(author) %>% top_n(15) %>% ggplot()+
  geom_col(aes(word,tf_idf,fill=author),show.legend = F)+
  facet_wrap(~author,scales = "free")+
  coord_flip()

#EXCERCISE: Find the most important word from each of darwin's literature
darwin <- gutenberg_download(c(1228,2300,944,1227),meta_fields = "title")

darwin_words <- darwin %>% unnest_tokens(word,text) %>% count(word,title,sort = T) %>% 
  bind_tf_idf(word,title,n) %>% arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))

#check the result, see if there is any abnormal words
darwin_words %>% group_by(title) %>% top_n(15) %>% ggplot()+
  geom_col(aes(word,tf_idf,fill=title),show.legend = F)+
  facet_wrap(~title,scales = "free")+coord_flip()

#find some string in text
darwin %>% 
  filter(str_detect(text, "degs\\.")) %>% 
  select(text)

#replace degs. with degrees
darwin_words$word <- str_replace(darwin_words$word,"degs","degrees")

#remove the abnormal words
custom_stop <- data.frame(word= c("ii","ibid","vol","pp","fig","s","prof","footnote","among",
                                  1860:1875))
darwin_words <- darwin_words %>% anti_join(custom_stop) %>% arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))
  
png("D:/R/tutorial/text_mining/chap3_tf_idf1.png", 
      width = 4240, height = 2160, units = 'px', res = 300)  
darwin_words %>% group_by(title) %>% top_n(15) %>% 
  ggplot()+
  geom_col(aes(word,tf_idf,fill=tf_idf))+
  facet_wrap(~title,scales = "free")+
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
        legend.text = element_text(colour = "white") )+
  labs(x=NULL,y=NULL,fill="tf-idf",
       title = "Most Important Words in Each of Darwin's Book",
       subtitle = "Using the score of term frequency (tf)- inverse document frequency (idf)")
dev.off()

freq_by_rank <- darwin_words %>% arrange(desc(tf)) %>% mutate(rank = row_number())
rank_subset <- freq_by_rank %>% 
  filter(rank < 30000,
         rank > 100)

lm(log10(tf)~log10(rank),data = rank_subset)

png("D:/R/tutorial/text_mining/chap3_tf_idf2.png", 
    width = 3960, height = 2160, units = 'px', res = 300)  
freq_by_rank %>% 
  ggplot()+
  geom_abline(aes(intercept=-0.332,slope=-.9963),lty=2,color="gray",size=0.5)+
  geom_line(aes(rank,tf,color=title),show.legend = F,size=1,alpha=1/2)+
  scale_x_log10()+scale_y_log10()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1D2024",color=NULL),
        plot.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "lightyellow"))+
  labs(x = "Rank", y= "Term Frequency", 
       title = "Zipf's Law in Darwin's Literatures",
       subtitle = "The corpus are not exactly following the Zipf's distribution",
       caption = "Zipf's Law: Given a large corpus of natural language occurrences, the frequency of any word is inversely proportional to its rank in frequency table.")
dev.off()

