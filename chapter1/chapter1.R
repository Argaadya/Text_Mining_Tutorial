library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)
library(scales)

#Try to Unnest Tokens -------------------------------------------------------
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text_df <-tibble(line = 1:4,text)
text_df %>% unnest_tokens(word,text)

#Jane Austen books -------------------------------------------------
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>% unnest_tokens(word,text)

#Delete stop word from text---------------------------------------------
data(stop_words)
tidy_books <- tidy_books %>% anti_join(stop_words)

#count the number of words ------------------------------------------
tidy_books %>% count(word,sort = T)
tidy_books <- tidy_books %>% anti_join(stop_words) %>% count(word,sort = T)

#visualize the wordss ----------------------------------------
tidy_books %>% ggplot()+ geom_col(aes(word,n,fill=word))+ coord_flip()+
  scale_fill_viridis_d(guide=F)

#HG Wells books ------------------------------------------------------
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_HG <- hgwells %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% 
  count(word,sort = T)

#Bronte books -----------------------------------------------------------
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% 
  count(word,sort = T)

#Compare word count from 3 authors --------------------------------------
frequ <- bind_rows(mutate(tidy_bronte,author = "Bronte",),
                   mutate(tidy_HG, author = "HG Wells"),
                   mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word,"[a-z']+")) %>% 
  anti_join(stop_words) %>% 
  group_by(word,author) %>% 
  mutate(n = sum(n)) %>% ungroup() %>% 
  group_by(author)
frequ <- frequ[duplicated(frequ)==F,]
frequ <- frequ %>% 
  group_by(author) %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  spread(author,proportion) %>% 
  gather(author,proportion,"Bronte":"HG Wells") %>% na.omit()
colnames(frequ) <- c("word","Jane","author","proportion")
frequ

png("D:/R/tutorial/text_mining/chap1_correlate.png", 
    width = 4240, height = 2160, units = 'px', res = 300)  
frequ %>% ggplot(aes(x = proportion,y = Jane,
                     color = abs(Jane - proportion)))+
  geom_abline(color="white",lty=2)+
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label = word),check_overlap = T,hjust="center",vjust=1.5)+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  facet_wrap(facets = ~author)+
  labs(y= "Jane Austen", x =NULL,
       title = "Word Proportion comparison between Bronte Sisters and H.G. Wells with Jane Austen",
       caption = "source: https://www.tidytextmining.com/tidytext.html")+
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75",guide=F)+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        panel.grid = element_blank(),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "white"),
        strip.background = element_blank(),
        strip.text = element_text(colour = "white",size = 12))
dev.off()  

#Correlate --------------------------------------------------------
cor.test(data = frequ[frequ$author=="HG Wells",],
         ~proportion+Jane)

cor.test(data = frequ[frequ$author=="Bronte",],
         ~proportion+Jane)

#Excercise, Comparing Kant's and Nietzsche's books ----------------------------------------
nietzsche <- gutenberg_download(c(38145,19322,4363,51356))
tidy_nietzsche <- nietzsche %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% 
  count(word,sort = T)
kant <- gutenberg_download(c(4280,5682,5684))
tidy_kant <- kant %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% 
  count(word,sort = T)

frequ <- bind_rows(mutate(tidy_kant,author="Immanuel Kant"),
                   mutate(tidy_nietzsche, author = "Friedrich Nietzsche")) %>% 
  mutate(word = str_extract(word,"[a-z']+")) %>% 
  anti_join(stop_words) %>% 
  group_by(word,author) %>% 
  mutate(n = sum(n)) %>% ungroup() %>% 
  group_by(author)
frequ <- frequ[duplicated(frequ)==F,]
frequ <- frequ %>% 
  group_by(author) %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  spread(author,proportion) %>% na.omit()
colnames(frequ) <- c("word","Nietzsche","Kant")

png("D:/R/tutorial/text_mining/chap1_correlate2.png", 
    width = 4240, height = 2160, units = 'px', res = 300)  
frequ %>% ggplot(aes(x = Nietzsche,y = Kant,color=abs(Nietzsche-Kant)))+
  geom_abline(color="white",lty=2)+
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label = word),check_overlap = T,hjust="center")+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  labs(y= "Immanuel Kant", x ="Friedrich Nietzsche",
       caption = "Pearson's Correlation: 0.373",
       title = "Comparison of the Proportion of Words between Friedrich Nietzsche and Immanuel Kant",
       subtitle = "Both Philosophers talk a lot about world, time and (perhaps) human nature")+
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75",guide=F)+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        text = element_text(colour = "white",size=12),
        axis.text = element_text(colour = "white"),
        strip.background = element_blank(),
        strip.text = element_text(colour = "white",size = 12))
dev.off()

cor.test(data = frequ,~Nietzsche+Kant)
