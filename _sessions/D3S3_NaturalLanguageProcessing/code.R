

setwd('~/Dropbox (2.0)/Work/Software/BaselRBootcamp_2018April/_sessions/D3S3_NaturalLanguageProcessing/')

require(readr)
require(stringr)
require(tidytext)
require(rvest)
require(dplyr)
require(janeaustenr)
require(ggplot2)
library(tm)
require(topicmodels)



# load readr
require(readr)

# load a package
files <- unlist(lapply(paste0('data/season_',1:7), list.files, full.names = TRUE))

# data
got <- lapply(files, read_file)


# get names

paths = paste0('//*[@id="mw-content-text"]/div/table[',2:8,']')
names = unlist(lapply(paths, function(x) {
  read_html('https://en.wikipedia.org/wiki/List_of_Game_of_Thrones_episodes') %>% 
    html_nodes(xpath = x) %>% 
    html_table() %>% 
    `[[`(1) %>%  
    `[[`(3) %>% 
    str_replace_all('"','')
  }))

nam = paste0(paste0(rep(paste0('season_',1:7),c(rep(10,6),7)),'_episode_',rep(1:10,7)[1:67]),'_',names)

names(got) = nam


# look at data
str_sub(got[[54]],1,1000)
print(got[[54]])

# extract data
txt = str_extract_all(got,'(?<=\n)[^[:digit:]|<|=|Original Air Date|(|www|"Game|Transcript][:print:]+(?=\r)')
txt[c(54,56)] <- str_extract_all(got[c(54,56)],'(?<=\n)[^[:digit:]|<|=|Original Air Date|(|www|"Game|Transcript|\n][:print:]+(?=\n)')
names(txt) = names(got)
got = txt

# create tibble
got = lapply(1:length(got), function(x){
  ids = str_split(names(got)[x],'_')[[1]][c(2, 4, 5)]
  tibble(season = as.numeric(ids[1]), episode = as.numeric(ids[2]), title_no = paste0(ids[1],'_',ids[2],'_',ids[3]), 
         title = ids[3], text = got[[x]])
})

got = do.call(rbind, got)


# save RDS
saveRDS(got,'data/game_of_thrones.RDS')


# -------- TOKENIZE

# count words
cnts <- got %>%
  unnest_tokens(word, text) %>%
  count(season, episode, title, word) %>%
  ungroup()


# -------- DTM



# read data
got <- readRDS('data/game_of_thrones.RDS')

# count words
got_counts <- got %>%
  filter(season == 1) %>%
  unnest_tokens(word, text) %>%
  count(title, word) %>%
  ungroup()

starks = c('jon','ned','robb','sansa','arya','bran','rickon','catelyn')
titles = unique(season_words$title)
cnts = season_words %>% filter(word %in% starks) %>% print(n=42)

m = data.frame(matrix(ncol=7,nrow=6))
for(i in 1:length(starks)){
  for(j in 1:length(titles)){
    cnt = cnts$n[cnts$title == titles[j] & cnts$word == starks[i]]
    m[i,j] = ifelse(length(cnt)==0,0,cnt)
  }
}
names(m) = titles
rownames(m) = starks

knitr::kable(m[c(9, 6, 5, 3, 8, 1, 10, 7, 2, 4)])


cnts <- cnts %>%
  bind_tf_idf(word, title, n)


m = data.frame(matrix(ncol=7,nrow=6))
for(i in 1:length(starks)){
  for(j in 1:length(titles)){
    cnt = cnts$tf_idf[cnts$title == titles[j] & cnts$word == starks[i]]
    m[i,j] = ifelse(length(cnt)==0,0,cnt)
  }
}
names(m) = titles
rownames(m) = starks

knitr::kable(round(m[c(9, 6, 5, 3, 8, 1, 10, 7, 2, 4)],2))

round(cor(as.data.frame(t(m))),1)



# count words
cnts <- got %>%
  unnest_tokens(word, text) %>%
  count(season, episode, title, word) %>%
  ungroup()


cnts <- cnts %>%
  bind_tf_idf(word, title, n)

cnts %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  print(n = 50)


cnts %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(season, episode, title, word) %>% 
  summarize(tf_idf = sum(tf_idf)) %>%
  group_by(season, episode, title) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = season)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~season, ncol = 3, scales = "free") +
  coord_flip()



require(wordcloud)
require(reshape2)

a = function(x) {
  b = as.matrix(x[-1])
  rownames(b) = x[[1]]
  b
}




afinn <- get_sentiments("afinn")


# count words
title_words <- got %>%
  filter(season < 7) %>%
  mutate(title_no = paste0(season,'_',episode,'_',title)) %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn) %>%
  group_by(episode, title, title_no) %>%
  summarize(sentiment = mean(score, na.rm=T)) %>%
  arrange(episode)


ggplot(aes(x = episode, y = sentiment, col = sentiment), data = title_words) + 
  geom_point() + 
  #facet_wrap(~season) + 
  scale_colour_gradientn(colours = c('red','green')) + 
  geom_smooth()

got_sentiments <- got %>%
  mutate(title_no = paste0(season,'_',episode,'_',title)) %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn)





x = afinn %>%
  count(word, score, sort = TRUE) %>%
  filter(abs(score)>=3) %>%
  mutate(positive = ifelse(score>0,score,0),
         negative = ifelse(score<=0,score,0)) %>%
  select(-score,-n)

b = as.matrix(x[-1])
rownames(b) = x[[1]]


png('figs/sentiment.png',width=800,height=800)
wordcloud::comparison.cloud(b,colors = c(yarrr::piratepal('basel')['green'], yarrr::piratepal('basel')['red']),
                            max.words = 500,
                            scale=c(2,.1))
dev.off()


png('figs/sentiment_got.png',width=800,height=600) 

# count words
title_words <- got %>%
  filter(season < 7) %>%
  mutate(title_no = paste0(season,'_',episode,'_',title)) %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn) %>%
  group_by(episode, title, title_no) %>%
  summarize(sentiment = mean(score, na.rm=T)) %>%
  arrange(episode)
  

ggplot(aes(x = episode, y = sentiment, col = sentiment), data = title_words) + 
  geom_point() + 
  #facet_wrap(~season) + 
  scale_colour_gradientn(colours = c('red','green')) + 
  geom_smooth()

dev.off()


tidy_books %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)






season_words %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  print(n = 50)


season_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(season) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = season)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~season, ncol = 2, scales = "free") +
  coord_flip()




dtm = season_words %>%
  anti_join(stop_words) %>%
  cast_dtm(season, word, n)

lda <- LDA(dtm, k = 3, control = list(seed = 1234))
lda


season_topics <- tidy(lda, matrix = "beta")

top_terms <- season_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


texts = lapply(split(got$text,got$title),function(x) str_c(x, collapse = ' '))

texts = str_to_lower(texts)

texts = lapply(texts, function(x) tm::removeWords(iconv(x,to = 'utf-8'), tm::stopwords()))

#str_replace_all(str_sub(texts[1],1,100),'your|i','')

texts = texts[-63]

str_sub(texts[1],1,100)

corpus <- Corpus(VectorSource(texts))
dtm = DocumentTermMatrix(corpus)



got_lda <- LDA(dtm, k = 5)

got_topics <- tidy(got_lda, matrix = "beta")
got_topics


got_top_terms <- got_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

got_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



DocumentTermMatrix









a = tibble(text = c('I could kill for a lovely piece of yummy cake'))

afinn <- get_sentiments("afinn")


# count words
knitr::kable(t(a %>%
  unnest_tokens(word, text) %>%
  left_join(afinn)
))


a = tibble(text = c('I could kill for a lovely piece of yummy cake'))

afinn <- get_sentiments("afinn")


# count words
knitr::kable(t(a %>%
                 unnest_tokens(word, text) %>%
                 left_join(afinn)
))



%>%
  group_by(episode, title, title_no) %>%
  summarize(sentiment = mean(score, na.rm=T)) %>%
  arrange(episode)





# load a package
files <- unlist(lapply(paste0('data/season_',1:7), list.files, full.names = TRUE))


txt_r = lapply(files, read_file)


# extract data

txt = str_extract_all(txt_r,'[:digit:]{3}[:control:]+[<i>]*[:print:]+\r*\n\r*\n')


txt = str_extract_all(txt_r,'[^[:punct:]+[:digit:]+|=|-|Original Air Date|(|<i>(|www|"Game|Transcript][:print:]+')

txt = str_extract_all(txt_r,'(?<=\n(<i>){0,1})[:alpha:]+[:control:]*[:print:]+(?=(<i>){0,1}\r\n)')

txt[[1]][1:10]
str_sub(txt_r[1],1,300)


str_extract_all("<i>Come on, boys,</i>",'^>[:print:]+(?=<)','')


length(txt[[1]])

txt[[1]][nchar(txt[[1]])==30]





