library(SnowballC)

help(package = "SnowballC")


dat %>% unnest_tokens(word, story, "words") %>% 
  anti_join(get_stopwords(source = "smart")) %>%
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = T) %>% slice_max(n, n = 10)
  
library(textstem)
help(package = "textstem")

dat %>% unnest_tokens(word, story, "words") %>% 
  anti_join(get_stopwords(source = "smart")) %>%
  mutate(lemma = lemmatize_words(word)) %>%
  count(lemma, sort = T) %>% slice_max(n, n = 10)


dat %>% unnest_tokens(word, story, "words") %>% 
  anti_join(get_stopwords(source = "smart")) %>%
  mutate(stem = stem_words(word)) %>%
  count(stem, sort = T) %>% slice_max(n, n = 10)
