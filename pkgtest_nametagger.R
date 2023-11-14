library(nametagger)

#example provided by github page
model <- nametagger_download_model("english-conll-140408", model_dir = tempdir())
x     <- data.frame(doc_id      = c(1, 1, 2),
                    sentence_id = c(1, 2, 1),
                    text        = c("I\nlive\nin\nNew\nYork\nand\nI\nwork\nfor\nApple\nInc.", 
                                    "Why\ndon't\nyou\ncome\nvisit\nme", 
             
                                                           "Good\nnews\nfrom\nAmazon\nas\nJohn\nworks\nthere\n."))
predict(model, x)

help(package = "nametagger")

# me trying things out with a data file read in as DAT
library(tidyverse)
library(tidytext)

dat <- read_csv("C:/Users/alanski/Documents/GitHub/UDA/Fall2023/UDA Fall 2023 Stories v3.csv")

#tokenize with sentimentr
xr <- dat %>% 
  sentimentr::get_sentences() %>% data.frame() %>% 
  transmute(doc_id = element_id, sentence_id, text = story) %>%
  mutate(text = str_replace_all(text, pattern = "\\s", replacement = "\n"))
  
#works better with line breaks (easier to see results.. and finds more of them?)
xr_result <- predict(model, xr) %>% filter(entity != "O")

#tokenize with tidytext
xt <- dat %>% 
  unnest_tokens(output = text, input = story, token = "sentences", to_lower = FALSE) %>% 
  group_by(id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup() %>%
  transmute(doc_id = id, sentence_id, text) %>%
  mutate(text = str_replace_all(text, pattern = "\\s", replacement = "\n"))

xt_result <- predict(model, xt) %>% filter(entity != "O")

#results are basically the same. 
#looks like sentence numbering is slightly different 
#  probably due to different methods of tokenizing
# NOTE that keeping uppercase is SUPER IMPORTANT
anti_join(xr_result, xt_result, by = c("doc_id", "sentence_id", "term"))
anti_join(xt_result, xr_result, by = c("doc_id", "sentence_id", "term"))


