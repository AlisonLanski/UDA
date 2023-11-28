# Presentation responses

library(tidyverse)
library(googlesheets4)

#####################
#get the file
theUrl <- "https://docs.google.com/spreadsheets/d/1ypXlZLOHJ9rQrwH1ZMScFcKdYFzmjedsrH6XSdN5lB8"
df <- read_sheet(ss = theUrl) %>%
  rename(group = `Which group are you commenting on?`,
         quantity = `The presentation had the right amount of content for the time [3 = \"Just Right\"]` ,
         clear = `The presentation was clear and easy to follow`,
         learning = `The presentation gave me ideas or tools to try something new`,
         visual = `The visual aid was a great support for the presentation`,
         overall = `I would want to hear from this group again on a new topic`,
         comment = `For their final writeup: Can you provide a suggestion to improve? For example, an element to explain better, a question you'd like answered, etc. [shared]`,
         memorable = `Write one thing you want to remember from this presentation (useful, interesting, important...)`,
         additional = `Space for additional comments [private]: does anything stand out, positive or negative?`)
  

###############
#prep comments to send out

combined <- df %>% group_by(group) %>%
   mutate(longtext = str_flatten(string = comment, collapse = "\n", last = "")) %>%
  select(group, 
         longtext) %>%
  distinct()

write_sheet(data = combined, 
            ss = theUrl, 
            sheet = "Combined Responses")


###################
#calculate stats about the groups
df %>%
  pivot_longer(cols = c(quantity, clear, learning, visual, overall), 
               names_to = "metric",
               values_to = "score") %>%
  group_by(metric) %>%
  mutate(avg = mean(score),
         med = median(score),
         std_dev = sd(score)) %>%
  ungroup() %>%
  group_by(group, metric) %>%
  mutate(avg_score = mean(score),
         diff = round((avg_score - avg)/std_dev, 3)) %>%
  ungroup() %>%
  select(group, metric, avg_score, diff) %>% 
  distinct() %>%
  pivot_wider(names_from = metric,
              values_from = c(avg_score, diff)) %>% 
    #do something to weight & convert to score
  write_sheet(ss = theUrl, sheet = "Presentation Score")

###################
#calculate participation scores for students
df %>% 
  mutate(score_total = quantity + clear + learning + visual + overall) %>%
  group_by(`Email Address`) %>% 
  mutate(total_replies = n(),
         all_comments = str_flatten(string = comment, collapse = "\n"),
         all_scores = str_flatten(string = as.character(score_total), collapse = "\n")) %>%
  ungroup() %>%
  select(`Email Address`, all_scores, all_comments) %>%
  distinct() %>%
  write_sheet(ss = theUrl, sheet = "Student Participation")

