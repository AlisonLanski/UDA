library(tidyverse)
dat <- read_csv("C:/Users/alanski/Downloads/NDBasketball_articles.csv")

View(head(dat))

table(grepl(dat$Content, pattern = "bball"))

min(dat$Date)
max(dat$Date)


sort(unique(dat$Date)) %>% View()
glimpse(dat)

dat %>% count(year, search_term)


stringr::str_extract(string = dat$Content[1:20], pattern = "([^\\s]+\\s){1,5}basketball(\\s[^\\s]+){1,5}")

#find and remove box scores only
table(grepl(x = dat$Content, pattern = "Men Standings, schedules Big 12 Conference Conf"))

#problem: are articles "About" notre dame or "About" someone else? 
dat[6,] %>% View()
#the Overview has the paper it's from

table(grepl(x = dat$Overview, pattern = "Elkhart Truth"))
table(grepl(x = dat$Overview, pattern = "Chicago"))
table(grepl(x = dat$Overview, pattern = ""))
