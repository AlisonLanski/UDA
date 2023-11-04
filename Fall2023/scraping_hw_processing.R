#read in student data files

#now without unzipping first woooooo
filenames <- unzip("C:/Users/alanski/Downloads/submissions (6).zip",list = TRUE)[,1]
student_names <- data.frame(person = substr(filenames, 
                                            start = 1, 
                                            stop = str_locate(filenames, pattern = "_")-1),
                            index = c(1:length(filenames)))

unzip("C:/Users/alanski/Downloads/submissions (6).zip", 
      exdir = "C:/Users/alanski/Downloads/scrape")

library(readr)
library(dplyr)

bring_csv <- function(filename){
  read_csv(file = paste0("C:/Users/alanski/Downloads/scrape/", filename)) %>%
    mutate(person = substr(filename, 
                           start = 1, 
                           stop = str_locate(filename, pattern = "_")-1))
}

library(purrr)

files_listed <- map(filenames, bring_csv)
files_listed <- map(files_listed, ~ .x %>%
                      rename_all(~ tolower(.)))


library(stringr)
filename_detect <- function(single_file){
  
  data.frame(columns = colnames(single_file)[!colnames(single_file) %in% 
                                                        c("date_utc", "timestamp", "title", 
                                                          "text", "subreddit", "comments", 
                                                          "score", "up_ratio", "url", "sport", "keyword")],
             num_found = sum(colnames(single_file) %in% 
                               c("date_utc", "timestamp", "title", 
                                 "text", "subreddit", "comments", 
                                 "score", "up_ratio", "url", "sport", "keyword")),
             person = single_file$person[1])
}


columns_test <- map(files_listed, filename_detect) %>%
  purrr::map(data.frame) %>% 
  purrr::list_rbind() %>%
  group_by(person) %>%
  mutate(messy_cols = n()-1) %>%
  ungroup()

columns_ok <- columns_test %>%
  filter(messy_cols == 0, num_found == 11)

columns_not_ok_missing <- columns_test %>%
  filter(messy_cols == 0, num_found < 11)

columns_not_ok_extras <- columns_test %>%
  filter(messy_cols > 0, num_found == 11)

columns_not_ok_missing_extras <- columns_test %>%
  filter(messy_cols > 0, num_found < 11)

#check categories
length(unique(columns_ok$person)) + 
  length(unique(columns_not_ok_missing$person)) +
  length(unique(columns_not_ok_extras$person)) +
  length(unique(columns_not_ok_missing_extras$person)) == length(files_listed)

#check OK and give credit
sort(columns_ok$person)

#check actual data for the OKs
for (i in 1:length(files_listed)) {
  if(files_listed[[i]][1,"person"] %in% unique(columns_ok$person)){
    print(files_listed[[i]][1,])  
  }
} 

#check rows for everyone, find and check short
student_names[purrr::map(files_listed, nrow) < 500 ,]

View(files_listed[[6]])
View(files_listed[[32]])


## look at NOT OK and give credit, ID fixes
columns_not_ok_extras %>% 
  inner_join(student_names)

for (i in 1:length(files_listed)) {
  if(files_listed[[i]][1,"person"] %in% unique(columns_not_ok_extras$person)){
    print(files_listed[[i]][1,])  
  }
} 


### Find date problems
for (i in 1:length(files_listed)) {
    print(files_listed[[i]][1:2,c("date_utc", "person")])    
}


#Fix index column, fix date column type
file_fixer <- function(thisfile){
  if(sum(str_detect(colnames(thisfile), pattern = "...1")) > 0){
    thisfile <- thisfile %>% 
                  select(-1) %>%
                  mutate(indexcol = -1)
  }
  if(sum(str_detect(colnames(thisfile), pattern = "keywords")) > 0){
    thisfile <- thisfile %>% mutate(keyword = keywords,
                                    keywords = -1)
  }
  if(sum(str_detect(colnames(thisfile), pattern = "sports")) > 0){
    thisfile <- thisfile %>% mutate(sport = sports,
                                    sports = -1)
  }
  
  if(typeof(thisfile$date_utc) == "character"){
    thisfile <- thisfile %>% mutate(date_utc = lubridate::mdy(thisfile$date_utc),
                                     date_format = -1)
    
  }
  
  return(thisfile)
}


# ugh, one incorrect submission
extra <- read_csv("C:/Users/alanski/Downloads/reddit_warriors.csv") %>%
  mutate(keyword = keywords) %>%
  select(-keywords) %>%
  mutate(person = "lukinstianji")


## Create combined DF (may need more updates)
better_files <- map(files_listed, file_fixer)

final_file <- list_rbind(better_files) %>%
  mutate(sport = ifelse(person == "kavanaghpat", "hockey", sport)) %>%
  #mutate(sport= coalesce(sport, sports)) %>%
  mutate(sport = tolower(sport)) %>%
  mutate(sport = ifelse(sport == "basektball", "basketball", sport)) %>%
  mutate(text = str_replace_all(text, pattern = "\031", replacement = "'")) %>%
  mutate(title = str_replace_all(title, pattern = "\031", replacement = "'")) %>%
  mutate(keyword = ifelse(str_detect(keyword, pattern = "null"), NA, keyword)) %>%
  select(date_utc, timestamp, title, text, subreddit, comments, score,
         up_ratio, url, sport, keyword, person) %>%
  filter(!is.na(date_utc)) %>%
  rbind(extra) %>%
  filter(subreddit != "USWNT")
  




head(final_file)

final_file %>% filter(is.na(title)) %>%
  View()

summary(final_file)

final_file %>% summarize(across(everything(), ~sum(is.na(.))))
nrow(final_file)
19764/29544

final_file %>% count(sport, subreddit) %>% View()
