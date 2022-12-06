#read in student data files

filenames <- list.files("C:/Users/alanski/Downloads/UDAScrape")
library(readr)
library(dplyr)

bring_csv <- function(filename){
  read_csv(file = paste0("C:/Users/alanski/Downloads/UDAScrape/", filename)) %>% 
    mutate(person = filename) %>%
    mutate(person_short = substr(person, start = 1, stop = str_locate(person, pattern = "_")-1)) %>%
    mutate(index = row_number())
}

library(purrr)

files_listed <- map(filenames, bring_csv)
files_listed <- map(files_listed, ~ .x %>%
                      rename_all(~ tolower(.)))


library(stringr)
filename_detect <- function(single_file){
  
  data.frame(columns = colnames(single_file)[!colnames(single_file) %in% 
                                                        c("genre", "show", "season", 
                                                          "overall", "title", "viewers", 
                                                          "description")],
             num_found = sum(colnames(single_file) %in% 
                               c("genre", "show", "season", 
                               "overall", "title", "viewers", 
                               "description")),
             person_short = single_file$person_short[1])
}


#str_locate(filenames[1], "_")[1]

# #(You will have to add this: Use the item below that fits the best)
#         i. Comedy
#         ii. Drama
#         iii. SciFi
#         iv. Fantasy
#         v. Kids
#         vi. Documentary
#         vii. Reality
#         viii. Competition
#         ix. Sports
#         x. News

columns_ok <- map(files_listed, filename_detect)
#fix nicky
files_listed[[which(str_detect(filenames, "leiver"))]] <- files_listed[[which(str_detect(filenames, "leiver"))]] %>%
  select(colnames(.)[!str_detect(colnames(.), pattern = "million")])
#unlist(columns_ok)

# which(str_detect(filenames,"kristof|laszew|obring"))
# files_listed[[30]] #null -- Entourage 3, dunno what aired is for
# files_listed[[31]] #has Episode for instead of Overall, but lacking Overall  --- requires math booooo
# files_listed[[49]]


renames <- reduce(columns_ok, rbind.data.frame) %>% 
  filter(num_found < 7,
         !columns %in% c('person', 'person_short', 'index', 'directed by', 'written by', '...1', 
                         'no. inseason','original release date',
                         'original air date', 'aired', 'prod.code')) %>% 
  mutate(newcol = case_when(str_detect(columns, pattern = "millions|viewrs|viewers") ~ 'viewers',
                            str_detect(columns, pattern = "no.overall|overall") ~ 'overall',
                            str_detect(columns, pattern = "desc|no\\.\\.inseason") ~ 'description',
                            str_detect(columns, pattern = "episode") ~ 'add8')) %>%
  group_by(person_short) %>%
  mutate(renames = n()) %>%
  ungroup()

missing <- reduce(columns_ok, rbind.data.frame) %>% filter(num_found < 7) %>%
  group_by(person_short) %>%
  mutate(total = n())

library(tidyr)

actually_rename <- function(single_file){
  single_file %>%
    pivot_longer(-c(person, person_short, index), names_to = 'orig_cols', values_to = 'values', values_transform = as.character) %>%
    left_join(renames, by = c("orig_cols" = "columns", "person_short" = "person_short")) %>%
    fill(num_found, .direction ="downup") %>%
    fill(renames, .direction = "downup") %>%
    mutate(columns = coalesce(newcol, orig_cols)) %>% select(-orig_cols, -newcol) %>%
    pivot_wider(names_from = columns, values_from = values)  %>%
    mutate(num_found = replace_na(num_found, 7),
           renames = replace_na(renames, 0))
}

files_listed2 <- map(files_listed, actually_rename)

final_cols <- c("genre", "show", "season", 
                 "overall", "title", "viewers", 
                 "description", "person", 
                 "person_short", "num_found", "renames")

#find missing
missing <- function(single_file){
  data.frame(person = single_file$person_short[1],
             missings = ifelse(sum(!final_cols %in% colnames(single_file)) == 0, 
                           "none", 
                           final_cols[!final_cols %in% colnames(single_file)]))
                           
}

#fix nate
files_listed2[[which(str_detect(filenames, "lasze"))]] <- files_listed2[[which(str_detect(filenames, "lasze"))]] %>%
  mutate(overall = as.integer(add8) + 8) %>%
  select(-add8)

map_df(files_listed2, missing) %>% filter(missings != "none")

#add missing as nulls and increment renames

#fix andrew
files_listed2[[which(str_detect(filenames, "kristof"))]] <- files_listed2[[which(str_detect(filenames, "kristof"))]] %>%
  mutate(viewers = NA)

files_listed2[[which(str_detect(filenames, "kristof"))]] <- files_listed2[[which(str_detect(filenames, "kristof"))]] %>%
  mutate(renames = renames+1)

#fix leekihyun
files_listed2[[which(str_detect(filenames, "kihyun"))]] <-  files_listed2[[which(str_detect(filenames, "kihyun"))]] %>%
  mutate(viewers = NA)

files_listed2[[which(str_detect(filenames, "kihyun"))]] <- files_listed2[[which(str_detect(filenames, "kihyun"))]] %>%
  mutate(renames = renames+1)

# #fix owers
# files_listed2[[which(str_detect(filenames, "owersmich"))]] <-  files_listed2[[which(str_detect(filenames, "owersmich"))]] %>%
#   mutate(description = NA)
# 
# files_listed2[[which(str_detect(filenames, "owersmich"))]] <- files_listed2[[which(str_detect(filenames, "owersmich"))]] %>%
#   mutate(renames = renames+1)

#####
## select and combined!

prep_final <- function(single_file){
  single_file %>%
    transmute(show, genre, season, overall, episode_in_season = index, 
              viewers, title, description, person, person_short, num_found, renames)
}

files_listed_final <- map(files_listed2, prep_final)

combined <- reduce(files_listed_final, rbind.data.frame)

#which shows got multiples?
combined %>% select(show, season, person_short) %>% distinct() %>% count(show, season, sort = TRUE) %>% View()

#with dups, remove which ones?
combined %>% select(show, season, person_short) %>% 
  filter(!person_short %in% c('browndj', 'mercerwill'
                              , 'mottagonzalezgabo'
                              , 'tyrellaidan'
                              , 'ferenmitch'
                              , 'daneknathan'
                              , 'putzcarter'
                              , 'akeyabe'
                              , 'schorrgarsen', 
                              "rupnikmeghan", "woodmackenzie", "morrisonellie",
                              "kelleymiles",
                              "fieldermaggie",
                              "janicketrevor")) %>% 
  distinct() %>% 
  count(show, season) %>% filter(n > 1) %>% 
  inner_join(combined, by = c("show", "season")) %>% 
  filter(!person_short %in% c('browndj', 'mercerwill', 'mottagonzalezgabo', 'tyrellaiden', 'ferenmitch', 
                              'daneknathan', 'putzcarter', 'akeyabe', 'schorrgarsen',  
                              "rupnikmeghan", "woodmackenzie", 
                              "morrisonellie", "kelleymiles", "fieldermaggie", "janicketrevor")) %>%
  arrange(show, season, person_short)


#who had problems with colnames?
combined %>% filter(renames > 0) %>% select(person_short, renames) %>% arrange(-renames) %>% distinct()

#add count of quotes on titles
#then remove quotes
combined <- combined %>% mutate(titlequotes = str_detect(title, '^"'),
                                title = str_replace_all(title, pattern = '^"|"$', replacement = ""))

#fix genres 
combined <- combined %>% mutate(genrewrong = str_detect(genre, "Action|:|/",),
                                genre = case_when(genre == "Comedy/Drama" ~ "Comedy",
                                                  genre == "Drama: Western" ~ "Drama",
                                                  genre == "Action" ~ "Kids",
                                                  TRUE ~ genre))

#fix viewers
combined <- combined %>% mutate(viewers = ifelse(viewers == 'N/A', NA, viewers))

#special characters --- ugh don't care anymore.  that's on them.

#get something to grade from
combined %>% group_by(person_short) %>% mutate(index = row_number()) %>% ungroup() %>% filter(index == 1) %>%
  write_csv(., file = "scrape_review.csv")

#get final file for actual use
combined %>% 
  filter(!person_short %in% c('browndj', 'mercerwill', 'mottagonzalezgabo', 'tyrellaiden', 'ferenmitch', 
                              'daneknathan', 'putzcarter', 'akeyabe', 'schorrgarsen',  
                              "rupnikmeghan", "woodmackenzie", 
                              "morrisonellie", "kelleymiles", "fieldermaggie", "janicketrevor")) %>%
  select(-c(person, person_short, num_found, renames, titlequotes, genrewrong)) %>%
  write_csv(., file = "episodes.csv")

