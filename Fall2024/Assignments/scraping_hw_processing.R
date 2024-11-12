#read in student data files

library(tidyverse)

#now without unzipping first woooooo
filenames <- unzip("C:/Users/alanski/Downloads/submissions (15).zip",list = TRUE)[,1]
student_names <- data.frame(person = substr(filenames, 
                                            start = 1, 
                                            stop = str_locate(filenames, pattern = "_")-1),
                            index = c(1:length(filenames)))

#unzip, then fix problems
unzip("C:/Users/alanski/Downloads/submissions (15).zip", 
      exdir = "C:/Users/alanski/Downloads/scrape24")

#check for problem in file names
 ### too many submissions
student_names %>% count(person) %>% filter(n != 2) %>% select(person)
list.files("C:/Users/alanski/Downloads/scrape24", pattern = "weigand", full.names = T)
file.remove("C:/Users/alanski/Downloads/scrape24/weigandgrace_34226_4247585_reddit_threads.csv")
file.remove("C:/Users/alanski/Downloads/scrape24/weigandgrace_34226_4247586_reddit_top_posts.csv")
 ### naming problem
originals <- list.files("C:/Users/alanski/Downloads/scrape24", pattern = "[^(top_posts|threads)].csv$", full.names = T)
originals_fixed <- str_remove(originals, pattern = "(?<=(threads|posts))(-\\d|_df)")
file.rename(originals, originals_fixed)

## remake filenames with fixes
filenames <- list.files("C:/Users/alanski/Downloads/scrape24")


bring_csv <- function(filename){
  read_csv(file = paste0("C:/Users/alanski/Downloads/scrape24/", filename)) %>%
    mutate(person = substr(filename, 
                           start = 1, 
                           stop = str_locate(filename, pattern = "_")-1),
           category = str_extract(filename, pattern = "[:alpha:]+(?=\\.csv$)"))
}

library(purrr)

files_listed <- map(filenames, bring_csv)
files_listed <- map(files_listed, ~ .x %>%
                      rename_all(~ tolower(.)))


#############  FIX ONE

swap_categories <- function(single_file){
  if (str_detect(single_file$category[1], pattern = "post") & (str_detect(paste(colnames(single_file), collapse = " "), "comment_id"))) {
    single_file <- single_file %>% mutate(swap = "Yes", 
                                          category = "threads")
    
  } else if (str_detect(single_file$category[1], pattern = "threads") & (str_detect(paste(colnames(single_file),  collapse = " "), "title"))) {
    single_file <- single_file %>% 
      mutate(swap = "Yes", 
             category = "posts")
  } else {
    single_file
  }
}


files_better <- lapply(files_listed, swap_categories) 

#################### FIX TWO

rename_new <- function(single_file){

  #print(colnames(single_file))
  
  if(sum(str_detect(colnames(single_file), "threads.author|comments.author")) > 0){
    
    print(colnames(single_file))
    colnames(single_file) <- str_remove_all(colnames(single_file), pattern = "(^threads\\.)|(^comments\\.)")
    single_file <- single_file %>%
      mutate(rename_prefix = "Yes")
    #print("prefixes fixed")
    #print(colnames(single_file))
  } else {
    single_file
  }
      
  orig_title_loc <- str_detect(colnames(single_file), pattern = "(title.+)|(.+title)") & 
    !str_detect(colnames(single_file), pattern = "^new_title$")
  orig_text_loc <- str_detect(colnames(single_file), pattern = "(text.+)|(.+text)") & 
    !str_detect(colnames(single_file), pattern = "^new_text$")
  orig_comments_loc <- str_detect(colnames(single_file), pattern = "(comments.+)|(.+comment)|comment[^s]*") & 
    !str_detect(colnames(single_file), pattern = "^(new_comments|comment|comments|comment_id)$")
  
  
  #print(orig_title_loc)
  #print(orig_text_loc)
  #print(orig_comments_loc)
  #print(str_detect(colnames(single_file), pattern = "(comments.+)|(.+comment)|(comment[^s].*)") &
  #!str_detect(colnames(single_file), pattern = "^(new_comments|comment|comments|comment_id)$"))
  print(colnames(single_file)[orig_title_loc])
  print(colnames(single_file)[orig_text_loc])
  print(colnames(single_file)[orig_comments_loc])
  
  
  if (sum(orig_title_loc) > 0) {
      colnames(single_file)[which(orig_title_loc)] <- "new_title"
      single_file <- single_file %>%
        mutate(rename_title = "Yes")
      #print("title fixed")
      #print(colnames(single_file))
  } else {
  single_file
    }
    
  if (sum(orig_text_loc) > 0) {
      print(colnames(single_file)[which(orig_text_loc)])
      print(colnames(single_file))
      colnames(single_file)[which(orig_text_loc)] <- "new_text"
      single_file <- single_file %>%
        mutate(rename_text = "Yes")
      #print("text fixed")
      #print(colnames(single_file))
    } else {
      single_file
    }
    
  if (sum(orig_comments_loc) > 0) {
      colnames(single_file)[max(which(orig_comments_loc))] <- "new_comments"
      single_file <- single_file %>%
        mutate(rename_comments = "Yes")
      #print("comments fixed")
      #print(colnames(single_file))
    } else {
      single_file
    }
    

 #unrelated but still name fix
  if("...1" %in% colnames(single_file)){
    print("rownames fix")
    single_file <- single_file %>%
      rename(boo_rows = `...1`) %>%
      mutate(boo_rows = "Yes")
  } else {
    single_file
  }
 
 return(single_file)
  
}

files_better <- lapply(files_better, rename_new)


############ FIX THREE AND FIND MORE PROBLEMS

filename_detect <- function(single_file){

  if (str_detect(single_file$category[1], pattern = "post")) {
    #columns for posts
    
    if ("date_utc" %in% colnames(single_file)) {
      single_file <- single_file %>%
        select(any_of(c("author", "date", "date_utc", "timestamp", "score", "upvotes", "downvotes",
                        "up_ratio", "total_awards_received", "golds", "cross_posts",  "url", 
                        "title", "text", "subreddit", "comments", "new_title", "new_text", "netid", 
                        "swap", "rename_title", "rename_text")),
               person, category,
               everything())
    }
    
    top_post_cols <- c("url", "author", "date", "timestamp", 
                       "title", "text", "subreddit", "score", 
                       "upvotes", "downvotes", "up_ratio", 
                       "total_awards_received", "golds", 
                       "cross_posts", "comments", "new_title", "new_text", "netid", 
                       "person")
  
    data.frame(num_found = sum(colnames(single_file) %in% top_post_cols),
             which_extra = colnames(single_file)[!colnames(single_file) %in% top_post_cols],
             num_missing = sum(!colnames(single_file) %in% top_post_cols),
             which_missing = ifelse(length(top_post_cols[!top_post_cols %in% colnames(single_file)]) == 0, 
                                    "none",
                                    top_post_cols[!top_post_cols %in% colnames(single_file)]),
             person = single_file$person[1],
             category = "top_posts")
  } else {
    #columns for threads
    
    thread_cols <- c("url", "author", "date", "timestamp",
                   "score", "upvotes", "downvotes", "golds",
                   "comment", "comment_id", "new_comments", "netid", "person")

    data.frame(num_found = sum(colnames(single_file) %in% thread_cols),
               which_extra = colnames(single_file)[!colnames(single_file) %in% thread_cols],
               num_missing = sum(!colnames(single_file) %in% thread_cols),
               which_missing = ifelse(length(thread_cols[!thread_cols %in% colnames(single_file)]) == 0, 
                                  "none",
                                  thread_cols[!thread_cols %in% colnames(single_file)]),
               person = single_file$person[1],
               category = "threads")
  }
}

#troubleshooting
#str_detect(files_better[[1]]$category[1], pattern = "threads")
#filename_detect(files_better[[11]])
#View(files_better[[11]])

columns_test <- map(files_better, filename_detect) %>%
  purrr::map(data.frame) %>% 
  purrr::list_rbind() %>%
  group_by(person) %>%
  mutate(messy_cols = n()-1) %>%
  ungroup()



columns_ok <- columns_test %>%
  filter(messy_cols == 1) %>%
  filter(((num_found == 13) & (category == "threads"))  | ((num_found == 19) & category == "top_posts"))

columns_ok_pivot <- columns_ok %>%
  transmute(category, num_missing = num_missing * 15, person) %>%
  pivot_wider(names_from = category, values_from = num_missing, names_prefix = "ok_", values_fill = 0)

problems <- columns_test %>% 
  anti_join(columns_ok, by = c("person", "category")) %>%
  filter(!((which_extra == "category") & (which_missing == "none")))

swap_names <- problems %>%
  filter(which_extra == "swap")

has_rows_names <- problems %>%
  filter(which_extra == "boo_rows")

new_comment_names <- problems %>%
  filter(which_extra == "rename_comments")

new_text_names <- problems %>%
  filter(which_extra == "rename_text")

new_title_names <- problems %>% 
  filter(which_extra == "rename_title")

new_prefix_names <- problems %>%
  filter(which_extra == "rename_prefix")

missing_netid <- problems %>%
  filter(which_missing == "netid") %>%
  mutate(which_extra = "missing_netid")

name_fixes_overall <- rbind(swap_names, has_rows_names, new_comment_names, 
                            new_text_names, new_title_names, new_prefix_names,
                            missing_netid)

problems_withoutnames <- anti_join(problems, name_fixes_overall) %>%
  filter(!which_missing == "netid")

extra_issues <- problems_withoutnames %>%
  filter(which_extra != "category",
         which_extra != "date_utc",
         which_extra != "new_comment") %>%
  group_by(person) %>%
  select(which_extra, category, person) %>%
  distinct() %>%
  mutate(extra_problem = paste0("found problem column(s) in ", category, ": ", paste(which_extra, collapse = ", " ))) %>%
  select(person, extra_problem) %>% distinct() %>% ungroup() 

missing_issues <- problems_withoutnames %>%
  filter(which_missing != "none") %>%
  select(which_missing, person, category) %>% distinct() %>%
  group_by(person, category) %>%
  mutate(missing_problem = paste0("missing column in ", category, ": ", paste(which_missing, collapse = ", " ))) %>%
  select(person, missing_problem) %>% distinct() %>% ungroup() %>%
  pivot_wider(names_from = category, values_from = missing_problem) %>%
  mutate(missing_issues = case_when(is.na(top_posts) ~ threads,
                                    is.na(threads) ~ top_posts, 
                                    TRUE ~ paste(top_posts, "and", threads))) %>%
  select(person, missing_issues) %>%
  ungroup()



name_fixes_pivoted <- name_fixes_overall %>%
  transmute(which_extra, person, category, counter = 1) %>%
  distinct() %>%
  pivot_wider(id_cols = c(person, category), 
              names_from = "which_extra", values_from = counter, values_fill = 0) 

pivoted_points <- name_fixes_pivoted %>%
  group_by(person) %>%
  summarize(boo_rows = sum(boo_rows),
            rename_comments = 0-sum(rename_comments),
            rename_text = 0-sum(rename_text),
            rename_title = 0-sum(rename_title),
            swap = 0-sum(swap/2),
            rename_prefix  = 0-sum(rename_prefix)/2,
            missing_netid = 0-sum(missing_netid)/2)  %>% 
  ungroup()

general_problem_comments <- name_fixes_pivoted %>%
  mutate(rename_student = case_when((rename_comments + rename_text + rename_title) > 1 ~ "'new' columns required name fixes",
                                    (rename_comments + rename_text + rename_title) == 1 ~ "'new' column required name fix",
                                    TRUE ~ "'new' columns have correct names"),
         prefix_mess = case_when(rename_prefix == 1 ~ "original column names had extra prefixes",
                                 TRUE ~ NA_character_),
         no_netid = case_when(missing_netid == 1 ~ "no netid column provided",
                              TRUE ~ "netid column successfully added")) %>%
  mutate(prelim_issues = case_when(!is.na(prefix_mess) ~ paste("In the", category, "file:",  
                                                               paste(prefix_mess, rename_student, no_netid, sep = "; ")),
                                   TRUE ~ paste("In the", category, "file:",  
                                                paste(rename_student, no_netid, sep = "; ")))) %>%
  select(person, category, prelim_issues) %>%
  pivot_wider(names_from = category, values_from = prelim_issues, names_prefix = "prelim_") %>%
  full_join(missing_issues) %>%
  full_join(extra_issues) %>%
  left_join(pivoted_points) %>%
  mutate(swap_situation = case_when(swap == -1 ~ "file contents and names were swapped",
                                    swap == -0.5 ~ "both file names contained the same general content",
                                    TRUE ~ "file contents were appropriate for the file names")) %>%
  full_join(columns_ok_pivot)

# want to add netids before writing out the file


#################
## COMBINE AS_IS

problems_withoutnames %>% select(which_missing, person, category) %>% distinct() %>%
  filter(which_missing != "none")

#ayres has "nedit", missing newtext and newcomments
#whild has "new_tect"
#tjoajensen has extra "cleaned comment"
#andrew wonderlich has ""awunderl" and is missing downvotes



final_file_posts <- list_rbind(files_better) %>%
  filter(category == "posts") %>%
  select(-swap, -boo_rows, 
         -rename_comments, -rename_prefix, -rename_title, -rename_text, 
         -cleaned_comment, -nedit, -new_comments, -new_tect, -awunderl) %>%
  mutate(date = coalesce(date, date_utc)) %>%
  mutate(new_title = coalesce(new_title, title),
         new_text = coalesce(new_text, text)) %>%
  select(-date_utc) %>%
  filter(!is.na(new_title)) %>%
  mutate(netid = ifelse(person == "aveybrendan", "bavey", netid)) %>%
  distinct()
# do have null New Text, but those do have titles, so it's a reddit thing

#nedit is empty and extra
# date_utc needs to get turned into date



final_file_threads <- list_rbind(files_better) %>%
  filter(category == "threads") %>%
  select(-swap, -boo_rows, 
         -rename_comments, -rename_prefix, -rename_title, -rename_text, 
         -cleaned_comment, -awunderl, -new_title, -title, -text, 
         -subreddit, -up_ratio, -total_awards_received, -cross_posts, 
         -comments, -nedit, -date_utc, -new_tect) %>%
  mutate(new_comments = coalesce(new_comments, new_text)) %>%
  mutate(new_comments= coalesce(new_comments, comment)) %>%
  select(-new_text) %>%
  filter(!is.na(new_comments)) %>%
  mutate(netid = case_when(person == "aveybrendan" ~ "bavey",
                           person == "ayreschris" ~ "cayres2",
                           person == "gordonalan" ~ "agordon7",
                           person == "wunderlichdrew" ~ "awunderl",
                           TRUE ~ netid)) %>%
  distinct()

  # mutate(sport = ifelse(person == "kavanaghpat", "hockey", sport)) %>%
  # #mutate(sport= coalesce(sport, sports)) %>%
  # mutate(sport = tolower(sport)) %>%
  # mutate(sport = ifelse(sport == "basektball", "basketball", sport)) %>%
  # mutate(text = str_replace_all(text, pattern = "\031", replacement = "'")) %>%
  # mutate(title = str_replace_all(title, pattern = "\031", replacement = "'")) %>%
  # mutate(keyword = ifelse(str_detect(keyword, pattern = "null"), NA, keyword)) %>%
  # select(date_utc, timestamp, title, text, subreddit, comments, score,
  #        up_ratio, url, sport, keyword, person) %>%
  # filter(!is.na(date_utc)) %>%
  # rbind(extra) %>%
  # filter(subreddit != "USWNT")




## accounting for points/comments related to netids and threadcounts
general_problem_comments_rev <- general_problem_comments %>%
  mutate(missing_netid = ifelse(person %in% c("ayreschris", "gordonalan", "wunderlichdrew"),
                                -1,
                                missing_netid),
         prelim_threads = ifelse(person %in% c("ayreschris", "gordonalan", "wunderlichdrew"),
                                 paste0(prelim_threads, "; ", "no netid provided"),
                                 prelim_threads)) %>%
  left_join(final_file_threads %>% count(person, netid) %>%
              filter(n < 500) %>% select(person) %>% mutate(not_enough_threads = -1,
                                                            few_threads = "should aim for at least 500 rows of thread data"),
            by = "person")

#write outs

#for me --- complete with IDs
write_csv(final_file_posts, "Fall2024/Data/UDA24_reddit_top_posts_ME.csv", na = "")
write_csv(final_file_threads, "Fall2024/Data/UDA24_reddit_threads_ME.csv", na = "")


# for them --- ids and duplicates removed
final_file_posts %>% select(-person, -netid) %>%
  group_by(url) %>%
  slice_head(n = 1) %>%
  ungroup() %>% 
  write_csv("Fall2024/Data/UDA24_reddit_top_posts.csv", na = "")

final_file_threads %>% select(-person, -netid) %>%
  group_by(comment) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  write_csv("Fall2024/Data/UDA24_reddit_top_threads.csv", na = "")


# for me again ---  grading-related
emails <- rbind(final_file_threads %>% 
                  select(person, netid) %>% 
                  distinct(),
                final_file_posts %>% 
                  select(person, netid) %>% 
                  distinct()) %>%
  distinct() %>%
  mutate(email = paste0(netid, "@nd.edu"))
  

general_problem_comments_rev %>% 
  full_join(emails) %>% 
  select(person, netid, email, everything()) %>% 
  arrange(netid) %>%
  write_csv("C:/Users/alanski/Downloads/csv_prep_comments.csv", na = "")
