library(stringr)
library(dplyr)
library(tidyr)
library(pdftools)

encanto <- pdf_text("https://8flix.com/assets/screenplays/e/tt2953050/Encanto-2021-screenplay-by-Charise-Castro-Smith-Jared-Bush.pdf")

# paste as a single string
script_string <- paste(encanto, sep = " ", collapse = "\\\n")

# split by line with \n
script_byline <- str_extract_all(script_string, "\\\n.*")

# convert to dataframe
script_byline_df <- data.frame(script_byline[[1]])

#remove start/end of line white space
script_squish <-  script_byline_df %>%
  mutate(script_byline..1.. = str_squish(script_byline..1..))

# get row containing intro text
start_line <- which(script_squish$script_byline..1.. == "\\")[1]

# remove text above intro
script_cleaner <- data.frame(script_squish[start_line:nrow(script_squish),])

colnames(script_cleaner) <- "raw_text"

# filter out irrelevant lines
script_filt <- script_cleaner %>%
  filter(!(raw_text %in% c("8FLiX.com TRANSCRIPT DATABASE", 
                           "FOR EDUCATIONAL USE ONLY", 
                           "\\", 
                           "This transcript is for educational use only.", 
                           "Not to be sold or auctioned.",
                           "8FLiX.com SCREENPLAY DATABASE")))

#prep for separation by speaker/action in order
script_deconstruct <- 
  script_filt %>%
  mutate(scene = cumsum(str_detect(raw_text, 
                                   pattern = "ENCANTO [0-9]+\\."))) %>%
  filter(!str_detect(raw_text, 
                     pattern = "ENCANTO [0-9]+\\.")) %>%
  mutate(speaker_flag = str_detect(raw_text, 
                                   pattern = "^[A-Z]+\\s*[A-Z]*\\s*([:punct:]|[A-Z])*$")) %>%
  mutate(section_flag = str_detect(raw_text, 
                                   pattern = "^$")) %>%
  mutate(standalone_flag = ifelse(lag(section_flag,1) == TRUE & 
                                    lead(section_flag,1) == TRUE, TRUE, FALSE)) %>%
  mutate(section_number = cumsum(section_flag)) %>%
  mutate(speaker = ifelse(speaker_flag == TRUE, raw_text, NA)) %>%
  mutate(section = ifelse(lag(section_flag, 1) == TRUE, raw_text, NA))

#set up and clean the tag column to provide info on each section
script_tagged <- script_deconstruct %>%
  mutate(tag = coalesce(speaker, section)) %>%
  mutate(tag = ifelse(str_detect(tag, "[a-z]"), "Action", tag)) %>%
  mutate(tag = str_remove(tag, pattern = " \\(V\\.O\\.\\)| \\(O\\.S\\.\\)| \\(CONT'D\\)| \\(INTO V\\.O\\.\\)")) %>%
  fill(tag, .direction = "down") %>%
  filter(!str_detect(raw_text, "^$")) %>%
  mutate(tag = ifelse(standalone_flag == TRUE, 'Action', tag)) %>%
  mutate(tag = ifelse(str_detect(tag, "\\s.*\\s.*\\s.*"), 'Action', tag)) %>%
  group_by(scene, section_number) %>%
  mutate(tag = first(tag)) %>% 
  mutate(linenumber = row_number()) %>% ungroup()

#remove unnecessary rows, consolidate, order
script_final <- script_tagged %>%
  mutate(keep_flag = (tag == 'Action' | linenumber > 1)) %>%
  filter(keep_flag) %>% 
  group_by(scene, section_number) %>%
  mutate(all_text = paste(raw_text, collapse = " ")) %>%
  select(scene, section_number, tag, all_text) %>% distinct() %>%
  ungroup() %>%
  group_by(scene) %>%
  arrange(section_number) %>%
  mutate(section = row_number()) %>%
  ungroup() %>%
  select(scene, section, tag, all_text)

#save out the final file
library(readr)
library(here)
write_csv(script_final, here("newcode", "encanto.csv"))
