library(tidyverse)
library(tidytext)

with_code <- list.files("Fall2024/", recursive = FALSE, pattern = "_Completed.Rmd", full.names = TRUE)

docs <- purrr::map(with_code, read_file) %>% 
   # helpful for me when looking at it, not actually necessary
   # because we just rename the docs by position below
   # and because we aren't splitting out the data by starting doc
   # if we want to look at it like that, would need to dynamically add a set_names vector 
  #purrr::set_names(c("A", "B", "C")) %>%
  purrr::map(data.frame) %>% 
  purrr::list_rbind(names_to = "id") %>%
  dplyr::rename(Text = 2)


common_functions <- str_extract_all(string = docs$Text, pattern = "(\\w+\\()|(\\w+\\.\\w+\\()") %>%
  unlist() %>%
  data.frame() %>%
  rename(functions = 1) %>%
  count(functions, sort = T, name = "count") %>%
  mutate(functions = str_remove(functions, "\\("))

View(common_functions)

#first time, then move it and get the URL
#googlesheets4::gs4_create(name = "UDA Vocabulary Fall 2024", sheets = common_functions)

#after that
googlesheets4::write_sheet(common_functions, ss = "https://docs.google.com/spreadsheets/d/1VNYcLUtZquwpNth-jcZRLwNjj6n9n0bH94ph6oGF4uk/edit?",
                           sheet = "common_functions")
