library(tidyverse)
library(tidytext)

with_code <- list.files("Fall2023/", recursive = TRUE, pattern = "Completed.Rmd", full.names = TRUE)

docs <- purrr::map(with_code, read_file) %>% 
  purrr::set_names(c("A", "B", "C")) %>% 
  purrr::map(data.frame) %>% 
  purrr::list_rbind(names_to = "id") %>%
  dplyr::rename(Text = 2)


common_functions <- str_extract_all(string = docs$Text, pattern = "\\w+\\(") %>%
  unlist() %>%
  data.frame() %>%
  rename(functions = 1) %>%
  count(functions, sort = T, name = "count") %>%
  mutate(functions = str_remove(functions, "\\("))

View(common_functions)

#first time, then move it and get the URL
# googlesheets4::gs4_create(name = "UDA Vocabulary Fall 2023", common_functions)

#after that
googlesheets4::write_sheet(common_functions, ss = "https://docs.google.com/spreadsheets/d/18995RSFOJViZaB0TkhCZYJZI5nnZPHnbyLYBb0pX8S0/edit#gid=702396699",
                           sheet = "common_functions")
