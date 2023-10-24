##########################
### Scraping Billboard ###
###  Top Genre Songs   ###
##########################

library(furrr)
library(future)
library(lubridate)
library(rvest)


# For any given hot link, we can append a date to the end like this:
# https://www.billboard.com/charts/rock-songs/2021-10-02

hot_links <- c(alt_rock = c("https://www.billboard.com/charts/rock-songs/"),
               alt = c("https://www.billboard.com/charts/hot-alternative-songs/"), 
               hard_rock = c("https://www.billboard.com/charts/hot-hard-rock-songs/"), 
               pop = c("https://www.billboard.com/charts/pop-songs/"), 
               adult_pop = c("https://www.billboard.com/charts/adult-pop-songs/"), 
               country = c("https://www.billboard.com/charts/country-songs/"), 
               christian = c("https://www.billboard.com/charts/christian-songs/"))

# They are usually updated on Saturday, so we will need to create a vector of
# Saturday dates. I'd like to go back 10 years, so I'm going to pick the 
# first saturday in 2011 and count forward from there:

first_date <- as.Date("2013-01-01")

current_date <- Sys.Date()

saturday_dates <- seq.Date(first_date, current_date, by = "week")

# The following links could updated by Billboard, but they are not going 
# to update weekly like the hot links.

goat_links <- c(hip_hop = "https://www.billboard.com/charts/greatest-r-b-hip-hop-songs", 
                rock = "https://www.billboard.com/charts/greatest-of-all-time-mainstream-rock-songs", 
                pop = "https://www.billboard.com/charts/greatest-of-all-time-pop-songs", 
                country = "https://www.billboard.com/charts/greatest-country-songs", 
                alt = "https://www.billboard.com/charts/greatest-alternative-songs", 
                adult_pop = "https://www.billboard.com/charts/greatest-adult-pop-songs")

# Now, we get to define a function to handle our scraping.

general_scrape_function <- function(link) {
  
  # Never a bad idea to throw a little random time out in
  # when scraping a lot of links from the same place:
  
  Sys.sleep(runif(n = 1, min = 0, max = 1))
  # You need to be defensive when scraping a lot of links. 
  # One error and the whole thing crashes, so best to throw it
  # into an exception handler:
  try_out <- tryCatch({
    initial_read <- read_html(link)
    # We need to do some checking with what is in the link -- the layout varies
    # across different page types, so we need to be a bit defensive about it.
    
    # We will grab both layout types. One should return the proper length
    # and the other will be a nodeset of 0.
    
    hot_layout <- html_nodes(initial_read, ".chart-element__information")
    
    regular_layout <- html_nodes(initial_read, ".chart-list-item")
    
    if(length(hot_layout) != 0) {
      song <- hot_layout |> 
        html_nodes(".chart-element__information__song") |> 
        html_text()
      
      artist <- hot_layout |> 
        html_nodes(".chart-element__information__artist") |> 
        html_text()
      
      complete <- data.frame(song = song, 
                             artist = artist, 
                             link = link)
    } else {
      song <- regular_layout |> 
        html_nodes(".chart-list-item__title-text") |> 
        html_text()
      
      artist <- regular_layout |> 
        html_nodes(".chart-list-item__artist") |> 
        html_text()
      
      complete <- data.frame(song = song, 
                             artist = artist, 
                             link = link)
    }
    
    return(complete)  
  }, error = function(e) {
    return(data.frame(song = NA, 
                      artist = NA, 
                      link = link))
  })
  return(try_out)  
}

quick_testing <- purrr::map_df(hot_links, general_scrape_function)

# Things look pretty solid and will clean once everything else is in.

weekly_links <- sapply(hot_links, function(x) paste0(x, saturday_dates), 
                       simplify = FALSE)

plan("future::multisession", workers = availableCores() - 1)

all_hot_songs <- future_map_dfr(unlist(weekly_links), general_scrape_function, 
                                .progress = TRUE)

all_goat_songs <- future_map_dfr(unlist(goat_links), general_scrape_function)

plan("sequential")

save(all_hot_songs, all_goat_songs,
     file = "data/billboard_hot_song_links_2023.RData")

