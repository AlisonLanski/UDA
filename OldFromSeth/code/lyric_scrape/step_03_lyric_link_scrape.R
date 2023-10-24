#######################
### Scraping Lyrics ###
#######################

library(httr)
library(rvest)

load("data/billboard_cleaned.RData")

# For each of creating links to genius, I'm going to drop 
# "Featuring", go lower case, and replace spaces with plus signs
 
text_cleaning <- function(text) {
  cleaned <- gsub("Featuring.*", 
                  "", 
                  text)
  cleaned <- gsub("[[:punct:]]", "", cleaned)
  cleaned <- gsub("^\\s|\\s$|\\s{2,}", "", cleaned)
  cleaned <- tolower(cleaned)
  cleaned <- gsub("\\s", "+", cleaned)
  return(cleaned)
}

all_goat_songs$clean_artist <- text_cleaning(all_goat_songs$artist)

all_goat_songs$clean_song <- text_cleaning(all_goat_songs$song)

all_goat_songs$artist_song_search <- paste(all_goat_songs$clean_artist, 
                                           all_goat_songs$clean_song, 
                                           sep = "+")

all_hot_songs$clean_artist <- text_cleaning(all_hot_songs$artist)

all_hot_songs$clean_song <- text_cleaning(all_hot_songs$song)

all_hot_songs$artist_song_search <- paste(all_hot_songs$clean_artist, 
                                          all_hot_songs$clean_song, 
                                          sep = "+")

lyric_links <- function(song, artist, search, original_link,
                        week = NA, genre = NA) {
  link <- paste0("https://genius.com/api/search/multi?per_page=1&q=", 
                 search)
  
  out <- tryCatch({
    Sys.sleep(runif(1, .1, 1))
    
    link_request <- GET(link)
    
    lyric_return <- jsonlite::fromJSON(content(link_request, as = "text"))
    
    lyric_path <- lyric_return$response$sections$hits[[1]]$result$path[1]
    
    lyric_link <- paste0("https://genius.com", lyric_path)[1]
    
    returned_artist <- 
      lyric_return$response$sections$hits[[1]]$result$primary_artist$name[1]
    
    returned_song <- lyric_return$response$sections$hits[[1]]$result$title[1]
    
    returned_data <- data.frame(searched_song = song, 
                                returned_song = returned_song, 
                                searched_artist = artist, 
                                returned_artist = returned_artist, 
                                lyric_link = lyric_link, 
                                original_link = original_link,
                                week = week, 
                                genre = genre)
    
    return(returned_data)
  }, 
  error = function(e) {
    data.frame(searched_song = song, 
               returned_song = NA, 
               searched_artist = artist, 
               returned_artist = NA, 
               lyric_link = link, 
               original_link,
               week = NA, 
               genre = NA)
  })
  return(out)
}

debugonce(lyric_links)

lyric_links(all_hot_songs$song[1], 
            all_hot_songs$artist[1], 
            all_hot_songs$artist_song_search[1], 
            all_hot_songs$link[1],
            all_hot_songs$week[1], 
            all_hot_songs$genre[1])

plan("future::multisession", workers = availableCores() - 1)

all_lyric_links <- furrr::future_pmap_dfr(list(all_hot_songs$song, 
                                           all_hot_songs$artist, 
                                           all_hot_songs$artist_song_search, 
                                           all_hot_songs$link, 
                                           all_hot_songs$week, 
                                           all_hot_songs$genre), 
                                      lyric_links, .progress = TRUE)

all_goat_links <- furrr::future_pmap_dfr(list(all_goat_songs$song, 
                                              all_goat_songs$artist, 
                                              all_goat_songs$artist_song_search, 
                                              all_goat_songs$link), 
                                         lyric_links, .progress = TRUE)

plan("sequential")

save(all_lyric_links, all_goat_links, file = "data/genius_lyric_links.RData")
