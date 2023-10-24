#######################
### Cleaning Lyrics ###
#######################

library(stringdist)
library(stringr)

load("data/all_lyrics.RData")

lyric_cleaner <- function(lyrics) {
  song_lyrics <- gsub("([a-z])([A-Z])", "\\1 \\2", lyrics)
  
  song_lyrics <- gsub("\\[(.*?)\\]", " ", song_lyrics)
  
  song_lyrics <- gsub("([0-9]{1,}?)Embed.*", " ", song_lyrics)
  
  song_lyrics <- gsub("[[:punct:]]", "", song_lyrics)
  
  song_lyrics <- gsub("^\\s|\\s$|\\s{2, }", "", song_lyrics)
  
  song_lyrics <- tolower(song_lyrics)
  
  return(song_lyrics)
}

artist_cleaner <- function(artist) {
  clean_artist <- gsub("^\\s|\\s$|\\s{2, }", "", artist)
  
  clean_artist <- tolower(clean_artist)
  
  return(clean_artist)
}

all_lyrics_info$lyrics <- lyric_cleaner(all_lyrics_info$lyrics)

all_lyrics_info$searched_artist <- artist_cleaner(all_lyrics_info$searched_artist)

all_lyrics_info$returned_artist <- artist_cleaner(all_lyrics_info$returned_artist)

all_lyrics_info$artist_dist <- stringdist(tolower(all_lyrics_info$searched_artist), 
                                          tolower(all_lyrics_info$returned_artist), 
                                          method = "jw")

filtered_lyrics <- all_lyrics_info[all_lyrics_info$artist_dist < 0.3587302, ]

filtered_lyrics$genre[which(is.na(filtered_lyrics$genre))] <- 
  stringr::str_extract(filtered_lyrics$original_link[which(is.na(filtered_lyrics$genre))], 
                     "adult-pop|pop|country|alternative|rock|hip-hop")
