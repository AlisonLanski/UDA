################################
### Cleaning Billboard Songs ###
################################

library(data.table)
library(stringr)

load("data/billboard_hot_song_links.RData")

# Now, we can start to clean up the text.
# The same clean up will get applied to 
# everything, so we will just make a function.

text_cleaning <- function(text_variable) {
  cleaning <- gsub("\n", "", text_variable)
  cleaning <- gsub("\\s{2,}", "", cleaning)
  clearning <- gsub("^\\s+|\\s+$", "", cleaning)
  return(cleaning)
}

all_goat_songs[, 
               colnames(all_goat_songs)] <- lapply(all_goat_songs, 
                                                   function(x) {
                                                     text_cleaning(x)
                                                   })

all_hot_songs[, 
               colnames(all_hot_songs)] <- lapply(all_hot_songs, 
                                                   function(x) {
                                                     text_cleaning(x)
                                                   })

# When we scraped the data originally, we could have put 
# the genre and the date into the data. Instead, I tend
# to prefer to pull them out of a link. Purely a matter
# of preference, but I would opt to process on the back
# end, as opposed to handling it on the front.

all_hot_songs$week <- str_extract(all_hot_songs$link, "[0-9]{4}.*$")

# I'd like to make life easy, so I need to check something first:
# Does every link contain the word "-songs"?

sum(!is.na(all_hot_songs$link)) == sum(grepl("-songs", all_hot_songs$link))

# That returns a TRUE, so I can safely make that assumption
# and reduce my cleaning by a step

# Here is a compound look around.
# Look behind charts/ (but don't include it)
# Look ahead of -song (but don't include it)

all_hot_songs$genre <- str_extract(all_hot_songs$link, 
                                   "(?<=charts/).*(?=-song)")

save(all_hot_songs, all_goat_songs, file = "data/billboard_cleaned.RData")
