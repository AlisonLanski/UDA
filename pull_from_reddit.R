#pulling some reddit data to use for sample models

#basic packages
library(tidyverse)
library(RedditExtractoR)

# create revised function locally
# because I want a couple more columns that are available via the API
# but aren't included in this function
build_thread_df <- function(json) {
  data.frame(
    date_utc = RedditExtractoR:::extract_json_attribute(json, "created_utc") |> 
      as.POSIXct(origin="1970-01-01") |>
      as.Date() |>
      format("%Y-%m-%d"),
    timestamp = RedditExtractoR:::extract_json_attribute(json, "created_utc"),
    title = RedditExtractoR:::extract_json_attribute(json, "title"),
    text = RedditExtractoR:::extract_json_attribute(json, "selftext"),
    subreddit = RedditExtractoR:::extract_json_attribute(json, "subreddit"),
    comments = RedditExtractoR:::extract_json_attribute(json, "num_comments"),
    score = RedditExtractoR:::extract_json_attribute(json, "score"),
    up_ratio = RedditExtractoR:::extract_json_attribute(json, "upvote_ratio"),
    url = paste0(RedditExtractoR:::REDDIT_URL, RedditExtractoR:::extract_json_attribute(json, "permalink")),
    stringsAsFactors = FALSE
  )
}

# replace the function in the package with this one
assignInNamespace(x = "build_thread_df", value = build_thread_df, ns = "RedditExtractoR")

#now pull ~1000 rows of data
df <- find_thread_urls(subreddit = "ultimate", sort_by = "new", period = "year")
df_other <- find_thread_urls(subreddit = "CHIbears", sort_by = "new", period = "year") 
df_another <- find_thread_urls(subreddit = "notredamefootball", sort_by = "new", period = "year")


#combine and clean up a little (I could clean up a lot more...)

#get it in the same format the students had to create
df_new <- df %>%
  mutate(sport = "ultimate",
         keyword = NA)

df_other_new <- df_other %>%
    mutate(sport = "football",
         keyword = NA)

df_another_new <- df_another %>%
    mutate(sport = "college",
         keyword = NA)

#ok we could do a LOT more to clean up the text aside from this, but... eh...
new_reddit <- rbind(df_new, df_other_new, df_another_new) %>%
  mutate(title = str_replace_all(title, pattern = "\031", replacement = "'"),
         text = str_replace_all(text, pattern = "\031", replacement = "'"))

#write it out
write_csv(new_reddit, "reddit_three.csv")
