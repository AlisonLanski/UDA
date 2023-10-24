## trying to scrape reddit

#install.packages("RedditExtractoR")

#this is the whole package --- it's not a scraper, it uses the API
library(RedditExtractoR)

dat <- find_subreddits("UltimateFrisbee")
View(dat)

turl <- find_thread_urls(keywords = "regionals",
                 #sort_by = "top",
                 subreddit ="ultimate",
                 period = "year")


team <- turl$url[grep(turl$title, pattern = "regionals")]

content <- get_thread_content(team)

user <- content$threads$author
users <- get_user_content(user)
content$comments$comment

#### I think we have potential for something good here

readr::write_csv(dat, "C:/Users/alanski/Downloads/reddit_ultimate_keyword.csv")
readr::write_csv(turl, "C:/Users/alanski/Downloads/reddit_ultimate_posts.csv")
readr::write_csv(content$comments, "C:/Users/alanski/Downloads/reddit_ultimate_threads.csv")


##############
max_url <- find_thread_urls(subreddit = "ultimate", period = "month")
max_content <- get_thread_content(max_url$url)


friz <- find_thread_urls(keywords = "frisbee", period = "month") 

content <- get_thread_content(team)

user <- content$threads$author
users <- get_user_content(user)
content$comments$comment


#### here's the deal
## with a keyword search, you can get a topic across lots of subreddits, top level only
##### keyword search has date, timestamp, title, text, usbreddit, url, comment count
## with the thread content search, you can dig down into multiple threads
##### with the thread content, you get 2 DFs in a list
##### threads has score, upratio, and comments (also the other stuff) for the top level
##### comments has score but no ratio and text, indexed so you can track the comment levels
######## you could count the comments under it using the levels 


friz[7, 4] <- gsub(x = friz[7,4], pattern = "\031", replacement = "'")
friz[7, 4] <- gsub(x = friz[7,4], pattern = "\n", replacement = " ")
friz[7, 4]
