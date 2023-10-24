### Twitter API ###

api_k <- "1SaEQ0rWyE2KS3Wm06DrAlcWq"

api_secret <- "xzsDZl1gGPEoYyeSeCkWVdxlmYYthAuwyhLSTDYq4j9iaQyhzR"

access_t <- "836938535471038464-BpCj1mrUU92FPIwzPiaiWX6hdxOj33p"

access_secret <- "F6RAAmSYqvzJbBTcpmWsDTsP7WRsBkQ74ptUDDasRyTNC"

# With V1, you would need to run the following in your
# terminal to produce the bearer:
 
`curl -u "1SaEQ0rWyE2KS3Wm06DrAlcWq:xzsDZl1gGPEoYyeSeCkWVdxlmYYthAuwyhLSTDYq4j9iaQyhzR" --data 'grant_type=client_credentials' 'https://api.twitter.com/oauth2/token'`

toke <- "AAAAAAAAAAAAAAAAAAAAALC%2FzQAAAAAAJrUtwt7YrPz50biiem4xunMrJbQ%3DEvnsHjcF4PSqfcOVZhgPdhrgTb9BTU31pnzc5WJSI0OsdJFMXX"

# This is for V1:

headers <- c(`Authorization` = sprintf('Bearer %s', access_t))

tweet_get <- GET("https://api.twitter.com/1.1/users/show.json?screen_name=rstudio", 
    httr::add_headers(.headers = c(`Authorization` = sprintf('Bearer %s', toke))))

jsonlite::fromJSON(content(tweet_get, "text"))

# And for V2:

v2_key <- "Dhc95fzNiDT87ZCVhz1K4UrEN"

v2_secret <- "IDpqd6DfWmnU5hlG98Jxw4ckbi0Db8ZysstPG2OdWUeahk0y3m"

v2_bearer <- "AAAAAAAAAAAAAAAAAAAAABKjYAEAAAAAvWsgTj%2BDmjWQ8aYNQiT8ciUMxgA%3DKptkxb7Tyk5ObxlfLf4w1hydLa8gM5g7cX1di04HLkx4IDN89p"

url_handle <- paste0('https://api.twitter.com/2/tweets/search/recent?query=from:rstudio&tweet.fields=created_at&expansions=author_id&user.fields=created_at')

GET(url_handle, add_headers(c(`Authorization` = sprintf('Bearer %s', v2_bearer))))

response <-GET(url_handle, 
               add_headers(c(`Authorization` = sprintf('Bearer %s', v2_bearer))))
httr::content(response, as = "text")



