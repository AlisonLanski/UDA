# https://www.trade.gov/data

library(httr)

tarifs <- GET("https://api.trade.gov/gateway/v1/tariff_rates/search?q=canada", 
              add_headers(c(`Authorization` = "Bearer 1b7daff9-0f44-38b0-86f6-ee86e8f72c0d")))

content(tarifs, "text")
