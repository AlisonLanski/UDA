df <- read_csv("C:/Users/alanski/Downloads/reddit_class_2023.csv")

df2 <- df %>% select(-person)

write_csv(df2, "C:/Users/alanski/Downloads/reddit_everyone_2023.csv") 


df <- read_csv("C:/Users/alanski/Downloads/NDBasketball_articles_16-23.csv")

str(df)

df %>% count(Date) %>%
  ggplot(
    aes(x = Date, y = n)
  ) +
  geom_line()

max(df$Date)

df %>%
  write_csv("C:/Users/alanski/Downloads/basketball_2016to2022seasons.csv")

