# install.packages("reticulate")
# 
# Sys.which("python")

library(word2vec)
#install.packages("word2vec")

dat <- all_lyrics[1:20,]
View(dat)
datw <- tolower(all_lyrics$lyrics)

set.seed(34567)
#takes... about a minute
model <- word2vec(x = datw, type = "cbow", dim = 15, iter = 20)

#puts it into a matrix: word per row, each embedding vector as a column 
embedding <- as.matrix(model)

pred1 <- predict(model, c("time", "love"), type = "embedding")
pred2 <- predict(model, c("time", "love"), type = "nearest", top_n = 5)

summary(model, "vocabulary")

pred3 <- predict(model, c("game", "lose"), type = "embedding")
new <- pred3["game", ] - pred3["lose", ]# + pred3["woman", ]
predict(model, newdata = new, type = "nearest", top_n = 5)

### different method
set.seed(34567)
#takes... about a minute
model2 <- word2vec(x = datw, type = "skip-gram", window = 6, dim = 15, iter = 20)

#puts it into a matrix: word per row, each embedding vector as a column 
embedding <- as.matrix(model)

pred1 <- predict(model, c("time", "love"), type = "embedding")
pred2 <- predict(model, c("time", "love"), type = "nearest", top_n = 5)

summary(model, "vocabulary")

pred3 <- predict(model, c("game", "lose"), type = "embedding")
new <- pred3["game", ] - pred3["lose", ]# + pred3["woman", ]
predict(model, newdata = new, type = "nearest", top_n = 5)
