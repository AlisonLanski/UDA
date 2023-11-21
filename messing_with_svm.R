#messing around with string-related kernels for svm

dat_str <- 
  dat %>%
  mutate(full_text = paste(text, title),
         sport = factor(sport))

library(caret)
set.seed(1234)
sample_set_str <- createDataPartition(y = dat_str$sport, p = .8, list = FALSE)
dat_train_str <- dat_str[sample_set_str, c("full_text", "sport")]
dat_test_str <- dat_str[sample_set_str, c("full_text", "sport")]

dat_train_str_label <- dat_train_str %>% pull(sport) 
dat_train_str_text <- dat_train_str %>% select(full_text) %>% t() %>% as.list()

#load the library for svm
library(kernlab)

#create the model: train on everything except the ID and label columns
#we're using 5-fold crossvalidation and a Cost of 1
set.seed(1234)

#runs really slowly (haven't made it through yet) 
#but i think it may be looking at character combos, not strings... I'm not sure...
svm_mod <- train(x = cbind(text = dat_train_str_text), y = dat_train_str_label,
                 method = "svmBoundrangeString",
                 trControl = trainControl(method = "cv", number = 3),
                 tuneGrid = expand.grid(length = 2, C = 1))

svm_mod


getModelInfo("svmBoundrangeString")

