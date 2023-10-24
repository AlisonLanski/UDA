library(magrittr)

rec_sys_data <- read.csv("data/recommender_system_data (Responses) - Form Responses 1.csv")

name_cleaning <- gsub("^.*it\\.\\.\\.", "", names(rec_sys_data))

name_cleaning <- gsub("\\.\\.\\..*", "", name_cleaning)

name_cleaning <- gsub("\\.*$", "", name_cleaning)

name_cleaning <- gsub("\\.+", "_", name_cleaning)

name_cleaning[3:5] <- c("hobbies", "fave_song", "fave_movie")

name_cleaning <- tolower(name_cleaning)

colnames(rec_sys_data) <- name_cleaning 

rec_sys_data[, 6:length(name_cleaning)] <- lapply(name_cleaning[6:length(name_cleaning)], function(x) {
  ifelse(grepl("garbage", rec_sys_data[, x]), 1, 
         ifelse(grepl("fine", rec_sys_data[, x]), 2, 
                ifelse(grepl("slaps|masterpiece", rec_sys_data[, x]), 3, 0)))
})

rec_sys_data$hobbies <- tolower(gsub("/", "_", rec_sys_data$hobbies))

hobby_types <- sort(unique(gsub("\\s", "", unlist(strsplit(rec_sys_data$hobbies, ",")))))

rec_sys_data <- tidyr::separate(data = rec_sys_data, 
                                col = hobbies, 
                                into = hobby_types, 
                                sep = ",")

hobbies_melted <- reshape2::melt(rec_sys_data[, c("email_address", hobby_types)], 
                                 id.vars = "email_address", 
                                 measure.vars = hobby_types)

hobbies_out <- reshape2::dcast(na.omit(hobbies_melted[, c("email_address", "value")]), 
                               email_address ~ value, 
                               length)

rec_sys_data <- tidyr::separate(data = rec_sys_data, 
                         col = fave_song, 
                         into = paste("fave_song_", 1:5), 
                         sep = ",")

songs_fave <- grep("fave_song_", names(rec_sys_data), value = TRUE)

songs_melted <- reshape2::melt(rec_sys_data[, c("email_address",
                                                songs_fave)], 
                               id.vars = "email_address", 
                               measure.vars = songs_fave)

songs_melted$value <- tolower(songs_melted$value)

songs_melted$value <- gsub("^\\s|\\s$", "", songs_melted$value)

songs_melted$value <- gsub("\\s", "_", songs_melted$value)

songs_out <- reshape2::dcast(na.omit(songs_melted[, c("email_address", "value")]), 
                             email_address ~ value, 
                             length)

rec_sys_data$fave_movie <- tolower(rec_sys_data$fave_movie)

rec_sys_data <- tidyr::separate(data = rec_sys_data, 
                                col = fave_movie, 
                                into = paste0("fave_movie_", 1:5), 
                                sep = ",")

movie_fave <- grep("fave_movie", names(rec_sys_data), value = TRUE)

movie_melted <- reshape2::melt(rec_sys_data[, c("email_address",
                                                movie_fave)], 
                               id.vars = "email_address", 
                               measure.vars = movie_fave)

movie_melted$value <- stringr::str_squish(gsub("^\\s|\\s$|'|:", 
                                               "", 
                                               movie_melted$value))

movie_melted$value <- gsub("\\s", "_", movie_melted$value)

movie_out <- reshape2::dcast(na.omit(movie_melted[, c("email_address", "value")]), 
                             email_address ~ value, 
                             length)

drop_cols <- c(which(colnames(rec_sys_data) %in% hobby_types), 
               grep("fave", colnames(rec_sys_data)))

rec_sys_data <- rec_sys_data[, -c(drop_cols)]

rec_sys_data <- dplyr::left_join(rec_sys_data, songs_out) %>%  
  dplyr::left_join(., movie_out)

rec_sys_data[is.na(rec_sys_data)] <- 0

rec_sim <- lsa::cosine(t(as.matrix(rec_sys_data[, -c(1:2)])))

rec_sim <- cbind(email = rec_sys_data$email_address, 
                 as.data.frame(rec_sim))

colnames(rec_sim)[2:length(colnames(rec_sim))] <- rec_sim$email

rec_sim$match <- sapply(rec_sim[, 2:length(rec_sim)], function(x) {
  max_match <- max(x[x < 1])
  rec_sim$email[which(x == max_match)[1]]
})
