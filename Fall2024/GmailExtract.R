
## process below is based on the code here:
# https://github.com/hbnsarah/gmail_data_extraction/tree/main
# which includes an HTML tag remover (not used below)



library(stringr)
library(stringi)
library(lubridate)

### TO GET THE MBOX FILE:
### Go to takeout.google.com
### Select (only) gmail
### Select (only) the label of stuff you want to extract
### Download the result files (when available), unzip, read in below using path

mbox_file <- "C:/Users/alanski/Downloads/takeout/Takeout/Mail/CAIDP.mbox"


mbox_content <- tolower(readLines(mbox_file, warn = FALSE))


# Apply the function to extract emails
emails_df <- extract_email_data(mbox_content)

relevant_lines <- mbox_content[grep("^(date:|subject:|from:|content-type:)", mbox_content)]

# Extract date & time field
dates_lines <- relevant_lines[grep("^date:", relevant_lines)]
# Date extraction ( dd - month name - year - HH:mm:ss )
dates <- str_extract(dates_lines, "\\d{1,2} [a-z]+ \\d{4} \\d{2}:\\d{2}:\\d{2}")
# Format string in date/time format
dates <- dmy_hms(dates)

from_lines <- relevant_lines[grep("^from:", relevant_lines)]
froms <- str_extract(from_lines, "<.+?>")
froms <- str_replace_all(froms, "[<>]", "")

# Extract mails Subject
subject_lines <- relevant_lines[grep("^subject:", relevant_lines)]
subjects <- str_replace(subject_lines, "^subject: ", "")

content_text_lines <- mbox_content[grep("^content-type: text/html", mbox_content, fixed = TRUE)]
content_texts <- str_extract_all(mbox_content, "<div dir=\"ltr\">.*?</div>")
content_texts <- unlist(content_texts)
content_texts <- str_remove_all(content_texts, "<.*?>")  

parts <- mbox_content[grep("_part_([0-9])+_([0-9])+.([0-9])+", mbox_content)]
part2 <- str_remove_all(parts, pattern = "(-|=)+")
part3 <- unique(part2)
part4 <- part3[grepl(x = part3, pattern = "^_part")]


messages <- data.frame(contents = character())

for (part in part4){
  print(part)
  position <- (grep(mbox_content, pattern = part))
  message <- mbox_content[position[2]:position[3]]
  message_rev <- toString(message)
  message_rev <- str_remove_all(message_rev, "=, ")
  message_rev <- str_remove_all(message_rev, paste0("------=", part, "[, ]?"))
  message_rev <- str_remove_all(message_rev, "content-type: text/plain; charset=utf-8, content-transfer-encoding: quoted-printable, , ")
  message_rev <- str_remove_all(message_rev, "subscribe to caidp | let us know your suggestions here. | privacy policy | contact | donate |     caidp | 1100 13th st nw, suite 800, washington, dc 20005 unsubscribe alanski@nd.edu update profile | constant contact data notice sent by admin@caidp.org powered by try email marketing for free today!,")
  message_rev <- str_remove_all(message_rev, "=e2=80=8a")
  message_rev <- str_remove_all(message_rev, "=e2=80=9[a-z]")
  message_rev <- str_remove_all(message_rev, "=e2=80=9[0-9]")
  messages <- rbind(messages, data.frame(contents = message_rev))
}


messages$date <- dates
messages$from <- froms
messages$subject <- subjects

## write out results!!!





#checking
library(tidytext)
emailwords <- unnest_tokens(messages, word, contents)
emailwords %>% count(word, sort = T) %>% 
  anti_join(stop_words, by = "word") %>% 
  slice_max(order_by = n, n = 20)

