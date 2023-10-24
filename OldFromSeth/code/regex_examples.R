# I was so happy to find this Amazon review data a few years ago:
# https://jmcauley.ucsd.edu/data/amazon/
# They also have some other cool data there, too.
# Unfortunately, I learned a lesson about json not having any real 
# standards and R won't read unpure json. That is okay, though, 
# because we can just make the json into a more readable structure
# for R.

link <- "https://www3.nd.edu/~sberry5/data/reviews_Musical_Instruments_5.json"

# You might not get to use readLines too much, but it is a classic standby.
# It can read any text-based file, line by line.

musicalInstruments <- readLines(link)

# The json we are dealing with isn't pure json, so we need to write
# a function to make it work. Our goal is to get things separated by
# a comma -- shouldn't be too tricky.

musicalInstruments <- paste(unlist(lapply(musicalInstruments, function(x) {
  paste(x, ",", sep = "")
})), collapse = "")

# This will just paste an open bracket at the beginning of this
# long string and a closed bracket at the end. A real json file
# starts and ends with a bracket.

musicalInstruments <- paste("[", musicalInstruments, "]", sep = "")

# Now we can use something that really isn't a regular expression, 
# but a straightforward text pattern:

musicalInstruments <- gsub("},]", "}]", musicalInstruments)

# Now we have a string that we can read as json!

review_df <- jsonlite::fromJSON(musicalInstruments)

head(review_df)

# Check that reviewTime variable out for its format:

# 02 28, 2014

# That really isn't much of a date format, so we should
# probably get something more usable. To rearrange it, 
# we will need to use those capturing groups. Before
# we do that, let's explain the pattern that we see:
# exactly 2 numbers -- [0-9]{2,} -- can be simplified to [0-9]{2}
# followed by a space -- \\s, 
# followed by 2 numbers -- [0-9]{2,}, 
# followed by a comma -- ,, 
# followed by a space -- \\s, 
# followed by 4 numbers -- [0-9]{4,} 

# We only want to rearrange the numbers, so we don't need to include
# everything within the capturing groups, just the numbers will do.

# Remember that the capturing groups can be referred to with numbers. 
# The first set of digits (what is currently the month) will be named \\1
# The next set is the day and it will be named \\2
# Finally, the year is named \\3
# We will flip the order, so that it goes \\3, \\2, \\1, separated by
# dashes.

gsub(pattern = "([0-9]{2})\\s([0-9]{2}),\\s([0-9]{4})", 
     replacement = "\\3-\\2-\\1", 
     review_df$reviewTime)

# As you go through those results, you might see where our pattern didn't
# match everything. Why might that be? That second set of digits only had 1
# digit. In other words, our pattern was too restrictive. How do we take a
# range of value as a quantifier -- we just throw them into the {}, 
# {1, 2}. We are saying that we want that part of the pattern to have 1 or
# 2 digits.

gsub(pattern = "([0-9]{2})\\s([0-9]{1,2}),\\s([0-9]{4})", 
     replacement = "\\3-\\2-\\1", 
     review_df$reviewTime)

# Since we have that in pretty good shape now, we can assign that variable:

review_df$reviewTime <- gsub(pattern = "([0-9]{2})\\s([0-9]{1,2}),\\s([0-9]{4})", 
                             replacement = "\\3-\\2-\\1", 
                             review_df$reviewTime)

# Cool, but we still have some inconsistencies in our values; essentially, 
# we need to take our single digit days and add a zero to them. The pattern
# we see for the offending values is hyphen, digit, hyphen. What we don't want
# to do is replace the digit, just add something to it. We can use another
# capture group to make that work.

# Our expression is going to be
# -, followed by
# ([0-9]), a grouped numeric range, followed by
# -
# Giving us this: -([0-9])-

# Our replacement is pretty simple. We are going to keep our first hyphen, 
# add a 0, place our captured group, and then keep the last hyphen.
# -0\\1-

gsub(pattern = "-([0-9])-", 
     replacement = "-0\\1-", 
     review_df$reviewTime)

# Looks fine, so let's assign:

review_df$reviewTime <- gsub(pattern = "-([0-9])-", 
                             replacement = "-0\\1-", 
                             review_df$reviewTime)

# Now that we have that cleaned up, let's just see how many reviews contain
# some form of "Good". We could keep rocking our base R functions, 
# but we can make life easier by using some stringr stuff.

library(stringr)

# Note that every function in stringr holds the same argument order, 
# where string (your actual data) is the first argument:

str_count(string = review_df$reviewText, 
          pattern = "[Gg]ood")

# Wonder if any user names are hyphenated?

str_detect(string = review_df$reviewerName, 
           pattern = "\\w+-\\w+") # This is equivalent to grepl

# Seems to be at least a few TRUEs in there, so let's see what those values are:

str_subset(string = review_df$reviewerName, 
           pattern = "\\w+-\\w+") # This is equal to grep(., ., value = TRUE)

# Purely for fun, let's get the first word of the reviews:

str_extract_all(string = review_df$reviewText, 
                pattern = "^\\w+")

# Remember that \w is a word character.

# That isn't too exciting, so let's get the first or the last word:

str_extract_all(string = review_df$reviewText, 
                pattern = "^\\w+|\\w+$")

# Something doesn't really feel right there, so we could probably 
# imagine that we are missing punctuation.

str_extract_all(string = review_df$reviewText, 
                pattern = "^\\w+|\\w+.$")

# Much closer to what we would actually want to extract there.

# I'm curious to see a user's nickname within their name. If you give
# that column a look, you will see there are quotations in there 
# that might be worth pulling out. Since we are going to be looking for
# double quotations, we are going to open our pattern with single quotes.

str_extract_all(string = review_df$reviewerName, 
                pattern = '".*"')

# I think that we all want to know what "Bob in Big Bear Ca" thinks about 
# his purchase.

# People will sometimes complain how the cost of something within a review, 
# so let's see what we can find. Note that we are looking for a literal
# dollar sign, so we will need to escape it.

str_extract_all(string = review_df$reviewText, 
                pattern = "\\$\\d+")

# I don't want to sort through all of those empty characters, so
# let's make life easy.

dollars <- str_extract_all(string = review_df$reviewText, 
                           pattern = "\\$\\d+")

dollars[dollars != "character(0)"]

# Instead of using a [0-9] range, we just used the \\d for digit.

# You can see that we aren't doing anything to grab potential decimals.
# We need to add a conditional period (i.e., it could be there or not), 
# and then between 0 and 2 digits.

str_extract_all(string = review_df$reviewText, 
                pattern = "\\$\\d+\\.?\\d{0,2}")
