## API stuff
# 
# install.packages("RPublica")
# 
# 
# library(RPublica)
# library(curl)
# 
# np_search("propublica")
# 
# 
# npsearch <- function (q = NULL, order = NULL, sort = NULL, state = NULL, 
#           ntee = NULL, subsection = NULL, ...) 
# {
#   args <- formals()
#   if (!is.null(q)) 
#     args[names(args) == "q"] <- curl_escape(q)
#   if (!is.null(order)) 
#     args[names(args) == "q"] <- curl_escape(order)
#   names(args)[names(args) == "sort"] <- curl_escape("sort_order")
#   names(args)[names(args) == "state"] <- curl_escape("state[id]")
#   names(args)[names(args) == "ntee"] <- curl_escape("ntee[id]")
#   names(args)[names(args) == "subsection"] <- curl_escape("c_code[id]")
#   args <- paste("?", paste(names(args), args, sep = "=", collapse = "&"), 
#                 sep = "")
#   out <- ppQuery("search", "https://projects.propublica.org/nonprofits/api/v2/", 
#                  args = args, ...)
#   return(out)
# }
# op <- "search"
# baseurl <- "https://projects.propublica.org/nonprofits/api/v2/"
# args = 
# 
# ppQuery <- function (op, baseurl, args = NULL, ...) 
# {
#   print(paste(baseurl, op, ".json", args, sep = ""))
#   response <- GET(paste(baseurl, op, ".json", args, sep = ""), 
#                   ...)
#   stop_for_status(response)
#   fromJSON(content(response, as = "text"), flatten = TRUE)
# }
# 
# 
# npsearch(q = "propublica", max = 1)
# ppQuery




library(httr2)
library(jsonlite)
req <- request("https://projects.propublica.org/nonprofits/api/v2/search.json?q=propublica")
req

req <- request("https://www.federalregister.gov/api/v1/documents.json?fields[]=&per_page=5&order=&conditions[type][]=PRESDOCU&conditions[presidential_document_type][]=executive_order&conditions[president][]=barack-obama")
req <- httr2::request("https://www.federalregister.gov/api/v1/documents.json?fields[]=agencies&fields[]=document_number&fields[]=executive_order_number&fields[]=presidential_document_number&fields[]=publication_date&fields[]=raw_text_url&fields[]=title&fields[]=topics&fields[]=type&per_page=5&order=&conditions[type][]=PRESDOCU&conditions[presidential_document_type][]=executive_order&conditions[president][]=barack-obama")

req_result <- httr2::req_perform(req)
str(req_result)
req_string <- httr2::resp_body_json(req_result)
str(req_string)
this_doc_url <- req_string$results[[3]]$raw_text_url

reqdoc <- httr2::request(this_doc_url)
reqdoc_result <- httr2::req_perform(reqdoc)
reqdoc_final <- httr2::resp_body_string(reqdoc_result)
View(reqdoc_final)

#req_string <- readChar(req_result$body, nchar = length(req_result$body))
#strsplit(req_string, split = ",")

#better_string <- fromJSON(req_string, flatten = TRUE)

############################################################################

org_code <- better_string$organizations$ein[1]

req <- request(paste0("https://projects.propublica.org/nonprofits/api/v2/organizations/", org_code, ".json"))
req_result <- req_perform(req)
req_string <- readChar(req_result$body, nchar = length(req_result$body))
better_string_org <- fromJSON(req_string, flatten = TRUE)
better_string_org$filings_with_data$pdf_url[4]

install.packages("pdftools")
library(pdftools)

#works in principle, but these are scanned aka image files in the pdfs, so not readable as pdf_text
download.file(better_string_org$filings_with_data$pdf_url[4], "propub.pdf", mode = "wb")
pp_doc <- pdf_text("propub.pdf")

###################################################
install.packages("federalregister")
library(federalregister)

?fr_search
obama <- fr_search(type = "PRESDOCU", presidential_document_type='executive_order', 
                   president='barack-obama', per_page=10)

print(obama$results$excerpts[2])

doc <- obama$results$document_number[9]

docrecord <- fr_get(doc)
fulldoc <- httr::content(httr::GET(docrecord[[1]]$raw_text_url), "text", encoding = "UTF-8")
cat(substring(fulldoc, 1, 10000))

fr_test(presidential_document_type='executive_order',
          fields=c('executive_order_number','president','raw_text_url','document_number'),
          per_page=30)

fr_test(term='climate', publication_date=list(gte='2013-01-01',lte='2013-03-31'))

test_ob <- fr_test(type = "PRESDOCU", presidential_document_type='executive_order', 
          president='barack-obama', per_page=10)

fr_search

library(curl)

fr_test <- function (..., fields = NULL, per_page = NULL, page = NULL, order = "relevance", 
          version = "v1", getopts = NULL) 
{
  baseurl <- paste("https://www.federalregister.gov/api/", 
                   version, "/documents.json?", sep = "")
  query <- list(...)
  if ("publication_date" %in% names(query)) {
    w <- which(names(query) == "publication_date")
    p <- query$publication_date
    names(p) <- paste("publication_date][", names(p), sep = "")
    query <- query[-w]
    query <- c(query, p)
  }
  if ("effective_date" %in% names(query)) {
    w <- which(names(query) == "effective_date")
    p <- query$effective_date
    names(p) <- paste("effective_date][", names(p), sep = "")
    query <- query[-w]
    query <- c(query, p)
  }
  if ("cfr" %in% names(query)) {
    w <- which(names(query) == "cfr")
    p <- query$cfr
    names(p) <- paste("cfr][", names(p), sep = "")
    query <- query[-w]
    query <- c(query, p)
  }
  if ("near" %in% names(query)) {
    w <- which(names(query) == "near")
    p <- query$near
    names(p) <- paste("near][", names(p), sep = "")
    query <- query[-w]
    query <- c(query, p)
  }
  query <- paste(curl_escape(paste("conditions[", names(query), 
                                   "]=", query, sep = "")), collapse = "&")
  print(paste("query", query))
  if (!is.null(per_page) && as.numeric(per_page) > 1000) {
    stop("'per_page' cannot be greater than 1000")
  }
  else if (!is.null(per_page) & !is.null(page)) {
    p <- paste("per_page=", per_page, "&page=", page, sep = "")
  }
  else if (!is.null(per_page) & is.null(page)) {
    p <- paste("per_page=", per_page, sep = "")
  }
  else if (!is.null(page)) {
    p <- paste("page=", page, sep = "")
  }
  else {
    p <- NULL
  }
  if (!is.null(fields)) {
    fields <- paste(paste(curl_escape("fields[]"), fields, 
                          sep = "="), collapse = "&")
    args <- paste(fields, query, p, sep = "&")
  }
  else {
    args <- paste(query, p, sep = "&")
  }
  print(args)
  print(c(list(url = paste(baseurl, args, sep = "")), getopts))
  r <- do.call("GET", c(list(url = paste(baseurl, args, sep = "")), 
                    getopts))
  stop_for_status(r)
  response <- content(r, "text")
  out <- fromJSON(response)
  out$results <- lapply(out$results, `class<-`, "fedreg_document")
  return(out)
}


#############

####WORKS

#info here: https://evilinsult.com/api/

req <- request("https://evilinsult.com/generate_insult.php?lang=en&type=json")
req_result <- req_perform(req)
str(req_result)
resp_body_json(req_result)$insult
resp_body_json(req_result)$createdby

#### WHAT do we do with it?

######
req <- request("http://api.nobelprize.org/2.1/laureates?limit=2&sort=asc")
req_result <- req_perform(req)
str(req_result)
nice_result <- resp_body_json(req_result)
str(nice_result)
nice_result

## so we can get html back from wikipages and wikidata but it's pretty ugly and not easy to work with
req <- request("https://en.wikipedia.org/wiki/Michael_Spence")
req_result <- req_perform(req)
str(req_result)
nice_result <- resp_body_html(req_result)


#####################
# totally different
# works pretty well
# this site for instructions: https://www.datamuse.com/api/
# mess around with it


req <- request("https://api.datamuse.com/words?ml=ringing+in+the+ears")
req_result <- req_perform(req)
str(req_result)
nice_result <- resp_body_json(req_result)
for (i in 1:10){
  print(c(nice_result[[i]]$word, nice_result[[i]]$score))
}

purrr::list_rbind(nice_result)

nice_result[[1]]

jsonlite::flatten(nice_result, recursive = TRUE)
############################
#####
apple <- readxl::read_excel(path = "G:/My Drive/ITAO Teaching/Data Viz/Datasets/statista_appleshareprice.xlsx")
#########################

library(rvest)
document <- read_html("https://en.wikipedia.org/wiki/Star_Trek:_The_Original_Series")
doc_summary <- document %>% html_elements("div.mw-heading.mw_heading3")
