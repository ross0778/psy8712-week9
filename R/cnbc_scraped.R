# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)

# Data Import and Cleaning
# This creates the labels for the data I'm going to be scraping and includes the links for scraping 
sources <- c("Business", "Investing", "Tech", "Politics")
urls <- c(
  "https://www.cnbc.com/business/",
  "https://www.cnbc.com/investing/",
  "https://www.cnbc.com/technology/",
  "https://www.cnbc.com/politics/"
)

# Creates an empty list to gather the headlines from each category
list_cnbc <- list()

for(i in seq_along(sources)) {
  # I used read_html() to download and parse the HTML from the URL. Also used indexing here
  page <- read_html(urls[i])
  # html_elements() will find all the elements that match the CSS .Card-title and html_text() will extract the text from each element that matched
  headlines <- html_text(html_elements(page, ".Card-title"))
  # Create the tibble including the headlines, word counts, and the source names
  list_cnbc[[i]] <- tibble(
    headline = headlines,
    length = str_count(headlines, "\\S+"), # Used regex here
    source = sources[i]
  )
}
cnbc_tbl <- bind_rows(list_cnbc)

