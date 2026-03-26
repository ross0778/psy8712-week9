# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)
library(jsonlite)

# Data Import and Cleaning
urls_r <- find_thread_urls(
  subreddit = "rstats",
  sort_by = "new",
  period = "month"
)

  