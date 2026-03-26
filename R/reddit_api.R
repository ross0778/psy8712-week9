# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(httr)
library(jsonlite)

# Data Import and Cleaning
# I tried to use RedditExtractoR but it didn't seem to be returning the upvotes data, so I used a function called grab_page() to wrap the GET() call. I used after = NULL so that the first call works without a token.
grab_page <- function(after = NULL) {
  scraped_response <- GET("https://www.reddit.com/r/rstats/.json",
                          user_agent("Graduate student at UMN ross0778@umn.edu"),
                          query = list(limit = 100, after = after))
  fromJSON(content(scraped_response, as = "text"))
}
# Since Reddit uses Unix timestamps, I converted 30 days to seconds in order to estimate the month, subtracting this number from the current time. Then as.numeric() converts this to a Unix timestamp.
one_month <- as.numeric(Sys.time()) - 2592000 #this is 30 days converted to seconds
# This makes an empty list to gather the posts from across pages
every_post <- list()
# This means no after token on the first call so that Reddit will begin from the most recent post
after <- NULL

repeat {
  # This grabs the current page and includes after, which is null on the first iteration
  page <- grab_page(after)
  # This adds that page's posts to the list
  every_post <- c(every_post, list(page$data$children$data))
  # This makes sure the after token is updated with the current page's value for the next iteration
  after <- page$data$after
  # This means the iteration will stop when the oldest post on the page is more than 30 days old
  if(min(page$data$children$data$created_utc) < one_month) break
}

# This binds all the data frames from each page into one
every_post <- bind_rows(every_post)

# This creates the tibble including each post's title, upvotes, and comments
rstats_tbl <- tibble(
  post = every_post$title,
  upvotes = every_post$score,
  comments = every_post$num_comments
)

# Visualization
rstats_tbl %>% 
  ggplot(aes(x = comments, y = upvotes)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Comments to Upvotes",
       x = "Comments",
       y = "Upvotes"
       ) +
  theme_bw()
           


  
  