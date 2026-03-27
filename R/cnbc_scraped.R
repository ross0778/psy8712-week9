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

# Visualization
# Created a boxplot since source is categorical and length is continuous
cnbc_tbl %>% 
  ggplot(aes(x = source, y = length, fill = source)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Headline Length by Section", x = "Section", y = "Word Count") +
  theme_bw()

# Analysis
# Ran an ANOVA using aov()
cnbc_anova <- aov(length ~ source, data = cnbc_tbl)
# This runs a summary of the ANOVA and extracts the first element of the list, which is the ANOVA table. This will let us pull out each value for the Publication section
aov_summary <- summary(cnbc_anova)[[1]]
f <- aov_summary[["F value"]][1]
p <- aov_summary[["Pr(>F)"]][1]
df_between <- aov_summary[["Df"]][1]
df_within <- aov_summary[["Df"]][2]

# Publication
# The results of an ANOVA comparing lengths across sources was F(3, 130) = 5.28, p = .00. This test was statistically significant.
# I used sprintf in order to force two decimal places to show the p-value since it was too small to display using round(). "%.2f" specifies exactly two decimal points using fixed-point notation. Used regex to identify the leading zero and ifelse() to specify significant vs. not significant
cat(paste(
  "The results of an ANOVA comparing length across sources was F(",
  df_between, ", ", df_within, ") = ",
  sprintf("%.2f", f), ", p = ",
  str_remove(sprintf("%.2f", p), "^0"),
  ". This test ", ifelse(p < .05, "was", "was not"), " statistically significant.",
  sep = ""
))
