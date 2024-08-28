
# Load necessary libraries
library(tidyverse)
library(rvest)
library(xml2)

# URLs for scraping
allYearHTML <- c(
  'http://www.baseball-reference.com/register/league.cgi?code=JPPL&class=Fgn', #パ
  'http://www.baseball-reference.com/register/league.cgi?code=JPCL&class=Fgn'  #セ
)

# Custom string manipulation functions
splitLeft <- function(str, separ) {
  # Returns the left part of the string based on separator
  strsplit(str, separ)[[1]][1]
}

splitRight <- function(str, separ) {
  # Returns the right part of the string based on separator
  tail(strsplit(str, separ)[[1]], 1)
}

# Sample function for scraping data (Placeholder for actual implementation)
scrape_data <- function(url) {
  webpage <- read_html(url)
  # Add more scraping logic here
  # For example, extracting tables or specific elements
}

# Loop through all URLs and apply scraping
results <- map(allYearHTML, scrape_data)

# Further processing and analysis
# Add your code here for data analysis and storage
