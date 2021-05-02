# Libraries
library(tidyverse)
library(rvest)

# Function for extracting next-page urls
url_extract <- function(extension, n_pages) {
  urls <- c()
  start_url <- "https://dk.trustpilot.com/"
  for (i in 1:n_pages) {
    url <- paste0(start_url, extension)
    urls <- c(urls, ifelse(url %in% urls, character(0), url))
    new_url <- read_html(url) %>% 
               html_nodes(".pagination-container") %>%
               html_nodes("a") %>%
               html_attr("href")
    extension <- ifelse(length(new_url) > 1, new_url[2], new_url)
  }
  na.omit(urls)
}

# urls of all review pages
voi_urls <- url_extract("review/voiscooters.com", 10) 
tier_urls <- url_extract("review/www.tier.app", 10)
fynbus_urls <- url_extract("review/www.fynbus.dk", 10)

# Function for extracting the review (both title and text)
reviews <- function(url) {
  read_html(url) %>% 
    html_nodes(".review-content__body") %>% 
    html_text() %>% 
    str_replace_all("[\n]", "") %>% 
    str_trim(side = "both")
}

# Extracting reviews 
voi_reviews <- lapply(voi_urls, reviews) %>% 
  unlist() %>% 
  data.frame()

tier_reviews <- lapply(tier_urls, reviews) %>% 
  unlist() %>% 
  data.frame()

fynbus_reviews <- lapply(fynbus_urls, reviews) %>% 
  unlist() %>% 
  data.frame()

# Binding all the reviews to same dataframe
reviews <- data.frame(transport = c(rep("voi", nrow(voi_reviews)), rep("tier", nrow(tier_reviews)),
                         rep("fynbus", nrow(fynbus_reviews))),
                      review = rbind(voi_reviews, tier_reviews, fynbus_reviews))

write.csv(reviews, "trustpilot-reviews.csv")
