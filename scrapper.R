library(rvest)
library(stringr)
library(dplyr)





urls <- c(
  "https://www.metacritic.com/tv/foundation/user-reviews/?filter=Negative%20Reviews",
  "https://www.metacritic.com/tv/chernobyl/user-reviews/?filter=Positive%20Reviews",
  "https://www.metacritic.com/tv/alien-earth/user-reviews/?filter=Negative%20Reviews",
  "https://www.metacritic.com/tv/bojack-horseman/user-reviews/?filter=Positive%20Reviews",
  "https://www.metacritic.com/tv/wednesday/user-reviews/?filter=Negative%20Reviews",
  "https://www.metacritic.com/tv/batwoman/user-reviews/?filter=Negative%20Reviews",
  "https://www.metacritic.com/tv/bridgerton/user-reviews/?filter=Positive%20Reviews",
  "https://www.metacritic.com/tv/batwoman/user-reviews/?filter=Positive%20Reviews",
  "https://www.metacritic.com/tv/wednesday/user-reviews/?filter=Positive%20Reviews",
  "https://www.metacritic.com/tv/alien-earth/user-reviews/?filter=Positive%20Reviews",
  "https://www.metacritic.com/tv/foundation/user-reviews/?filter=Positive%20Reviews",
  "https://www.metacritic.com/tv/bridgerton/user-reviews/?filter=Negative%20Reviews"
)

allReviews <- data.frame(
  reviews = character(),
  sentiment = character(),
  stringsAsFactors = FALSE
)

for (url in urls) {
  webPage <- read_html(url)
  
  comment <- webPage %>%
    html_elements("div.c-siteReview_quote.g-outer-spacing-bottom-small > span") %>%
    html_text2()
  
  # decide sentiment from URL
  sentiment_label <- ifelse(str_detect(url, "Positive"), "positive", "negative")
  
  # make a data.frame for this batch
  batch <- data.frame(
    reviews = comment,
    sentiment = sentiment_label,
    stringsAsFactors = FALSE
  )
  
  # append to master
  allReviews <- bind_rows(allReviews, batch)
  
  cat("Scraped", length(comment), sentiment_label, "reviews from", url, "\n")
}

# Final counts
table(allReviews$sentiment)

View(allReviews)
# Save to CSV
write.csv(allReviews, "reviews.csv", row.names = FALSE)



