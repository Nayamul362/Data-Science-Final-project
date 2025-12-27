# Load required libraries
library(tidyverse)
library(tidytext)
library(e1071)  # For Naive Bayes
library(caret)   # For confusion matrix and data splitting

# Read the cleaned data
df <- read_csv("cleaned_text.csv")

# Read the TF-IDF data
tfidf_data <- read_csv("tfidf_by_doc.csv")

library(dplyr)
library(tidyr)

# Rebuild model_data cleanly (adjust column names if yours differ)
model_data <- tfidf_data %>%
  left_join(df %>% select(doc_id, sentiment), by = "doc_id") %>%
  select(doc_id, stem, tf_idf, sentiment) %>%   # keep only the needed cols
  mutate(tf_idf = as.numeric(tf_idf)) %>%
  filter(!is.na(sentiment))

# Aggregate any duplicates just in case, then pivot
wide_data <- model_data %>%
  group_by(doc_id, sentiment, stem) %>%
  summarise(tf_idf = mean(tf_idf, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(doc_id, sentiment),
    names_from = stem,
    values_from = tf_idf,
    values_fill = list(tf_idf = 0) ,
    values_fn = list(tf_idf = mean)  # safe aggregator if pivot finds >1 value
  )

# Split data into training and testing sets (80% train, 20% test)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(wide_data$sentiment, p = 0.8, list = FALSE)
train_data <- wide_data[train_index, ]
test_data <- wide_data[-train_index, ]

# Train Naive Bayes model
# Note: We exclude the sentiment column from the features
nb_model <- naiveBayes(sentiment ~ ., data = train_data)

# Make predictions on test data
predictions <- predict(nb_model, test_data %>% select(-sentiment))

# Ensure the test sentiment is a factor
test_data$sentiment <- factor(test_data$sentiment)

# Match factor levels between predictions and reference
predictions <- factor(predictions, levels = levels(test_data$sentiment))

# Now compute confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$sentiment)
accuracy <- confusion_matrix$overall["Accuracy"]

# Print results
cat("Model Accuracy:", round(accuracy * 100, 2), "%\n")
print(confusion_matrix)

# Get feature importance (top words for each sentiment)
# Calculate mean TF-IDF for each word by sentiment
feature_importance <- model_data %>%
  group_by(sentiment, stem) %>%
  summarise(mean_tfidf = mean(tf_idf, na.rm = TRUE), .groups = 'drop') %>%
  group_by(sentiment) %>%
  slice_max(mean_tfidf, n = 10) %>%
  arrange(sentiment, desc(mean_tfidf))

# Display top words for each sentiment
cat("\nTop words for each sentiment (by mean TF-IDF):\n")
print(feature_importance)

# Optional: Save the model for future use
# saveRDS(nb_model, "naive_bayes_model.rds")