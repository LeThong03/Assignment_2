# Load necessary libraries
library(tidyverse)
library(corrplot)

# Load the dataset
players_data <- read.csv("Players_Score.csv")

# Check the original dataset
cat("=== ORIGINAL DATASET ===\n")
cat("Total rows:", nrow(players_data), "\n")
cat("Total columns:", ncol(players_data), "\n")
cat("Column names:", paste(names(players_data), collapse=", "), "\n\n")

# Check for duplicates
duplicates_count <- sum(duplicated(players_data))
cat("=== DUPLICATE ANALYSIS ===\n")
cat("Number of duplicate rows:", duplicates_count, "\n")
cat("Percentage of duplicates:", round(duplicates_count/nrow(players_data)*100, 2), "%\n\n")

# Remove duplicates
players_clean <- players_data %>%
  distinct()

# Check the cleaned dataset
cat("=== CLEANED DATASET ===\n")
cat("Rows after cleaning:", nrow(players_clean), "\n")
cat("Rows removed:", nrow(players_data) - nrow(players_clean), "\n\n")

# Correlation analysis
# Select numeric columns
numeric_data <- players_clean %>% select_if(is.numeric)

# Compute correlations
cor_pearson <- cor(numeric_data, method = "pearson")
cor_spearman <- cor(numeric_data, method = "spearman")
cor_kendall <- cor(numeric_data, method = "kendall")

# Output correlation matrices
cat("=== Pearson Correlation ===\n")
print(cor_pearson)

cat("\n=== Spearman Correlation ===\n")
print(cor_spearman)

cat("\n=== Kendall Correlation ===\n")
print(cor_kendall)

# Correlation tests
cat("\n=== Pearson Test ===\n")
print(cor.test(players_clean$Rating, players_clean$Mins, method = "pearson"))

cat("\n=== Spearman Test ===\n")
print(cor.test(players_clean$Rating, players_clean$Mins, method = "spearman"))

cat("\n=== Kendall Test ===\n")
print(cor.test(players_clean$Rating, players_clean$Mins, method = "kendall"))

# Visualization: Histograms
# Histogram for Rating
ggplot(players_clean, aes(x = Rating)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Player Ratings",
       x = "Player Rating",
       y = "Frequency") +
  theme_minimal()

# Histogram for Mins
ggplot(players_clean, aes(x = Mins)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Minutes Played",
       x = "Minutes Played",
       y = "Frequency") +
  theme_minimal()