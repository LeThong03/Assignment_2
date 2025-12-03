library(tidyverse)
install.packages("tidyverse", type="source")
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

# Converted Assists and Rating to numeric
players_clean$Assists <- as.numeric(as.character(players_clean$Assists))
players_clean$Rating  <- as.numeric(as.character(players_clean$Rating))

# Removed rows with missing values in these columns
players_clean <- na.omit(players_data[, c("Assists", "Rating")])

# Calculates and output Pearson correlation
correlation_result <- cor(players_clean$Assists, players_clean$Rating, method = "pearson", use = "complete.obs")
correlation_result

# Run significance test (gives r and p-value)
test_result <- cor.test(players_clean$Assists, players_clean$Rating, method = "pearson")
test_result