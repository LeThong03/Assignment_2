library(tidyverse)
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

# 3. CONVERT TO NUMERIC
players_clean$Assists <- as.numeric(as.character(players_clean$Assists))
players_clean$Rating <- as.numeric(as.character(players_clean$Rating))
players_clean$age <- as.numeric(as.character(players_clean$age))
players_clean$Goals <- as.numeric(as.character(players_clean$Goals))

cat("=== DATA TYPE CHECK ===\n")
cat("Assists type:", class(players_clean$Assists), "\n")
cat("Rating type:", class(players_clean$Rating), "\n\n")

# 4. NOW USE CLEANED DATA FOR EVERYTHING!
cat("\n=== SUMMARY STATISTICS ===\n")
summary(players_clean)  

# Specific statistics for our variables of interest
cat("\n=== ASSISTS STATISTICS ===\n")
cat("Mean:", mean(players_clean$Assists, na.rm = TRUE), "\n")
cat("Median:", median(players_clean$Assists, na.rm = TRUE), "\n")
cat("SD:", sd(players_clean$Assists, na.rm = TRUE), "\n")
cat("Min:", min(players_clean$Assists, na.rm = TRUE), "\n")
cat("Max:", max(players_clean$Assists, na.rm = TRUE), "\n")

cat("\n=== RATING STATISTICS ===\n")
cat("Mean:", mean(players_clean$Rating, na.rm = TRUE), "\n")
cat("Median:", median(players_clean$Rating, na.rm = TRUE), "\n")
cat("SD:", sd(players_clean$Rating, na.rm = TRUE), "\n")
cat("Min:", min(players_clean$Rating, na.rm = TRUE), "\n")
cat("Max:", max(players_clean$Rating, na.rm = TRUE), "\n")

# 5. PREPARE FOR CORRELATION
# Remove rows with missing values in Assists or Rating
analysis_data <- players_clean %>%
  filter(!is.na(Assists) & !is.na(Rating))

cat("\n=== SAMPLE SIZE FOR CORRELATION ===\n")
cat("Players with complete data:", nrow(analysis_data), "\n")
cat("Players excluded (missing data):", nrow(players_clean) - nrow(analysis_data), "\n")

# 6. CORRELATION ANALYSIS
cat("\n=== PEARSON CORRELATION ANALYSIS ===\n")

# Calculate Pearson correlation coefficient
correlation_result <- cor.test(analysis_data$Assists, 
                               analysis_data$Rating, 
                               method = "pearson")

# Display results
cat("\n--- Correlation Test Results ---\n")
print(correlation_result)

# Extract key values
r_value <- correlation_result$estimate
p_value <- correlation_result$p.value
confidence_interval <- correlation_result$conf.int

cat("\n--- KEY FINDINGS ---\n")
cat("Pearson's r:", round(r_value, 3), "\n")
cat("R-squared:", round(r_value^2, 3), "\n")
cat("P-value:", format(p_value, scientific = TRUE), "\n")
cat("95% Confidence Interval: [", round(confidence_interval[1], 3), ",", 
    round(confidence_interval[2], 3), "]\n")
cat("Sample size (n):", nrow(analysis_data), "\n")