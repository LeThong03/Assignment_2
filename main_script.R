library(tidyverse)
library(corrplot)
library(dplyr)
library(ggplot2)
install.packages("corrplot")
install.packages("dplyr")
install.packages("ggplot2")
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


#Correlation analysis
# Select numeric columns
numeric_data <- players_clean %>% select_if(is.numeric)

# Pearson (assumes normality)
cor_pearson <- cor(numeric_data, method = "pearson")
cor_spearman <- cor(numeric_data, method = "spearman")
cor_kendall <- cor(numeric_data, method = "kendall")

cat("=== Pearson Correlation ===\n")
print(cor_pearson)

cat("\n=== Spearman Correlation ===\n")
print(cor_spearman)

cat("\n=== Kendall Correlation ===\n")
print(cor_kendall)

# Pearson (normal data)
cor.test(players_clean$Rating, players_clean$Mins, method = "pearson")

# Spearman (non-normal / ordinal)
cor.test(players_clean$Rating, players_clean$Mins, method = "spearman")

# Kendall (small samples, many ties)
cor.test(players_clean$Rating, players_clean$Mins, method = "kendall")



#Create and save boxplot
png("boxplot_by_Category.png", width = 800, height = 600)

# Create the boxplot to display in the Plots pane
ggplot(players_clean, aes(x = as.factor(Assists), y = Rating)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Boxplot of Rating by Category",
       x = "Assists",
       y = "Rating") +
  theme_minimal()

print(boxplot)


