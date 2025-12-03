library(tidyverse)
library(ggplot2)
library(reshape2)

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
<<<<<<< HEAD

# Creates and outputs Pearson correlation matrix
cor_matrix <- cor(players_clean, method = "pearson", use = "complete.obs")

# Convert matrix into long format for ggplot
cor_melt <- melt(cor_matrix)

# Save PNG file
png("correlation_matrix.png", width = 800, height = 600)

ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 3)), size = 6, color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1, 1),
                       name = "Pearson r") +
  labs(title = "Correlation Matrix (Pearson Method)",
       x = "Variables", y = "Variables") +
  theme_minimal(base_size = 16)

dev.off()

cat("PNG saved as: correlation_matrix.png\n")
=======
>>>>>>> 4ffc5ebb447781f099154e071ee90296a65d0d6a
