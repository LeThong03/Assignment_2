# =====================================================
# LOAD REQUIRED PACKAGES
# =====================================================
# Install once if needed:
# install.packages("tidyverse")

library(tidyverse)
library(ggplot2)

# =====================================================
# LOAD DATA
# =====================================================
players_data <- read.csv("Players_Score.csv", stringsAsFactors = FALSE)

# Clean column names (remove hidden spaces)
names(players_data) <- trimws(names(players_data))

cat("=== ORIGINAL DATASET ===\n")
cat("Total rows:", nrow(players_data), "\n")
cat("Total columns:", ncol(players_data), "\n")
cat("Column names:", paste(names(players_data), collapse = ", "), "\n\n")


# =====================================================
# DUPLICATE ANALYSIS
# =====================================================
duplicates_count <- sum(duplicated(players_data))
cat("=== DUPLICATE ANALYSIS ===\n")
cat("Duplicate rows:", duplicates_count, "\n")
cat("Percentage:", round(duplicates_count / nrow(players_data) * 100, 2), "%\n\n")

# Remove duplicates
players_clean <- players_data %>% distinct()


# =====================================================
# SAFE NUMERIC CONVERSION
# =====================================================
  
  clean_numeric <- function(x) {
  x <- gsub("[^0-9.-]", "", x)  # keep digits, minus, dot
  suppressWarnings(as.numeric(x))
}

players_clean <- players_clean %>%
  mutate(across(
    .cols = where(~ is.character(.) && mean(grepl("[0-9]", .)) > 0.5),
    .fns = clean_numeric
  ))


cat("=== STRUCTURE AFTER CLEANING ===\n")
str(players_clean)


# =====================================================
# CORRELATION ANALYSIS
# =====================================================
numeric_data <- players_clean %>% select(where(is.numeric))

cat("\n=== Pearson Correlation Matrix ===\n")
print(cor(numeric_data, method = "pearson", use = "complete.obs"))

cat("\n=== Spearman Correlation Matrix ===\n")
print(cor(numeric_data, method = "spearman", use = "complete.obs"))

cat("\n=== Kendall Correlation Matrix ===\n")
print(cor(numeric_data, method = "kendall", use = "complete.obs"))


# =====================================================
# CORRELATION TEST: Assists vs Rating
# =====================================================
cat("\n=== CORRELATION TESTS (Assists vs Rating) ===\n")

if ("Rating" %in% names(players_clean) && "Assists" %in% names(players_clean)) {
  print(cor.test(players_clean$Rating, players_clean$Assists, method = "pearson"))
  print(cor.test(players_clean$Rating, players_clean$Assists, method = "spearman"))
  print(cor.test(players_clean$Rating, players_clean$Assists, method = "kendall"))
} else {
  cat("ERROR: Columns 'Rating' or 'Assists' not found in dataset.\n")
}


# =====================================================
# SCATTER PLOT: ASSISTS vs RATING
# =====================================================
scatter_plot <- ggplot(players_clean, aes(x = Assists, y = Rating)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatter Plot: Assists vs Player Rating",
    x = "Number of Assists",
    y = "Player Rating"
  ) +
  theme_minimal()

# Show in RStudio Plots panel
print(scatter_plot)

# Save plot as image
ggsave(
  filename = "scatter_plot_assists_vs_rating.png",
  plot = scatter_plot,
  width = 10,
  height = 7,
  dpi = 150
)

cat("\nPlot saved as: scatter_plot_assists_vs_rating.png\n")





