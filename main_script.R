# Load necessary libraries
library(tidyverse)
library(corrplot)

# Read the dataset
players_data <- read.csv("Players_Score.csv")

# Checking for duplicates
duplicates_count <- sum(duplicated(players_data))
if (duplicates_count > 0) {
  players_data <- players_data %>% distinct()
}

# Rename columns to remove extra spaces (if any)
names(players_data) <- trimws(names(players_data))

# Convert Assists and Rating to numeric if necessary
players_data$Assists <- as.numeric(players_data$Assists)
players_data$Rating <- as.numeric(players_data$Rating)

# Create a combined dataset for histogram
# Use correct mapping for names
hist_data <- players_data %>%
  select(Assists, Rating) %>%
  pivot_longer(cols = c(Assists, Rating), names_to = "Variable", values_to = "Value")

# Plotting the histogram
ggplot(hist_data, aes(x = Value, fill = Variable)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram distribution of players assist and rating in world cup 2018",
       x = "Assists",
       y = "Rating") +
  scale_fill_manual(values = c("blue", "orange"),
                    name = "Variable",
                    labels = c("Assists", "Rating")) +
  theme_minimal()