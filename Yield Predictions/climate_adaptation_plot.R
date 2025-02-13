library(ggplot2)
library(dplyr)
library(readxl)

# Simulated dataset structure as per the provided image
# Load the data (replace 'your_data.xlsx' with actual file path)
data <- read_excel('Yield Predictions/yield_predictions_data.xlsx')

# Filter baseline yields (A1) where Adaptation type is 'No'
baseline_data <- data %>%
  filter(Adaptation.type == "No")

# Calculate mean baseline yield (A1)
A1 <- mean(baseline_data$Baseline.yield..t.ha.)

# Calculate B1: future yield without adaptation (where Adaptation type = 'No')
B1 <- mean(baseline_data$Projected.yield..t.ha.)

# Calculate B2: future yield with adaptation (all other adaptation types)
adaptation_data <- data %>%
  filter(Adaptation.type != "No")
B2 <- mean(adaptation_data$Projected.yield..t.ha.)

# Calculate B3: future yield with adaptation & potential tech improvements (max yield)
B3 <- max(adaptation_data$Projected.yield..t.ha.)

# Placeholder values for climate stressors
current_climate <- 1
future_climate <- 2

# Data frame for plotting
plot_data <- data.frame(
  Climate = c("Current", "Future", "Future", "Future"),
  Yield = c(A1, B1, B2, B3),
  Group = c("Baseline", "No Adaptation", "Adaptation", "Tech + Adaptation"),
  x = c(current_climate, future_climate, future_climate, future_climate)
)

# Plot similar to the provided schematic
ggplot(plot_data, aes(x = x, y = Yield, color = Group)) +
  geom_point(size = 4) +
  geom_line(aes(group = Group)) +
  labs(x = "Climate Variable / Stressor", y = "Agricultural Outcome (e.g., yield)",
       title = "Estimating Impact / Adaptation") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Current Climate", "Future Climate")) +
  theme_minimal() +
  geom_text(aes(label = round(Yield, 2)), vjust = -1)

# Add annotations for effects
annotate("text", x = 1.5, y = A1 + (B2 - B1) / 2, label = "Impact (B2 - A1)", angle = 90) +
  annotate("text", x = 2, y = B2 + (B3 - B2) / 2, label = "Adaptation (B3 - B2)", angle = 90)
