library(ggplot2)
library(dplyr)
library(readxl)

dataset<- read.csv("C:/Users/JNamita/OneDrive - Wageningen University & Research/climate-impacts-legumes/adaptation data subset.csv")

library(dplyr)

# Remove rows where all values are NA
dataset <- dataset %>%
  filter(if_any(everything(), ~ !is.na(.)))

# Alternatively, use complete.cases
dataset <- dataset[complete.cases(dataset), ]

# Confirm blank rows are gone
summary(dataset)

# Filter baseline yields (A1) where Adaptation type is 'No'
baseline_data <- dataset %>%
  filter(Adaptation.type == "No")

# Calculate mean baseline yield (A1)
A1 <- mean(baseline_data$Baseline.yield..t.ha.)

# Filter baseline yields (A2) where Adaptation type is 'No'
baseline_adapt_data <- dataset %>%
  filter(Adaptation.type != "No")

# Calculate mean baseline yield (A2)
A2 <- mean(baseline_adapt_data$Baseline.yield..t.ha.)

# Calculate B1: future yield without adaptation (where Adaptation type = 'No')
B1 <- mean(baseline_data$Projected.yield..t.ha.)

# Calculate B2: future yield with adaptation (all other adaptation types)
adaptation_data <- dataset %>%
  filter(Adaptation.type != "No")
B2 <- mean(adaptation_data$Projected.yield..t.ha.)



# Placeholder values for climate stressors
current_climate <- 1
future_climate <- 2

# Plot with arrows and annotations and additional space
ggplot(plot_data, aes(x = x, y = Yield, color = Group)) +
  geom_point(size = 4, shape = 16) +  # Fixed circle shape
  geom_line(aes(group = Group)) +
  
  # Add arrows from A1 -> B1 and A1 -> B2
  geom_segment(aes(x = 1, xend = 2, y = A1, yend = B1), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue", size = 1) +
  geom_segment(aes(x = 1, xend = 2, y = A1, yend = B2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1) +
  
  # Add labels
  geom_text(aes(label = round(Yield, 2)), vjust = -1, color = "black") +
  
  # Adjust axes with extra space below
  labs(x = "Climate Variable / Stressor", y = "Legume yield (t/ha)",
       title = "Estimating Impact / Adaptation") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Current Climate", "Future Climate")) +
  scale_y_continuous(limits = c(1, NA), expand = expansion(mult = c(0.2, 0.05))) +  # More space below
  theme_minimal()
