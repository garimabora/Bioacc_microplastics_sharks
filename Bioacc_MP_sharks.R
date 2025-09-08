data<-read.csv("/Users/garimabora/Desktop/Papers/Monali/Monali_MP/Data.csv")

# Load necessary libraries
library(MASS) 
install.packages("MASS")# for negative binomial GLM

# Assuming you have a dataset 'data' with the following columns:
# microplastic_concentration - the dependent variable (microplastic concentration)
# location - location of capture (e.g., 'Malvan', 'Kochi')
# TL - trophic level (e.g., 'TL1', 'TL2')

# Fit the negative binomial GLM model
model <- glm.nb(MP_pergram ~ Location + TL, data = data)

# Display the model summary
summary(model)

# Load necessary libraries
library(MASS)  # for negative binomial GLM

# Assuming you have a dataset 'data' with the following columns:
# microplastic_concentration - the dependent variable (microplastic concentration)
# location - location of capture (e.g., 'Malvan', 'Kochi')
# TL - trophic level (e.g., 'TL1', 'TL2')

# Filter the data for Malvan and Kochi separately

# Malvan data
data_malvan <- subset(data, Location == "Malvan")

# Fit the negative binomial GLM for Malvan
model_malvan <- glm.nb(MP_pergram ~ TL, data = data_malvan)
summary(model_malvan)

# Kochi data
data_kochi <- subset(data, Location == "Kochi")

# Fit the negative binomial GLM for Kochi
model_kochi <- glm.nb(MP_pergram ~ TL, data = data_kochi)
summary(model_kochi)


####PLOTS####
library(ggplot2)
ggplot(data, aes(x = Location, y = MP_pergram, fill = Location)) +
  geom_boxplot(width = 0.5) + # Adjust box width for thinner boxes
  facet_wrap(~Tissue, scales = "free_y") + # Side-by-side panels for TL1 and TL2
  scale_fill_manual(values = c("Malvan" = "black", "Kochi" = "grey")) +
  labs(
    title = "",
    x = "Location",
    y = "Microplastic Concentration (MP/gram)"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add border to panels
    strip.background = element_rect(color = "black", fill = "lightgrey"), # Border for facet labels
    strip.text = element_text(face = "bold", size = 14, color = "black"), # Black font for facet label text
    axis.title = element_text(size = 16, color = "black"), # Black font for axis titles
    axis.text.x = element_blank(), # Remove x-axis text (location labels)
    axis.text.y = element_text(size = 14, color = "black"), # Black font for y-axis text
    legend.text = element_text(size = 14, color = "black") # Black font for legend text
  )

library(dplyr)
# Filter the data to exclude rows where Tissue is "Whole Body"
filtered_data <- data %>% 
  filter(Tissue != "Whole body")

# Create the plot using the filtered data

ggplot(filtered_data, aes(x = TL, y = MP_pergram, fill = TL)) +
  geom_boxplot(width = 0.5, color = "black") +
  facet_wrap(~ Location, scales = "free_y") +
  scale_fill_manual(values = c("TL1" = "grey30", "TL2" = "grey")) +
  labs(
    x = "Location",  # You can leave this if you want a generic label or remove it entirely
    y = "Microplastic Concentration (MP/gram)"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(color = "black", fill = "lightgrey"),
    strip.text = element_text(face = "bold", size = 14, color = "black"),
    axis.text.x = element_blank(),              # remove Gut/Liver labels
    axis.ticks.x = element_blank(),             # remove x-axis ticks
    axis.title.y = element_text(size = 14, color = "black"),
    axis.title.x  = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 13, color = "black")
  )

library(tidyr)

data_long <- data %>%
  pivot_longer(cols = c(Fibers, Fragments, Films), 
               names_to = "Type", 
               values_to = "Count")

# Remove rows where Count is zero
data_long_filtered <- data_long %>%
  filter(Count > 0)

# Create the boxplot with separate facets, independent y axes, and a legend for trophic levels
ggplot(data_long_filtered, aes(x = Type, y = Count, fill = TL)) +
  geom_boxplot(position = position_dodge(width = 1.2), width = 0.6) +  # Increase space between boxes by adjusting width
  scale_fill_manual(values = c("TL1" = "black", "TL2" = "grey")) +  # Manual color scale for TL1 and TL2
  labs(title = "",
       x = "Microplastic Type", y = "Number of Microplastics") +
  theme_minimal() +
  facet_wrap(~Type, scales = "free_y", strip.position = "bottom") +  # Move facet labels to the bottom
  scale_x_discrete(expand = c(0.2, 0.2)) +  # Center the boxes in each facet
  theme(legend.title = element_blank(),
        legend.position = "top",  # Move the legend to the top
        axis.text.x = element_blank(),  # Remove x-axis labels of the types
        axis.title = element_text(size = 10),  # Increase axis title font size
        axis.text = element_text(size = 10),   # Increase axis tick font size
        strip.text = element_text(size = 10),  # Increase facet label font size
        plot.title = element_text(size = 10))  # Increase plot title font size

# Load necessary library
install.packages("MASS")  # Run if you haven't installed MASS
library(MASS)

# Load necessary library
library(MASS)

# Subset the data for Kochi and Malvan
data_kochi <- subset(data, Location == "Kochi")  # Adjust column name if needed
data_malvan <- subset(data, Location == "Malvan")  # Adjust column name if needed

# Fit Negative Binomial GLM for Kochi
model_kochi <- glm.nb(MP_pergram ~ TL * Tissue * Habitat, data = data_kochi)

# View the summary of the Kochi model
summary(model_kochi)

# Fit Negative Binomial GLM for Malvan
model_malvan <- glm.nb(MP_pergram ~ TL * Tissue * Habitat, data = data_malvan)

# View the summary of the Malvan model
summary(model_malvan)

# Compare AIC values for both models
aic_kochi <- AIC(model_kochi)
aic_malvan <- AIC(model_malvan)

# Print AIC values
cat("AIC for Kochi model: ", aic_kochi, "\n")
cat("AIC for Malvan model: ", aic_malvan, "\n")

# Perform an analysis of deviance to assess the contribution of predictors
anova(model, test = "Chisq")

# Fit Poisson GLM
poisson_model <- glm(MP_pergram ~ TL * Tissue * Habitat, family = poisson, data = data)

# Print the summary of the Poisson model
summary(poisson_model)

# Fit Negative Binomial GLM
library(MASS)  # Ensure the MASS package is loaded
nb_model <- glm.nb(MP_pergram ~ TL * Tissue * Habitat, data = data)

# Print the summary of the Negative Binomial model
summary(nb_model)
# Compare AIC values
aic_poisson <- AIC(poisson_model)
aic_nb <- AIC(nb_model)

# Display AIC values
cat("AIC for Poisson model: ", aic_poisson, "\n")
cat("AIC for Negative Binomial model: ", aic_nb, "\n")

#code to plot the benthic vs pelagic comparison wrt type of microplastics
library(tidyr)
library(ggplot2)
library(dplyr)



###sharayu's data################
# Load and pivot
sharayu <- read.csv("/Users/garimabora/Desktop/Papers/Monali/sharayu.csv")

df_long <- sharayu %>%
  pivot_longer(cols = -Type, names_to = "Microplastic_Type", values_to = "Count")

# Plot
ggplot(df_long, aes(x = Type, y = Count, fill = Type)) +
  geom_boxplot(color = "black", width = 0.2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("Benthic" = "grey20", "Pelagic" = "grey70")) +
  facet_wrap(~ Microplastic_Type, scales = "free_y", nrow = 1, strip.position = "bottom") +
  labs(
    title = "",
    x = "Microplastic type",
    y = "Number of Microplastics",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9),  
    strip.background = element_blank(),
    strip.text = element_text(),  # keeps "Fibres", "Films", etc. as facet titles
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_blank(),         # removes x-axis tick labels
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(size = 12),  # reduced x-axis title size
    axis.title.y = element_text(size = 12) 
  )

