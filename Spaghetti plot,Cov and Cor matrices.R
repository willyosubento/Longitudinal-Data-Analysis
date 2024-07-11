#packages
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(knitr)
#importing data
Rats_data<-read.table("C:\\Users\\user\\Desktop\\rats.csv")
View(Rats_data)
#Rename data
Rats_dataset<-Rats_data %>%
  rename(Rat_ID_Number="V1",Weeks="V2",Weight_g="V3",Group_indicator="V4") %>%
  mutate(Group_indicator=as.factor(Group_indicator),Group_Indicator=recode(Group_indicator,"Treatment_group1"="1",
                                                                           "Treatment_group2"="2","Treatment_group"="3",
                                                                           "Treatment_group"="4")) %>%
  view()
names(Rats_dataset)
#Spaghetti plots
spaghetti_plot <- ggplot(Rats_dataset, aes(x = Weeks, y = Weight_g,group = Rat_ID_Number, color = as.factor(Group_Indicator))) +
  geom_line() +
  labs(title = "Longitudinal Profile plot for each Treatment Group",
       x = "Weeks",
       y = "Weight in g") +
  scale_color_discrete(name = "Group") +
  facet_wrap(~Group_Indicator) +
  theme_minimal()

print(spaghetti_plot)

#calculate the mean for each treatment group
Rats_mean_dataset <- Rats_dataset %>%
  group_by(Group_indicator, Weeks) %>%
  summarise(mean_weight = mean(Weight_g))%>%
  view()
kable(Rats_mean_dataset, caption = "Mean Weight for each Treatment Group at each Time Point")
#plot for each Treatment group
means_plot <- ggplot(Rats_mean_dataset, aes(x = Weeks, y = mean_weight, group = Group_indicator, color = as.factor(Group_indicator))) +
  geom_line() +
  labs(title = "Mean Weight for each Treatment Group over Time",
       x = "Weeks",
       y = "Mean Weight in g") +
  scale_color_discrete(name = "Group") +
  theme_minimal()

print(means_plot)
# Pivot the data to have each rat's weight as a separate column
pivoted_data <- Rats_dataset %>%
  pivot_wider(names_from = Weeks, values_from = Weight_g) %>%
  view()

# Split the pivoted data by group
grouped_data <- split(pivoted_data, pivoted_data$Group_indicator)
view(grouped_data)
# Calculate sample covariance matrices and estimated correlation matrices for each group
covariance_matrices <- lapply(grouped_data, function(group) {
  # Select only weight columns for covariance calculation
  weight_columns <- select(group, matches("^\\d+$"))
  # Calculate covariance matrix
  cov_matrix <- cov(weight_columns, use = "complete.obs")
  # Calculate correlation matrix based on covariance matrix
  cor_matrix <- cov2cor(cov_matrix)
  # Return both covariance and correlation matrices
  list(covariance_matrix = cov_matrix, correlation_matrix = cor_matrix)
})
view(covariance_matrices)
# Print covariance matrices for each group
for (i in 1:length(covariance_matrices)) {
  group_num <- names(covariance_matrices)[i]
  cat("Group", group_num, ":\n")
  cat("Covariance Matrix:\n")
  print(covariance_matrices[[i]]$covariance_matrix)
  cat("Correlation Matrix:\n")
  print(covariance_matrices[[i]]$correlation_matrix)
  cat("\n")
}

