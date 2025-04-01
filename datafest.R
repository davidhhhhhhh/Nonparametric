# Library
library(dplyr)
library(ggplot2)
library(lme4)

# DataFest Visualizations -----
a_fil = read.csv("a_fil.csv")

library(dplyr)
library(ggplot2)

# Step 1: Group and summarize
leasedSF_summary <- a_fil %>%
  group_by(pre.post) %>%
  summarise(mean_leasedSF = mean(leasedSF, na.rm = TRUE))

# Set the factor order so that "pre" comes before "post"
leasedSF_summary$pre.post <- factor(leasedSF_summary$pre.post, levels = c("pre", "post"))

# Step 2: Create the plot with transparent backgrounds
p <- ggplot(leasedSF_summary, aes(x = pre.post, y = mean_leasedSF, fill = pre.post)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(mean_leasedSF, 0)), vjust = -0.5, size = 5) +
  labs(title = "Monthly Leased Area (square feet)",
       x = "Period",
       y = "Mean Leased SF",
       fill = "Pandemic Phase") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

# Step 3: Save the plot with a transparent background using ggsave
ggsave("leased_area_plot.png", plot = p, bg = "transparent", width = 8, height = 6, dpi = 300)

#Cluster Plot-----------
cluster = read.csv("cost_of_living_selected_cities.csv")

# Define your city groups
group1 <- c("Washington, DC, United States", "Chicago, IL, United States",
            "San Francisco, CA, United States", "New York, NY, United States",
            "Los Angeles, CA, United States")

group2 <- c("Austin, TX, United States", "Dallas, TX, United States",
            "Houston, TX, United States", "Philadelphia, PA, United States")

# Mean Stats
cluster_1 <- cluster[cluster$City %in% group1, ]
cluster_2 <- cluster[cluster$City %in% group2, ]
wilcox.test(cluster_1$Rent.Index, cluster_2$Rent.Index)
(mean(cluster_2$Rent.Index) - mean(cluster_1$Rent.Index))/mean(cluster_1$Rent.Index)
wilcox.test(cluster_1$Restaurant.Price.Index, cluster_2$Restaurant.Price.Index)
(mean(cluster_2$Restaurant.Price.Index)-mean(cluster_1$Restaurant.Price.Index)) / mean(cluster_1$Restaurant.Price.Index)

# Assign groups
cluster <- cluster %>%
  mutate(Group = case_when(
    City %in% group1 ~ "CBD-Centric",
    City %in% group2 ~ "Decentralized",
    TRUE ~ "Other"
  ))

# Extract short city name for labels (optional)
cluster <- cluster %>%
  mutate(Label = sub(",.*", "", City))

# Plot with Rent Index on x-axis and Restaurant Price Index on y-axis
ggplot(cluster, aes(x = Rent.Index, y = Restaurant.Price.Index, color = Group)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = Label,
        # If it's New York, shift the label inside; otherwise, use default
        hjust = ifelse(Label == "New York", 1.2, -0.1),
        vjust = ifelse(Label == "New York", 1.2, -0.3)
    ),
    size = 3.5
  ) +
  labs(
    title = "Rent Index vs. Restaurant Price Index",
    x = "Rent Index",
    y = "Restaurant Price Index",
    color = "CityType"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
# Regression Model----------
b = read.csv("leased_area_summary.csv")

# Example: Set 'Finance' as the reference level for internal_industry
b$internal_industry <- as.factor(b$internal_industry)
b$internal_industry <- relevel(b$internal_industry, ref = "Legal")
b$internal_class = as.factor(b$internal_class)
b$internal_class <- relevel(b$internal_class, ref = "A")
b$big_city <- as.factor(b$CityType)
b$Pandemic <- as.factor(b$Pandemic)
b$Go_Stay <- as.factor(b$Go_Stay)
b$Pandemic <- relevel(b$Pandemic, ref = "Pre")
b$Go_Stay <- relevel(b$Go_Stay, ref = "Stay")

# Fit linear model
base_model <- lm(leasedSF ~internal_industry + 
              internal_class + CityType + Pandemic + Go_Stay + 
              avg_occupancy_proportion,
            data = b)

# Print model summary
summary(base_model)

# Interaction
interact_city_go <- lmer(leasedSF ~internal_industry + 
                          internal_class + CityType + Pandemic + Go_Stay + 
                          avg_occupancy_proportion + 
                         CityType:Pandemic:Go_Stay+Pandemic:internal_industry:internal_class+
                         (1|market),
                        data = b)
summary(interact_city_go)
temp = step(interaction_model)
summary(temp)

test <- lm(leasedSF ~ Pandemic *  internal_industry + CityType +
                          internal_class  + Go_Stay,
                        data = b)
summary(test)
