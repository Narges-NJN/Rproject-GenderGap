################### Load necessary libraries ###############
library(data.table)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(readr)
############################################################

#loading the data set
#in this data set we have employment rate of female and male(percentage of the whole population) in 4 different age groups
#and 4 different years in 34 countries in Europe, we want to compute the gender gap in employment rate and analyze it in these different countries
dt <- fread("lfsi_emp_a__custom_11491788_linear.csv")
############################################################


#removing the columns that are not needed
dt  <- dt[ , -2] 
dt  <- dt[ , -2]
dt  <- dt[ , -1] 
dt  <- dt[ , -4]
dt  <- dt[ , -7] 
dt  <- dt[ , -1] 
head(dt)
#filtering out rows that we don't need
dt <- dt[geo != "EU27_2020"]
dt <- dt[geo != "EA20"]

#creating a new table in which we have the obs_value of both male and female related to a single country in a particular age group and time period in one row
gapDT <- dcast(dt, age + geo + TIME_PERIOD ~ sex, value.var = "OBS_VALUE")
#making sure that gapDT is a data table
gapDT = as.data.table(gapDT)
# Calculate the difference and create a new column as gap
gapDT[, gap := M - F]
#order by year because the following analysis would be easier for us
gapDT <- gapDT[order(TIME_PERIOD)]
head(gapDT)

# Calculate summary statistics
summary_stats <- gapDT %>%
  group_by(geo) %>%
  summarize(
    mean_gap = mean(gap, na.rm = TRUE),
    median_gap = median(gap, na.rm = TRUE),
    sd_gap = sd(gap, na.rm = TRUE),
    min_gap = min(gap, na.rm = TRUE),
    max_gap = max(gap, na.rm = TRUE)
  )

summary_stats


# Step 2: Filter for European Union Countries
# Assuming EU countries based on a predefined list
eu_countries <- unique(gapDT$geo)

# Step 3: Aggregate the Gap Values by Region
# For simplicity, we'll treat each country as a region
gap_data <- gapDT %>%
  summarize(gap = mean(gap, na.rm = TRUE)) %>%
  rename(region = geo)

# Step 4: Compare the Gender Gap
# Statistical Analysis
gap_summary <- gap_data %>%
  summarize(mean_gap = mean(average_gap), sd_gap = sd(average_gap))

# Print summary statistics
print(gap_summary)

# Visualization
ggplot(gap_data, aes(x = reorder(region, average_gap), y = average_gap)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Employment Gender Gap by EU Country",
       x = "Country",
       y = "Average Employment Gap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# here we can see that the overall gap is decreasing in countries 
avg_data_by_age <- gapDT %>%
  group_by(age, TIME_PERIOD) %>%
  summarize(Average_Gap = mean(gap, na.rm = TRUE))

ggplot(avg_data_by_age, aes(x = TIME_PERIOD, y = Average_Gap, color = age, group = age)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Gender Gap Over Years by Age Group",
       x = "Year",
       y = "Average Gap",
       color = "Age Group") +
  theme_minimal()


# let's see how the gap changes over years for all countries
data_filtered <- gapDT[age == "Y20-64" & TIME_PERIOD %in% c(2014, 2017, 2020, 2023)]

# Plot the data using ggplot2
ggplot(data_filtered, aes(x = TIME_PERIOD, y = gap, color = geo)) +
  geom_point(size = 3) + # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "dashed", size = 1) + # Regression lines
  theme_minimal() + # Clean theme
  labs(
    title = "Gender Gap Over Time by Country - age group = 20-64",
    x = "Time Period",
    y = "Gap",
    color = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# now let's see which country has the most increasing trend and which has the most decreasiing trend
data_filtered <- gapDT[age == "Y20-64" & TIME_PERIOD %in% c(2014, 2017, 2020, 2023)]

# Compute the slopes for each country
slopes <- data_filtered[, .(slope = coef(lm(gap ~ TIME_PERIOD))[2]), by = geo]

# Identify the countries with the maximum and minimum slopes
max_slope_country <- slopes[which.max(slope)]$geo
min_slope_country <- slopes[which.min(slope)]$geo

# Filter the data to include only these countries
data_filtered <- data_filtered[geo %in% c(max_slope_country, min_slope_country)]

# Plot the data using ggplot2
ggplot(data_filtered, aes(x = TIME_PERIOD, y = gap, color = geo)) +
  geom_point(size = 3) + # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "dashed", size = 1) + # Regression lines
  theme_minimal() + # Clean theme
  labs(
    title = "Gender Gap Over Time by Country",
    x = "Time Period",
    y = "Gap",
    color = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### now will have two plots, one for increasing trends and one for decreasing ones
data_filtered <- gapDT[age == "Y20-64" & TIME_PERIOD %in% c(2014, 2017, 2020, 2023)]

# Compute the slopes for each country
slopes <- data_filtered[, .(slope = coef(lm(gap ~ TIME_PERIOD))[2]), by = geo]

# Split countries into increasing and decreasing groups
increasing_countries <- slopes[slope > 0]$geo
decreasing_countries <- slopes[slope < 0]$geo

# Filter data for these groups
data_increasing <- data_filtered[geo %in% increasing_countries]
data_decreasing <- data_filtered[geo %in% decreasing_countries]

# Plot the increasing trends
p1 <- ggplot(data_increasing, aes(x = TIME_PERIOD, y = gap, color = geo)) +
  geom_point(size = 3) + # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "dashed", size = 1) + # Regression lines
  theme_minimal() + # Clean theme
  labs(
    title = "Increasing Gender Gap Trends Over Time by Country",
    x = "Time Period",
    y = "Gap",
    color = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Plot the decreasing trends
p2 <- ggplot(data_decreasing, aes(x = TIME_PERIOD, y = gap, color = geo)) +
  geom_point(size = 3) + # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "dashed", size = 1) + # Regression lines
  theme_minimal() + # Clean theme
  labs(
    title = "Decreasing Gender Gap Trends Over Time by Country",
    x = "Time Period",
    y = "Gap",
    color = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Print the plots
print(p1)
print(p2)


#### violin
gapDT_filtered <- gapDT[age != "Y20-64"]
ggplot(gapDT_filtered, aes(x = age, y = gap, fill = age)) +
  geom_violin() +
  labs(title = "Violin Plot of Gender Gap by Age Group",
       x = "Age Group",
       y = "Gender Gap (M - F)") +
  theme_minimal()


library(tidyverse)
library(readr)


# Define the geo-political regions
geo_region_mapping <- c(
  "AT" = "Western Europe", "BE" = "Western Europe", "CH" = "Western Europe",
  "DE" = "Western Europe", "FR" = "Western Europe", "LU" = "Western Europe",
  "NL" = "Western Europe", "CY" = "Southern Europe", "EL" = "Southern Europe",
  "ES" = "Southern Europe", "IT" = "Southern Europe", "MT" = "Southern Europe",
  "PT" = "Southern Europe", "BG" = "Eastern Europe", "CZ" = "Eastern Europe",
  "EE" = "Eastern Europe", "HR" = "Eastern Europe", "HU" = "Eastern Europe",
  "LT" = "Eastern Europe", "LV" = "Eastern Europe", "PL" = "Eastern Europe",
  "RO" = "Eastern Europe", "SI" = "Eastern Europe", "SK" = "Eastern Europe",
  "IS" = "Northern Europe", "NO" = "Northern Europe", "DK" = "Northern Europe",
  "FI" = "Northern Europe", "SE" = "Northern Europe", "IE" = "Northern Europe",
  "MK" = "Eastern Europe", "RS" = "Eastern Europe", "ME" = "Eastern Europe",
  "BA" = "Eastern Europe", "TR" = "Eastern Europe"
)

# Map countries to their respective regions
gapDT$region <- geo_region_mapping[gapDT$geo]

# Calculate the average employment gap for each region
region_gap <- gapDT %>%
  group_by(region) %>%
  summarize(average_gap = mean(gap, na.rm = TRUE))

# Print the average employment gap for each region
print(region_gap)

# Plot the average employment gap for each region
ggplot(region_gap, aes(x = reorder(region, average_gap), y = average_gap)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Average Employment Gap by Geo-Political Region",
       x = "Region",
       y = "Average Employment Gap") +
  theme_minimal()


library(tidyverse)
library(igraph)
library(ggraph)

# Function to create a correlation network
create_correlation_network <- function(data, time_period, age_group, threshold) {
  data$geo <- as.factor(data$geo)
  
  filtered <- data %>% filter(age == age_group, TIME_PERIOD == time_period)
  data_numeric <- filtered %>%
    group_by(geo) %>%
    summarize(avg_gap = mean(gap, na.rm = TRUE))
  
  adj_matrix <- as.matrix(dist(data_numeric$avg_gap))
  rownames(adj_matrix) <- data_numeric$geo
  colnames(adj_matrix) <- data_numeric$geo
  
  adj_matrix <- 1 / (1 + adj_matrix)
  adj_matrix[adj_matrix < threshold] <- 0
  diag(adj_matrix) <- 0
  
  graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)
  
  return(graph)
}

# Function to create community partitions and visualize
create_community_partition <- function(graph) {
  louvain_communities <- cluster_louvain(graph)
  V(graph)$community <- louvain_communities$membership
  layout <- layout_with_fr(graph)
  
  ggraph(graph, layout = layout) +
    geom_edge_link(aes(width = weight), alpha = 0.7, color = "gray") +
    geom_node_point(aes(color = as.factor(community)), size = 8) +
    geom_node_text(aes(label = name), repel = TRUE, size = 4, color = "black") +
    scale_color_brewer(palette = "Set3") +
    theme_void() +
    labs(title = "Enhanced Network Visualization with Community Detection",
         color = "Community")
}

# Function to separate network into clusters and remove inter-cluster edges
separate_clusters <- function(graph) {
  clusters <- cluster_louvain(graph)
  V(graph)$cluster <- clusters$membership
  
  # Identify edges that connect different clusters
  edges_to_remove <- E(graph)[sapply(E(graph), function(e) {
    V(graph)[ends(graph, e)[1]]$cluster != V(graph)[ends(graph, e)[2]]$cluster
  })]
  
  # Create a new graph with edges between clusters removed
  graph_no_inter <- delete_edges(graph, edges_to_remove)
  
  subgraphs <- lapply(unique(V(graph)$cluster), function(cluster_id) {
    induced_subgraph(graph, V(graph)[cluster == cluster_id])
  })
  
  return(list(
    graph_no_inter = graph_no_inter,
    subgraphs = subgraphs
  ))
}

# Create the networks for each time period
network_2014 <- create_correlation_network(gapDT, 2014, "Y25-54", 0.4)
network_2017 <- create_correlation_network(gapDT, 2017, "Y25-54", 0.4)
network_2020 <- create_correlation_network(gapDT, 2020, "Y25-54", 0.4)
network_2023 <- create_correlation_network(gapDT, 2023, "Y25-54", 0.4)


# Plot original networks with clusters
create_community_partition(network_2014)
create_community_partition(network_2017)
create_community_partition(network_2020)
create_community_partition(network_2023)

# Separate networks into clusters and remove inter-cluster edges
separated_2014 <- separate_clusters(network_2014)
separated_2017 <- separate_clusters(network_2017)
separated_2020 <- separate_clusters(network_2020)
separated_2023 <- separate_clusters(network_2023)

# Plot the separated networks
plot(separated_2014$graph_no_inter, edge.width = E(separated_2014$graph_no_inter)$weight * 5, vertex.size = 10, vertex.label.cex = 0.8, main = "2014 Network without Inter-Cluster Edges")
plot(separated_2017$graph_no_inter, edge.width = E(separated_2017$graph_no_inter)$weight * 5, vertex.size = 10, vertex.label.cex = 0.8, main = "2017 Network without Inter-Cluster Edges")
plot(separated_2020$graph_no_inter, edge.width = E(separated_2020$graph_no_inter)$weight * 5, vertex.size = 10, vertex.label.cex = 0.8, main = "2020 Network without Inter-Cluster Edges")
plot(separated_2023$graph_no_inter, edge.width = E(separated_2023$graph_no_inter)$weight * 5, vertex.size = 10, vertex.label.cex = 0.8, main = "2023 Network without Inter-Cluster Edges")

write.csv2(gapDT,"F:/Sapienza Courses/4/DMA/Rproject-GenderGap/final.csv")
