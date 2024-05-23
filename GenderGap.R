#install the data table package if you don't have it
library(data.table)
#loading the dataset
dt <- fread("lfsi_emp_a__custom_11491788_linear.csv")
#removing the columns that are not needed
dt  <- dt[ , -2] 
dt  <- dt[ , -2]
dt  <- dt[ , -1] 
dt  <- dt[ , -4]
dt  <- dt[ , -7] 
dt  <- dt[ , -1] 
head(dt)

gapDT <- dcast(dt, age + geo + TIME_PERIOD ~ sex, value.var = "OBS_VALUE")

# Calculate the difference and create a new column
gapDT[, gap := M - F]
gapDT <- gapDT[order(TIME_PERIOD)]
head(gapDT)

library(ggplot2)


ggplot(gapDT, aes(x = gap)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Gender Gap in Employment Rates",
       x = "Gender Gap (M - F)",
       y = "Frequency")
create_correlation_network <- function(data, time_period, year, threshold) {
  # Filter data for the given time period
  data_filtered <- data[TIME_PERIOD == time_period]
  data_filtered <- data[age == year]
  # Create a wide format data.table with countries as columns and age groups as rows
  data_wide <- dcast(data_filtered, age ~ geo, value.var = "gap")
  
  # Calculate the correlation matrix
  corr_matrix <- cor(data_wide[, -1, with = FALSE], use = "complete.obs")
  
  # Apply the threshold to determine connections
  adj_matrix <- ifelse(abs(corr_matrix) > threshold, corr_matrix, 0)
  
  # Create an igraph object
  g <- graph.adjacency(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  return(g)
}
library(igraph)
# Example: Create a correlation network for 2020 with a threshold of 0.7
network_2020 <- create_correlation_network(gapDT, 2020,"Y25-54", 0.1)

# Plot the network
plot(network_2020, edge.width = E(network_2020)$weight * 5, vertex.size = 10, vertex.label.cex = 0.8)

