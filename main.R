# install.packages("ggplot2")
library(readr)
library(ggplot2)
# Data collection/sampling +
# Freq distr +
# some charts/maybe +
# Data descriptioın ->
# measures of central tendancy -> mean, median, mode, midrange, w mean
# PC of skewness
# measures of variation -> range, variance, chebshev's theorrem, emprical rule (normal)
# measures of position -> z score, percentiles, quartiles, outliers
# EDA, IQR, boxplot
# normalitiy check (hist, PC skew, outliers, normal quentile plot, chi square goodness of fit, kolmogov-smirnov test, lilliefors test)
# conf intervals, t dist, chi dist
# hypothesis testing -> traditional,  z test for a mean, t-test for a mean, p-value method, chi square for variance or a std, power of a test, type12 err
# hyp test 2 -> diff between 2 means proportions vatiances...

parse_stock_name <- function(cp){
  parsed_string <- strsplit(cp, "/")[[1]][length(strsplit(cp, "/")[[1]])]
  parsed_string <- stringr::str_remove(parsed_string, "\\.csv")
  return (parsed_string)
}
add_mean <- function(data, mean_list, stock_name){
    data$Adjusted.Close <- as.numeric(data$Adjusted.Close)
    missing_values <- sum(is.na(data$Adjusted.Close))
  
    # Check the number of non-missing values
    non_missing_values <- sum(!is.na(data$Adjusted.Close))
  
  if (non_missing_values < 2) {
    print(paste("Insufficient data points to create density plot in stock:", stock_name))
  } 
  else {
    # Plot the density with a fixed bandwidth value
    mean_stock <- mean(data$Adjusted.Close)
    mean_list[[stock_name]] <- mean_stock
    return(mean_list)
  }
}
create_dataframe <- function(mean_list){
  mean_dataframe <- data.frame(
    Stock = names(mean_list),
    Mean_Value = unlist(mean_list)
  )
  return (mean_dataframe)
}
get_title <- function(year, before=FALSE, after=FALSE){
  if (before == TRUE){
    title <- paste("Stock Mean Values 3 Months Before the",year,"Elections")}
  else if (after == TRUE){
    title <- paste("Stock Mean Values 3 Months After the",year,"Elections")}
  else{
    title <- paste("Stock Mean Values in 3 Months Range of the",year,"Elections")}
  return (title)
}

get_boxplot <- function(mean_data, year, before=FALSE, after=FALSE){
  title <- get_title(year, before, after)
  ggplot(mean_data, aes(x = "", y = Mean_Value)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5) +
  labs(title = paste("Boxplot of", title), y = "Mean Value") +
  theme_minimal()
}

plot_ditribution <- function(mean_data, year, before=FALSE,  after=FALSE){
  title <- get_title(year, before, after)
  ggplot(mean_data, aes(x = Mean_Value)) +
    geom_histogram(binwidth = NUM_BREAKS(mean_data), color="black", fill = "steelblue", linewidth=0.5) +
    labs(title = paste("Histogram Plot of", title), x = "Mean Value", y = "Count") +
    geom_vline(aes(xintercept=mean(Mean_Value)),
               color="red", linetype="dashed", linewidth=1)
}
plot_density <- function(mean_data, year, before=FALSE, after=FALSE){
  title <- get_title(year, before, after)
  ggplot(mean_data, aes(x= Mean_Value)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+
  geom_density(alpha=.2, fill="green")+
  labs(title = paste("Density Plot of", title), x = "Mean Value", y = "Density")
}

add_scatter_plot <- function(mean_data, year, before=FALSE, after=FALSE){
  title <- get_title(year, before, after)
  ggplot(mean_data, aes(x = Stock, y = Mean_Value)) +
  geom_point() +
  labs(title = title, x = "Stock Names", y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

identify_outliers <- function(dataframe, column_name) {
  # Calculate the IQR
  q1 <- quantile(dataframe[[column_name]], 0.25)
  q3 <- quantile(dataframe[[column_name]], 0.75)
  iqr <- q3 - q1
  
  # Determine the lower and upper bounds for outliers
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Identify outliers
  outliers <- dataframe[dataframe[[column_name]] < lower_bound | dataframe[[column_name]] > upper_bound, ]
  
  # Return outliers
  return(outliers)
}

# Usage example
outliers_df <- identify_outliers(mean_dataframe_2020, "Mean_Value")


NUM_BREAKS <- function(data){
num_data_points <- length(data)
num_breaks <- ceiling(log2(num_data_points)) + 1
print(num_breaks)
return(num_breaks)
}

files <- list.files(path="oil_subset_elections_stock_market/elections_stock_market", pattern="*.csv", full.names=TRUE, recursive=FALSE)
mean_list_2016 <- c()
mean_list_2020 <- c()
mean_list_2020_before <- c()
mean_list_2016_before <- c()
mean_list_2020_after <- c()
mean_list_2016_after <- c()

division_date_2016 <- as.Date("2016-11-08")
division_date_2020 <- as.Date("2020-11-02")

for (pth in files){
  data <- read.csv(pth)
  stock_name <- parse_stock_name(pth)
  data$Year <- format(as.Date(data$Date), "%Y")
  # Split the data into two based on the date ranges
  data_2016_2017 <- data[data$Year %in% c("2016", "2017"), ]
  data_2020_2021 <- data[data$Year %in% c("2020", "2021"), ]
  if (nrow(data_2016_2017!=0)){
    mean_list_2016 <- add_mean(data_2016_2017, mean_list_2016, stock_name)
    
    before_subset_2016 <- data_2016_2017[data_2016_2017$Date < division_date_2016, ]
    after_subset_2016 <- data_2016_2017[data_2016_2017$Date >= division_date_2016, ]
    
    mean_list_2016_after <- add_mean(after_subset_2016, mean_list_2016_after, stock_name)
    
    mean_list_2016_before <- add_mean(before_subset_2016, mean_list_2016_before, stock_name)
    }
  if(nrow(data_2020_2021)!=0){
    if(stock_name != "CRC"){ # CRC causes an issiue
    mean_list_2020 <- add_mean(data_2020_2021, mean_list_2020, stock_name)

    before_subset_2020 <- data_2020_2021[data_2020_2021$Date < division_date_2020, ]
    after_subset_2020 <- data_2020_2021[data_2020_2021$Date >= division_date_2020, ]
    
    mean_list_2020_after <- add_mean(after_subset_2020, mean_list_2020_after, stock_name)
    
    mean_list_2020_before <- add_mean(before_subset_2020, mean_list_2020_before, stock_name)
  }}
  
}

# Scatter plot for 2020
mean_dataframe_2020 <- create_dataframe(mean_list_2020)
mean(mean_dataframe_2020$Mean_Value)
mean(mean_dataframe_2016$Mean_Value)
mean(mean_dataframe_2016_after$Mean_Value)
mean(mean_dataframe_2016_before$Mean_Value)
mean(mean_dataframe_2020_after$Mean_Value)
mean(mean_dataframe_2020_before$Mean_Value)
outliers2020<- identify_outliers(mean_dataframe_2020, "Mean_Value")
outliers2020
outliers2016 <- identify_outliers(mean_dataframe_2016, "Mean_Value")
outliers2016
outliers2016_before <- identify_outliers(mean_dataframe_2016_before, "Mean_Value")
outliers2016_after <- identify_outliers(mean_dataframe_2016_after, "Mean_Value")
outliers2020_after <- identify_outliers(mean_dataframe_2020_after, "Mean_Value")
outliers2020_before <- identify_outliers(mean_dataframe_2020_before, "Mean_Value")


mean_dataframe_2016_no_outliers <- mean_dataframe_2016[!(mean_dataframe_2016$Mean_Value %in% outliers2016$Mean_Value), ]
mean_dataframe_2020_no_outliers <- mean_dataframe_2020[!(mean_dataframe_2020$Mean_Value %in% outliers2020$Mean_Value), ]
mean_dataframe_2016_no_outliers_before <- mean_dataframe_2016_before[!(mean_dataframe_2016_before$Mean_Value %in% outliers2016_before$Mean_Value), ]
mean_dataframe_2016_no_outliers_after <- mean_dataframe_2016_after[!(mean_dataframe_2016_after$Mean_Value %in% outliers2016_after$Mean_Value), ]
mean_dataframe_2020_no_outliers_before <- mean_dataframe_2020_before[!(mean_dataframe_2020_before$Mean_Value %in% outliers2020_before$Mean_Value), ]
mean_dataframe_2020_no_outliers_after <- mean_dataframe_2020_after[!(mean_dataframe_2020_after$Mean_Value %in% outliers2020_after$Mean_Value), ]
mean_dataframe_2020_no_outliers_before


get_boxplot(mean_dataframe_2016_no_outliers, 2016)
get_boxplot(mean_dataframe_2020_no_outliers, 2020)
get_boxplot(mean_dataframe_2020_no_outliers_before, 2020, before=TRUE)


add_scatter_plot(mean_dataframe_2020, 2020)
plot_ditribution(mean_dataframe_2020, 2020)
plot_density(mean_dataframe_2020, 2020)
get_boxplot(mean_dataframe_2020, 2020)
get_boxplot(mean_dataframe_2016, 2016)

# Scatter plot for 2020, 3 month BEFORE the elections
mean_dataframe_2020_before <- create_dataframe(mean_list_2020_before)
add_scatter_plot(mean_dataframe_2020_before, 2020, before=TRUE)
plot_ditribution(mean_dataframe_2020_before, 2020, before=TRUE)
plot_density(mean_dataframe_2020_before, 2020, before=TRUE)
get_boxplot(mean_dataframe_2020_before, 2020, before=TRUE)

# Scatter plot for 2020, 3 months AFTER the elections
mean_dataframe_2020_after <- create_dataframe(mean_list_2020_after)
add_scatter_plot(mean_dataframe_2020_after, 2020, after=TRUE)
plot_ditribution(mean_dataframe_2020_after, 2020,after=TRUE)
plot_density(mean_dataframe_2020_after, 2020,after=TRUE)
get_boxplot(mean_dataframe_2020_after, 2020, after=TRUE)

# Scatter plot for 2016
mean_dataframe_2016 <- create_dataframe(mean_list_2016)
add_scatter_plot(mean_dataframe_2016, 2016)
plot_ditribution(mean_dataframe_2016, 2016)
plot_density(mean_dataframe_2016, 2016)
get_boxplot(mean_dataframe_2016, 2016)

# Scatter plot for 2016, 3 month BEFORE the elections
mean_dataframe_2016_before <- create_dataframe(mean_list_2016_before)
add_scatter_plot(mean_dataframe_2016_before, 2016, before=TRUE)
plot_ditribution(mean_dataframe_2016_before, 2016, before=TRUE)
plot_density(mean_dataframe_2016_before, 2016, before = TRUE)

mean_dataframe_2016

# Scatter plot for 2016, 3 months AFTER the elections
mean_dataframe_2016_after <- create_dataframe(mean_list_2016_after)
add_scatter_plot(mean_dataframe_2016_after, 2016, after=TRUE)
plot_ditribution(mean_dataframe_2016_after, 2016, after=TRUE)
plot_density(mean_dataframe_2016_after, 2016, after=TRUE)


  # Define a function to calculate the mode
get_mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
measures_of_central_tendency <- function(df){
# Calculate the median
median_value <- median(df$Mean_Value)


# Calculate the mode
mode_value <- get_mode(df$Mean_Value)

# Calculate the midrange
min_value <- min(df$Mean_Value)
max_value <- max(df$Mean_Value)
midrange <- (min_value + max_value) / 2
print(paste(median_value, mode_value, midrange))
}
measures_of_central_tendency(mean_dataframe_2016)
measures_of_central_tendency(mean_dataframe_2016_after)
measures_of_central_tendency(mean_dataframe_2016_before)
measures_of_central_tendency(mean_dataframe_2020)
measures_of_central_tendency(mean_dataframe_2020_after)
measures_of_central_tendency(mean_dataframe_2020_before)


# Function to calculate the range
calculate_range <- function(data) {
  max_val <- max(data)
  min_val <- min(data)
  range_val <- max_val - min_val
  return(range_val)
}

# Function to calculate the variance
calculate_variance <- function(data) {
  variance_val <- var(data)
  return(variance_val)
}

# Function to calculate the standard deviation
calculate_std_dev <- function(data) {
  std_dev_val <- sd(data)
  return(std_dev_val)
}

# Function to calculate the coefficient of variation
calculate_coeff_variation <- function(data) {
  coeff_variation_val <- (sd(data) / mean(data)) * 100
  return(coeff_variation_val)
}

# Calculate range, variance, standard deviation, and coefficient of variation for each dataframe
range_2016 <- calculate_range(mean_dataframe_2016$Mean_Value)
range_2016_after <- calculate_range(mean_dataframe_2016_after$Mean_Value)
range_2016_before <- calculate_range(mean_dataframe_2016_before$Mean_Value)
range_2020 <- calculate_range(mean_dataframe_2020$Mean_Value)
range_2020_after <- calculate_range(mean_dataframe_2020_after$Mean_Value)
range_2020_before <- calculate_range(mean_dataframe_2020_before$Mean_Value)

variance_2016 <- calculate_variance(mean_dataframe_2016$Mean_Value)
variance_2016_after <- calculate_variance(mean_dataframe_2016_after$Mean_Value)
variance_2016_before <- calculate_variance(mean_dataframe_2016_before$Mean_Value)
variance_2020 <- calculate_variance(mean_dataframe_2020$Mean_Value)
variance_2020_after <- calculate_variance(mean_dataframe_2020_after$Mean_Value)
variance_2020_before <- calculate_variance(mean_dataframe_2020_before$Mean_Value)

std_dev_2016 <- calculate_std_dev(mean_dataframe_2016$Mean_Value)
std_dev_2016_after <- calculate_std_dev(mean_dataframe_2016_after$Mean_Value)
std_dev_2016_before <- calculate_std_dev(mean_dataframe_2016_before$Mean_Value)
std_dev_2020 <- calculate_std_dev(mean_dataframe_2020$Mean_Value)
std_dev_2020_after <- calculate_std_dev(mean_dataframe_2020_after$Mean_Value)
std_dev_2020_before <- calculate_std_dev(mean_dataframe_2020_before$Mean_Value)
