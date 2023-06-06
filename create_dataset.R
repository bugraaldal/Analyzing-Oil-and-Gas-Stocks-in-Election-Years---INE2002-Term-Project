library(readr)
library(magrittr)
library(dplyr)

get_max_date <- function(csv_paths_list){
  dates_list <- as.Date(NULL)
  for (cp in csv_paths_list){
    data <- read.csv(cp)
    c_date <- as.Date(data$Date[1], format="%d-%m-%Y")
    dates_list <- c(dates_list, c_date)
  }

  return (max(dates_list))
}
get_min_date <- function(csv_paths_list){
  dates_list <- as.Date(NULL)
  for (cp in csv_paths_list){
    data <- read.csv(cp)
    c_date <- as.Date(data$Date[length(data$Date)], format="%d-%m-%Y")
    dates_list <- c(dates_list, c_date)
  }
  
  return (min(dates_list))
}

check_stocks_elections <- function(csv_paths_list){
  opt_elections_dates <- c(
    as.Date("2000-11-07"),
    as.Date("2004-11-02"),
    as.Date("2008-11-04"),
    as.Date("2012-11-06"),
    as.Date("2016-11-08"),
    as.Date("2020-11-02")
  )
  election_dates <- c(
    as.Date("2016-11-08"),
    as.Date("2020-11-02")
  )
  for (cp in csv_paths_list){
    data <- read.csv(cp)
    parsed_string <- strsplit(cp, "/")[[1]][length(strsplit(cp, "/")[[1]])]
    parsed_string <- stringr::str_remove(parsed_string, "\\.csv")
    data$Date <- as.Date(data$Date, format="%d-%m-%Y")
    subset_data <- data %>% filter(Date >= (election_dates - 90) & Date <= (election_dates + 90))
    optional_subset_data <- data %>% filter(Date >= (opt_elections_dates - 90) & Date <= (opt_elections_dates + 90))
    if (nrow(optional_subset_data) != 0){
      write.csv(optional_subset_data, file = paste("oil_subset_elections_stock_market/opt_elections_stock_market/", parsed_string, ".csv", sep=""), row.names = FALSE)
    }
    if (nrow(subset_data) != 0){
      write.csv(subset_data, file = paste("oil_subset_elections_stock_market/elections_stock_market/", parsed_string, ".csv", sep=""), row.names = FALSE)
    }
  }
}



filter_save_csv <- function(csv_paths_list, max_date, min_date){
  for (cp in csv_paths_list){
    data <- read.csv(cp)
    parsed_string <- strsplit(cp, "/")[[1]][length(strsplit(cp, "/")[[1]])]
    parsed_string <- stringr::str_remove(parsed_string, "\\.csv")
    data$Date <- as.Date(data$Date, format="%d-%m-%Y")
    subset_data <- subset(data, Date >= max_date & Date <= min_date)
    if(nrow(subset_data) != 0){
      write.csv(subset_data, file = paste("oil_subset_stock_market/", parsed_string, ".csv", sep=""), row.names = FALSE)
  }}
  return("Done")
}

parse_stock_names <- function(file_path){
  # Read the text file, skipping the specified number of lines
  text_lines <- readLines(file_path, warn=FALSE)
  
  # Print the content of the text file
  print(text_lines)
  # Create an empty vector to store every other line
  selected_lines <- c()
  
  # Store every other line in the selected_lines vector
  for (i in seq(1, length(text_lines), by = 2)) {
    selected_lines <- c(selected_lines, text_lines[i])
  }
  return (as.list(selected_lines))
}
# healthcare_names <- parse_stock_names("healthcare.txt")
# print(length(healthcare_names))

oil_names <- parse_stock_names("oil_and_gas.txt")
print(paste("There are", length(oil_names), "oil company stock names in these files."))

count <- 0
avaiable_path <- c()
for(sn in oil_names){
  file_path <- paste("stock_market_data/nyse/csv/", sn, ".csv", sep="")
  if (file.exists(file_path)){
    avaiable_path <- c(avaiable_path, file_path)
  }
  else{
    file_path <- paste("stock_market_data/nasdaq/csv/", sn, ".csv", sep="")
    if (file.exists(file_path)){
      avaiable_path <- c(avaiable_path, file_path)
    }
    else{
      file_path <- paste("stock_market_data/forbes2000/csv/", sn, ".csv", sep="")
      if (file.exists(file_path)){
        avaiable_path <- c(avaiable_path, file_path)
      }
      else{
        file_path <- paste("stock_market_data/sp500/csv/", sn, ".csv", sep="")
        if (file.exists(file_path)){
          avaiable_path <- c(avaiable_path, file_path)
        }
        else{
          count <- count + 1
        }
      }
    }
  }
}
print(avaiable_path)
print(paste(count, "of oil company stock names are not included in the dataset files."))
print(paste(length(avaiable_path), "of oil company stocks can be used."))




directory_path <- "oil_subset_stock_market/"
dir.create(directory_path, recursive = TRUE)
directory_path_elections <- "oil_subset_elections_stock_market/"
dir.create(directory_path_elections, recursive = TRUE)
child_folder <- file.path(directory_path_elections, "elections_stock_market/")
child_folder_opt <- file.path(directory_path_elections, "opt_elections_stock_market/")
dir.create(child_folder)
dir.create(child_folder_opt)

max_date <- get_max_date(avaiable_path)
min_date <- get_min_date(avaiable_path)

filter_save_csv(avaiable_path, max_date, min_date)


check_stocks_elections(avaiable_path)
print("Datasets created")
