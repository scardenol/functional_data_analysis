# Clear environment
rm(list = ls())

# Set working directory to wherever the script is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load custom modules
source("modules/library_manager/library_manager.R")
source("modules/read_data/read_data.R")
source("modules/preprocessing/preprocessing.R")
source("modules/visualization/visualization.R")
source("modules/numerical/numerical.R")
source("modules/clustering/clustering.R")
source("modules/write_data/write_data.R")

# List all packages to install/load
packages <- c(
  "readxl", # To read the excel input files
  "tidyr",
  "plyr",
  "dplyr",
  "fda.usc", # Main functional data library
  "cluster", # Pam, Silhouette, Connectivity, Dunn
  "caret",
  "scales", # Opacity in plots
  "ggplot2", # Main static plot library
  "plotly", # Main dynamic plot library
  "factoextra", # Silhoutte diagram
  "clValid", # Connectivity and Dunn
  "dbscan", # HDBSCAN algorithm
  "DepthProc", # Functional boxplot fncBoxPlot
  "reshape2", # To use melt
  "LambertW",
  "latex2exp",
  "reshape2",
  "rgl",
  "htmltools"
)

# Load or Install and Load packages
load_packages(packages)

# Check if packages loaded
loaded_packages <- data.frame(
  Packages = packages,
  Loaded = packages %in% loadedNamespaces()
)
loaded_packages

# List all xlsx input files to be read
files <- c(
  "id_service_3144.xlsx",
  "id_service_3237.xlsx",
  "id_service_3260.xlsx",
  "id_service_3262.xlsx",
  "id_service_3285.xlsx",
  "id_service_3287.xlsx",
  "id_service_3302.xlsx",
  "id_service_3305.xlsx",
  "id_service_3314.xlsx",
  "id_service_3325.xlsx"
)

# Set the path to the data files
file_path <- paste(dirname(getwd()), "/data/", sep = "")

# Read all user files and store them in a named list
users <- read_files(files, file_path, col_range = cell_cols("C:F"))

# Convert each user into data frame
users <- lapply(users, as.data.frame)

# If files contains only one file, split the data frame into a list of data frames
if (length(files) == 1) {
  users <- split_df(users$users, split_col = "id_service")
}

# Check if the columns have the same name and data type for every user
check_columns(users)

# Print dimensions of all users
print_dim(users)

# Preprocess users:
# - Pass the columns to keep
# - Separate the timestamp column "record_timestamp" into date and time
# - Consolidate time column into hour
columns <- c("record_timestamp", "id_service", "value")
users <- preprocess_users(users, columns, timestamp_col = "record_timestamp")

# Keep only the last 4 months of data (2022-09-01 to 2022-12-31) dplyr style
users <- filter_by_date(users, start_date = "2022-09-01")

# Remove users with value close to 0 on an interval for a given frequency of hours per date
users <- remove_zero_users(users, interval = c(0, 1), freq = 0.8)

# Check how many complete days of data we have for each user vs the
# total amount of days
days_df <- check_days(users, user_name_col = "id_service")

# Filter users by matching dates
results <- matching_dates(users, days_df)
users <- results$users
dates <- results$dates

# Convert each user's data to an fdata object with its labels
users <- df_to_fdata(users, id_col = "id_service")

# Bind all users' data into a single fdata object and a single vector of labels
users <- bind_fdata(users)

# Extract the fdata object and labels from the resulting list
users_fdata <- users$fun_data
users_label <- users$cluster # User id name
users_id <- users$users

# Extract default labels and title from fdata object
plot_labels <- list(
  x = users_fdata$names$xlab,
  y = users_fdata$names$ylab,
  title = paste(users_fdata$names$main, " (", head(dates, 1), " - ", tail(dates, 1), ")", sep = "")
)

# Convert users_fdata to long format for plotting
users_lf <- fdata_to_long(users_fdata$data, str_dates = dates)

# Plot users by user id
p_raw <- plot_fdata(users_lf, plot_labels = plot_labels, group_by = users_id, legend_title = "User id")

# Scale users by standarization
users_fdata_scaled <- users_fdata # copy fdata object
scale_results <- scale_data(users_fdata$data)
# Extract results
users_fdata_scaled$data <- scale_results$scaled_data # scaled data
rm <- scale_results$rm # row means
rs <- scale_results$rs # row standard deviations

# Plot scaled users by user id
users_scaled_lf <- fdata_to_long(users_fdata_scaled$data, str_dates = dates)
p_scaled <- plot_fdata(users_scaled_lf, plot_labels = plot_labels, group_by = users_id, legend_title = "User id")

# Filtering
# Extract the colors used on a previous plot
colors <- unique(ggplot_build(p_raw)$data[[1]]$colour)
# Compute the deepest bands for each user
deepest_bands <- get_deepest_bands(users_fdata_scaled$data, users_id)
# Plot the deepest bands for each user
p_deepest_bands <- plot_deepest_bands(users_scaled_lf, deepest_bands, plot_labels = plot_labels, colors = colors)
# Filter data by the deepest bands with a frequency of 0.75
users_fdata_filtered <- users_fdata_scaled # copy fdata object
filter_results <- filter_data(users_fdata_scaled$data, deepest_bands, freq = 0.75)
# Extract results
users_fdata_filtered$data <- filter_results$filtered_data # filtered data
filter_idx <- filter_results$curves_idx # original index of the filtered curves

# Plot filtered users by user id
users_filtered_lf <- format_filtered_long_data(users_scaled_lf, group_by = users_id, filter_by = filter_idx)
p_filtered <- plot_fdata(users_filtered_lf, plot_labels = plot_labels, group_by = users_id, legend_title = "User id", filtered_data = TRUE)


# Experimentation
method <- "euclidean"
# 0. Run hdbscan_multiple_minPts to find the optimal minPts parameter
opt_minpts_df <- hdbscan_multiple_minPts(users_fdata_filtered, minPts = 2:100, method = method)
# Compute the optimal minPts parameter
opt_idx <- which.max(opt_minpts_df$mean_cluster_scores)
opt_minpts <- opt_minpts_df$minPts[opt_idx]

# 1. Density-based clustering with HDBSCAN
hdbscan_res <- hdbscan_fd(users_fdata_filtered, minPts = opt_minpts, method = method)

# Plot results
p_hdbscan <- plot_hdbscan_results(users_filtered_lf, hdbscan_res, opt_minpts_df)
p_hdbscan