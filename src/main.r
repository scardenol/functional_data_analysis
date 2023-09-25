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

# Check if the columns have the same name and data type for every user
check_columns(users)

# Print dimensions of all users
print_dim(users)

# Preprocess users:
# - Drop the "name" column
# - Separate the timestamp column "record_timestamp" into date and time
# - Consolidate time column into hour
users <- preprocess_users(users)

# Keep only the last 4 months of data (2022-09-01 to 2022-12-31) dplyr style
users <- filter_by_date(users, start_date = "2022-09-01")

# Remove users with value close to 0 on an interval for a given frequency of hours per date
users <- remove_zero_users(users, interval = c(0, 0.8), freq = 0.8)

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
# 1. Density-based clustering with HDBSCAN
hdbscan_res <- hdbscan_fd(users_fdata_filtered)

# Plot results
p_hdbscan <- plot_fdata(users_filtered_lf, plot_labels = plot_labels, group_by = hdbscan_res$cluster, legend_title = "Cluster")

# Visualize clusters on unscaled data
p_hdbscan_raw <- plot_fdata(format_filtered_long_data(users_lf), plot_labels = plot_labels, group_by = hdbscan_res$cluster, legend_title = "Cluster")

# 2. Distance-based clustering with: custom pam, k-means (Manhattan), k-means (Euclidean)
k_hdbscan <- length(unique(hdbscan_res$cluster)) # number of clusters from hdbscan
K <- 2:(2 * k_hdbscan - 2) # number of clusters to try
distance_res <- distance_based_clustering(users_fdata_filtered, K = K, verbose = "minimal")
# Extract results for pam
pam_res <- distance_res$pam_res

# Format results into a data frame
results_df <- results_to_df(distance_res, K = K)

# Visualize results: time, connectivity, dunn and silhouette
p_time <- plot_distance_results(results_df, result_type = "time")
p_connectivity <- plot_distance_results(results_df, result_type = "connectivity")
p_dunn <- plot_distance_results(results_df, result_type = "dunn")
p_silhouette <- plot_distance_results(results_df, result_type = "silhouette")

# Extract silhouette diagram for each method from the results
p_silhouette_kmeans <- distance_res$kmeans_res$diagram
p_silhouette_pam <- distance_res$pam_res$diagram
p_silhouette_kmeans_man <- distance_res$kmeans_man_res$diagram

# Conclude the optimal number of clusters from hdbscan results
k <- length(unique(hdbscan_res$cluster))

# Compute the norm indicators for the hdbscan method
norm_indicators_hdbscan <- plot_norm_indicators(users_fdata_filtered,
  method = "hdbscan",
  cluster = hdbscan_res$cluster,
  title = plot_labels$title,
  plot = FALSE
)
# Extract results for the hdbscan method
I_hdbscan <- norm_indicators_hdbscan$I # matrix with data in the indicators space
df_hdbscan <- norm_indicators_hdbscan$df # data frame with the indicators and their cluster
p_hdbscan_norm <- norm_indicators_hdbscan$sp # plot of the indicators space

# Compute the norm indicators for the pam method
norm_indicators_pam <- plot_norm_indicators(users_fdata_filtered,
  method = "pam",
  cluster = pam_res$output[[k - 1]]$cluster,
  title = plot_labels$title,
  plot = FALSE
)

# Extract results for the pam method
I_pam <- norm_indicators_pam$I # matrix with data in the indicators space
df_pam <- norm_indicators_pam$df # data frame with the indicators and their cluster
p_pam_norm <- norm_indicators_pam$sp # plot of the indicators space

# Filtered users_id
users_id_filtered <- users_id[filter_idx, , drop = FALSE]

# Visualize users versus hdbscan clusters
plot_res <- plot_users_vs_clusters(
  users = users_id_filtered,
  density_clusters = hdbscan_res$cluster,
  distance_clusters = pam_res$output[[k - 1]]$cluster,
  methods = c("hdbscan", "pam")
)

# Extract results
p_users_vs_hdbscan <- plot_res$p_users_vs_density
p_users_vs_pam <- plot_res$p_users_vs_distance

# Create a list with all the plots (variables starting with p_) and name them with the variable name
plot_list <- list(
  p_raw = p_raw,
  p_scaled = p_scaled,
  p_deepest_bands = p_deepest_bands,
  p_filtered = p_filtered,
  p_hdbscan = p_hdbscan,
  p_hdbscan_raw = p_hdbscan_raw,
  p_time = p_time,
  p_connectivity = p_connectivity,
  p_dunn = p_dunn,
  p_silhouette = p_silhouette,
  p_silhouette_kmeans = p_silhouette_kmeans,
  p_silhouette_pam = p_silhouette_pam,
  p_silhouette_kmeans_man = p_silhouette_kmeans_man,
  p_hdbscan_norm = p_hdbscan_norm,
  p_pam_norm = p_pam_norm,
  p_users_vs_hdbscan = p_users_vs_hdbscan,
  p_users_vs_pam = p_users_vs_pam
)

# Save the plot_list as an R object
write_object(plot_list, file_name = "plot_list", dir_name = "images")

# Read the plot_list from the R object
plot_list <- readRDS("images/plot_list.RDS")

# Save each plot from plot_list as an interactive html file
write_html_plots(plot_list, dir_name = "images", width = 1300, height = 600)
