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
source("modules/testing/testing.r")

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

# Testing parameters
start_dates <- c(
    "2022-08-01",
    "2022-09-01",
    "2022-10-01",
    "2022-11-01",
    "2022-12-01"
)

# Run tests
results <- run_tests(users, start_dates, verbose = TRUE)

# Print overall summary
results$df

# Get hdbscan plots for each start_date
p_hdbscan <- get_cluster_plot(start_dates, results)

# Get cluster proportions for each start_date
clusters_df <- get_cluster_results(start_dates, results)
clusters_df

# Get users per cluster proportions for each start_date
users_per_cluster_df <- get_users_per_cluster(start_dates, results)
users_per_cluster_df

# Melt clusters_df in 3 columns:
# 1. start_date: a column that contains the rownames(clusters_df)
# 2. cluster: a column that contains the colnames(clusters_df)
# 3. value: a column that contains the proportion value of clusters_df
clusters_melt <- melt(clusters_df, id.vars = "start_dates", variable.name = "cluster", value.name = "value")

# Plot the cluster proportions for each start_date
p_clusters_per_run <- plot_clusters_per_run(clusters_melt,
    x = "Start Date",
    y = "Cluster Proportion [%]",
    title = "Cluster Proportions for each Start Date"
)

# Plot the user proportions per cluster for each start_date
p_users_per_cluster <- plot_users_per_cluster(users_per_cluster_df)

# Create a list with all relevant results
results_list <- list(
    users = users,
    start_dates = start_dates,
    results = results,
    clusters_df = clusters_df,
    users_per_cluster_df = users_per_cluster_df,
    clusters_melt = clusters_melt
)

# Create a list with all the plots
plot_list <- list(
    p_hdbscan = p_hdbscan,
    p_clusters_per_run = p_clusters_per_run,
    p_users_per_cluster = p_users_per_cluster
)

# Set directory
dir_name <- "test_results"

# Save results and plots as RDS files in the test_results folder
write_object(results_list, file_name = "results_list", dir_name = dir_name)
write_object(plot_list, file_name = "plot_list", dir_name = dir_name)

# Read the plot_list from the R object
plot_list <- readRDS(paste(dir_name, "/plot_list.RDS", sep = ""))

# Save each plot from plot_list as an interactive html file
write_html_plots(plot_list, dir_name = dir_name, width = 1300, height = 600)
