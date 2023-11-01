# Function that peforms a test run which is composed of multiple preprocessing stages
# (filter zeroes, bind, scale, band filter, HDBSCAN clustering).
# It takes the users data and the start_date which is the only testing hyperparameter.
# The functions returns a list with every output of each stage.

test_run <- function(users, start_date) {
    # Keep only the last 4 months of data (2022-09-01 to 2022-12-31) dplyr style
    users <- filter_by_date(users, start_date = start_date)

    # Remove users with value close to 0 on an interval for a given frequency of hours per date
    users <- remove_zero_users(users, interval = c(0, 1), freq = 0.8)

    # Check how many complete days of data we have for each user vs the
    # total amount of days
    days_df <- check_days(users, user_name_col = "id_service")

    # Filter users by matching dates
    users_matching <- matching_dates(users, days_df, verbose = FALSE)
    users <- users_matching$users
    dates <- users_matching$dates

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

    # Density-based clustering with HDBSCAN
    hdbscan_res <- hdbscan_fd(users_fdata_filtered)

    # Plot results
    p_hdbscan <- plot_fdata(users_filtered_lf, plot_labels = plot_labels, group_by = hdbscan_res$cluster, legend_title = "Cluster")

    # Filtered users id
    users_id_filtered <- users_id[filter_idx, , drop = FALSE]

    # Create a named list with all the results
    results <- list(
        days_df = days_df,
        users_matching = users_matching,
        dates = dates,
        users_fdata = users_fdata,
        users_label = users_label,
        users_id = users_id,
        users_lf = users_lf,
        p_raw = p_raw,
        users_fdata_scaled = users_fdata_scaled,
        users_scaled_lf = users_scaled_lf,
        p_scaled = p_scaled,
        colors = colors,
        deepest_bands = deepest_bands,
        p_deepest_bands = p_deepest_bands,
        users_fdata_filtered = users_fdata_filtered,
        filter_idx = filter_idx,
        users_filtered_lf = users_filtered_lf,
        p_filtered = p_filtered,
        hdbscan_res = hdbscan_res,
        clusters = hdbscan_res$cluster,
        p_hdbscan = p_hdbscan,
        users_id_filtered = users_id_filtered,
        num_clusters = length(table(hdbscan_res$cluster)),
        cluster_proportions = round(table(hdbscan_res$cluster) * 100 / length(hdbscan_res$cluster), 2),
        num_days = length(dates),
        cluster_per_user = data.frame(
            users_id = paste(users_id_filtered),
            cluster = paste(hdbscan_res$cluster)
        )
    )

    # Return the results
    return(results)
}

# Create an auxiliary function to run all the tests
run_tests <- function(users, start_dates, verbose = TRUE) {
    results <- list()
    # Create empty data frame with columns: start_dates, run_time, num_days
    df <- data.frame(
        start_dates = start_dates,
        run_time = NA,
        num_days = NA,
        num_clusters = NA
    )
    # Create an empty list to store the tables
    clusters <- list()

    # Loop through each start date
    for (i in 1:length(start_dates)) {
        # Name the test based on the iteration i and the start date
        test_name <- paste("test_", i, "_", start_dates[i], sep = "")
        # Start counter to time run
        tic <- Sys.time()
        # Run test and store with name
        results[[test_name]] <- test_run(users, start_dates[i])
        # End counter
        toc <- Sys.time()

        # Compute run time in seconds and store in test_df
        df[i, "run_time"] <- round(difftime(toc, tic, units = "secs"), 2)
        # Store number of days in test_df
        df[i, "num_days"] <- results[[test_name]]$num_days
        # Store number of clusters in test_df
        df[i, "num_clusters"] <- results[[test_name]]$num_clusters
        # Store cluster proportions in clusters list
        clusters[[test_name]] <- results[[test_name]]$cluster_proportions

        if (verbose) {
            # Print date and run time in seconds rounded to 2 decimals
            print(paste("Date: ", start_dates[i],
                ", Time: ", df[i, "run_time"], " seconds, ",
                "Days: ", df[i, "num_days"],
                sep = ""
            ))
            # Print cluster results
            print("Clusters:")
            print(results[[test_name]]$cluster_proportions)
        }
    }

    # Create a results named list
    test_results <- list(
        results = results,
        df = df,
        clusters = clusters
    )

    return(test_results)
}

# Get the hdbscan plot for each start_date
get_cluster_plot <- function(start_dates, results) {
    # Create an empty list to store the plots
    p <- list()

    # Loop through each start_date
    for (i in 1:length(start_dates)) {
        # Create the name to store the plot in the named list
        name <- paste("p_hdbscan_", i, sep = "")
        # Plot the users per cluster proportions of reach start_date
        p[[name]] <- results$results[[i]]$p_hdbscan
    }

    # Return the list of plots
    return(p)
}

# A function that takes the test_results as input and returns a data frame with the cluster proportions
# for each start_date
get_cluster_results <- function(start_dates, test_results) {
    # Extract the cluster proportions from the test_results
    clusters <- test_results$clusters
    # Extract the df from the test_results
    df <- test_results$df
    # Check if all the rows for num_clusters column are the same
    if (all(df$num_clusters == df[1, "num_clusters"])) {
        # Append every table in cluster_proportions into a single table
        clusters <- do.call(rbind, clusters)
        # Convert to data frame and keep the original column names. Add start_dates as the first column
        clusters <- data.frame(start_dates, clusters, check.names = FALSE)
    }

    # Return the cluster proportions df
    return(clusters)
}

# Function to create a barplot of the cluster proportions for each start date
plot_clusters_per_run <- function(clusters_melt, x = NULL, y = NULL, title = NULL) {
    # Check if x label is NULL
    if (is.null(x)) x <- "Start Date"
    # Check if y label is NULL
    if (is.null(y)) y <- "Cluster Proportion [%]"
    # Check if title is NULL
    if (is.null(title)) title <- "Cluster Proportions for each Start Date"

    # Plot the cluster proportions for each start_date using ggplot and barplot.
    # Display the values on top of the bars and color the text according to the cluster.
    p <- ggplot(clusters_melt, aes(x = start_dates, y = value, fill = cluster)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.25) +
        labs(
            x = x,
            y = y,
            title = title
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Return the plot object
    return(p)
}

# Function to create a data frame with the proportions of users per cluster for each start date
get_users_per_cluster <- function(start_dates, test_results) {
    # Create an empty data frame
    clusters_users_df <- data.frame()

    # Loop through each start_date
    for (i in 1:length(start_dates)) {
        # Data frame of users per cluster
        cluster_per_user <- test_results$results[[i]]$cluster_per_user
        cluster_per_user <- cluster_per_user %>%
            group_by(cluster, users_id) %>%
            summarize(count = n(), .groups = "keep") %>%
            as.data.frame() %>%
            transmute(cluster, users_id, value = count * 100 / sum(count))
        # Add start_dates[i] as a column
        cluster_per_user <- cbind(start_dates = start_dates[i], cluster_per_user)
        # Bind to clusters_users_df
        clusters_users_df <- rbind(clusters_users_df, cluster_per_user)
    }

    # Return the data frame
    return(clusters_users_df)
}

# Plot users per cluster for each start date
plot_users_per_cluster <- function(users_per_cluster_df, x = NULL, y = NULL) {
    # Unique number of clusters
    n_clusters <- as.numeric(unique(users_per_cluster_df$cluster))

    # Check if x label is NULL
    if (is.null(x)) x <- "Start Date"
    # Check if y label is NULL
    if (is.null(y)) y <- "Cluster Proportion [%]"

    # Create an empty list to store the plots
    p <- list()

    # Loop through each cluster and plot the users per cluster proportions of reach start_date
    for (i in 1:length(n_clusters)) {
        # Create the name to store the plot in the named list
        name <- paste("p_users_per_cluster_", n_clusters[i], sep = "")
        # Plot the users per cluster proportions of reach start_date
        p[[name]] <- ggplot(users_per_cluster_df[users_per_cluster_df$cluster == n_clusters[i], ], aes(x = start_dates, y = value, fill = users_id)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(aes(label = round(value, 2)), position = position_dodge(width = 0.9), vjust = -0.25) +
            labs(
                x = x,
                y = y,
                title = paste("Cluster", n_clusters[i], "per user for each Start Date", sep = " ")
            )
    }

    # Return the list of plots
    return(p)
}
