# Function to plot fdata with ggplot
plot_fdata <- function(users_lf, plot_labels = NULL, group_by = NULL, legend_title = NULL, color = "#0072B2", filtered_data = FALSE) {
    # Number of observations (columns) per curve
    cols <- length(unique(users_lf$Hour))

    # If we want to color the data by group_by
    if (length(group_by) != 0) {
        # Add group id column if filtered_data is not true
        if (!filtered_data) {
            users_lf$id <- unlist(do.call(rbind, replicate(cols, as.data.frame(group_by), simplify = FALSE)))
        }

        # Create plot object with ggplot
        plot_object <- users_lf %>%
            ggplot(aes(x = Hour, y = Value, color = as.factor(id), group = interaction(date, label))) +
            geom_line() +
            scale_color_discrete() +
            labs(
                x = plot_labels$x,
                y = plot_labels$y,
                color = legend_title,
                title = plot_labels$title
            )
    } else { # If we don't want to color the data by group_by

        # Create plot object with ggplot
        plot_object <- users_lf %>%
            ggplot(aes(x = Hour, y = Value, group = interaction(date, label))) +
            geom_line(color = color) +
            labs(
                x = plot_labels$x,
                y = plot_labels$y,
                title = plot_labels$title
            )
    }

    return(plot_object)
}

# Plot function to visualize the deepest bands per user.
# It takes the data and the results list from get_deepest_bands() and returns a list of ggplot objects.
plot_deepest_bands <- function(data_lf, deepest_bands, plot_labels = NULL, colors = NULL, legend_title = "Central band", title_per_plot = TRUE) {
    # Initialize variables
    i <- 1 # Index for colors and naming
    results <- list() # List of results
    labels <- plot_labels # Copy plot_labels
    # Iterate over the deepest bands
    for (db in deepest_bands) {
        # Unpack the deepest_bands results
        mean_curve <- db$mean # Mean curve
        upper_curve <- db$upper # Mean + sd curve
        lower_curve <- db$lower # Mean - sd curve
        curves_idx <- db$curves_idx # Indexes of the curves that belong to the current user
        user <- db$user # User name
        # Filter data based on curves idx
        data_filtered <- data_lf[data_lf$label %in% curves_idx, ]
        # Check if title_per_plot is true
        if (title_per_plot) labels$title <- paste(plot_labels$title, " - User ", user, sep = "")
        # Plot data with plot_fdata() to get the base plot
        p <- plot_fdata(data_filtered, labels, color = colors[i])
        # Row bind mean_curve, upper_curve and lower_curve as a data frame
        df <- as.data.frame(rbind(mean_curve, upper_curve, lower_curve))
        # Use gather to convert the data frame to long format and add a column with the curve type
        df <- df %>%
            mutate(type = c("mean", "upper", "lower")) %>%
            gather(key = "Hour", value = "Value", -type)
        # Add the deepest bands to the plot but don't inherit the aesthetics from the base plot.
        # Color all bands as black, but make the upper and lower bands dashed.
        p <- p +
            geom_line(data = df, aes(x = Hour, y = Value, color = type, group = type, linetype = type)) +
            scale_color_manual(values = c("mean" = "black", "upper" = "black", "lower" = "black")) +
            scale_linetype_manual(values = c("mean" = "solid", "upper" = "dashed", "lower" = "dashed")) +
            labs(
                color = legend_title,
                linetype = legend_title
            )
        # Set the current user name based on the counter
        name <- paste("user", i, sep = "")
        # Add the plot to the results list
        results[[name]] <- p
        # Increment the counter
        i <- i + 1
    }
    return(results)
}

# Auxiliary function to plot hdbscan results
plot_hdbscan_results <- function(data_lf, hdbscan_res, opt_minpts_df) {
    # Plot the results of hdbscan
    p_hdbscan <- plot_fdata(data_lf, plot_labels = plot_labels, group_by = hdbscan_res$cluster, legend_title = "Cluster")

    # Store both the optimal minPts and its mean cluster score
    opt_idx <- which.max(opt_minpts_df$mean_cluster_scores)
    opt_minpts <- opt_minpts_df$minPts[opt_idx]
    opt_minpts_mean_cluster_score <- opt_minpts_df$mean_cluster_scores[opt_idx]

    # Plot results as a line plot. Highlight the optimal minPts by changing the color of the point. Add a vertical dashed line as well.
    p_opt_minpts <- ggplot(opt_minpts_df, aes(x = minPts, y = mean_cluster_scores)) +
        geom_line() +
        geom_point() +
        geom_point(aes(x = opt_minpts, y = opt_minpts_mean_cluster_score), color = "red") +
        geom_vline(xintercept = opt_minpts, linetype = "dashed", color = "red") +
        labs(x = "minPts", y = "Mean cluster scores", title = "Mean cluster scores vs minPts")

    # Plot number of clusters vs minPts
    p_num_clusters <- ggplot(opt_minpts_df, aes(x = minPts, y = num_clusters)) +
        geom_line() +
        geom_point() +
        geom_point(aes(x = opt_minpts, y = num_clusters[opt_idx]), color = "red") +
        geom_vline(xintercept = opt_minpts, linetype = "dashed", color = "red") +
        labs(x = "minPts", y = "Number of clusters", title = "Number of clusters vs minPts")

    # Store plots in a named list
    results <- list(
        p_hdbscan = p_hdbscan,
        p_opt_minpts = p_opt_minpts,
        p_num_clusters = p_num_clusters
    )

    # Return results
    return(results)
}

# Plot function to visualize the results of distance-based clustering
plot_distance_results <- function(results_df, result_type = NULL) {
    # List of possible result types
    result_types <- c("time", "connectivity", "dunn", "silhouette")

    # Stop if result_type is not in result_types
    if (!(result_type %in% result_types) || is.null(result_type)) {
        stop("result_type must be one of these: ", paste(result_types, collapse = ", "))
    }

    # Create a list of parameters to pass to ggplot depending on the result_type
    parameters <- list(
        time = list(
            y_aes = "time",
            y = "Time (s)",
            title = "Time for each method"
        ),
        connectivity = list(
            y_aes = "connect",
            y = "Connectivity index",
            title = "Connectivity index for each method"
        ),
        dunn = list(
            y_aes = "di",
            y = "Dunn index",
            title = "Dunn index for each method"
        ),
        silhouette = list(
            y_aes = "avg_sil",
            y = "Silhouette index",
            title = "Silhouette index for each method"
        )
    )

    # Get parameters for the result_type
    x_lab <- "Number of clusters k"
    y_aes <- parameters[[result_type]]$y_aes
    y_lab <- parameters[[result_type]]$y
    title_lab <- parameters[[result_type]]$title

    # Check if result_type is time
    if (result_type == "time") {
        # Use a barplot for time. Put time values on top of each bar and make the x-axis discrete
        plot_object <- ggplot(results_df, aes(x = factor(k), y = get(y_aes), fill = method)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(aes(label = round(time, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
            theme_bw() +
            labs(
                x = x_lab,
                y = y_lab,
                title = title_lab
            )
    } else {
        # For every other result type simply use lines and points
        plot_object <- ggplot(results_df, aes(x = k, y = get(y_aes), color = method)) +
            geom_line() +
            geom_point() +
            theme_bw() +
            labs(
                x = x_lab,
                y = y_lab,
                title = title_lab
            )
    }
    return(plot_object)
}

# Function that uses the custom norm indicators to transform the data space into a norm space and plot the results
norm_indicators <- function(fun_data, label, p, title, plot = TRUE) {
    if (length(p) < 2 || length(p) > 3) {
        stop("p needs to be a vector of length 2 or 3")
    }


    if (length(p) == 2) {
        # 2D case
        # Calculate the norm indicators
        num_curves <- nrow(fun_data$data)
        I <- matrix(nrow = 2, ncol = num_curves)
        sum1 <- c()
        sum2 <- c()
        p1 <- p[1] # Norm for I1
        p2 <- p[2] # Norm for I2
        for (j in 1:num_curves) {
            for (i in 1:num_curves) {
                sum1[i] <- lp_norm(fun_data$data[j, ] - fun_data$data[i, ], p = p1)
                sum2[i] <- lp_norm(fun_data$data[j, ] - fun_data$data[i, ], p = p2)
            }
            I[, j] <- c(mean(sum1), mean(sum2))
        }

        # Save as data frame
        df <- data.frame(I[1, ], I[2, ], label)
        names(df) <- c("norm1", "norm2", "class")

        if (missing(title)) {
            title <- paste(
                "Curve indicators $\\textit{I}_{", p1,
                "}$ vs $\\textit{I}_{", p2, "}$"
            )
            title <- TeX(title)
        } else {
            title <- title
        }

        # Create and save plot
        sp <- ggplot(
            data = df,
            mapping = aes(x = norm1, y = norm2, group = factor(class))
        ) +
            geom_point(alpha = 0.8, aes(color = factor(class))) +
            guides(color = guide_legend(title = "Population")) +
            labs(fill = "Population") +
            ggtitle(title) +
            xlab(paste0("I_", p1, sep = "")) +
            ylab(paste0("I_", p2, sep = ""))

        if (plot) {
            # Show plot
            print(sp)
        }

        # Return indicators
        return(list(I = I, df = df, sp = sp))
    } else {
        # 3D case
        # Calculate the norm indicators
        num_curves <- nrow(fun_data$data)
        I <- matrix(nrow = 3, ncol = num_curves)
        sum1 <- c()
        sum2 <- c()
        sum3 <- c()
        p1 <- p[1] # Norm for I1
        p2 <- p[2] # Norm for I2
        p3 <- p[3] # Norm for I3
        for (j in 1:num_curves) {
            for (i in 1:num_curves) {
                sum1[i] <- lp_norm(fun_data$data[j, ] - fun_data$data[i, ], p = p1)
                sum2[i] <- lp_norm(fun_data$data[j, ] - fun_data$data[i, ], p = p2)
                sum3[i] <- lp_norm(fun_data$data[j, ] - fun_data$data[i, ], p = p3)
            }
            I[, j] <- c(mean(sum1), mean(sum2), mean(sum3))
        }

        # Save as data frame
        df <- data.frame(I[1, ], I[2, ], I[3, ], label)
        names(df) <- c("norm1", "norm2", "norm3", "class")

        if (missing(title)) {
            title <- paste(
                "Curve indicators $\\textit{I}_{", p1,
                "}$ vs $\\textit{I}_{", p2, "}$ vs $I_{", p3, "}$"
            )
            title <- TeX(title)
        } else {
            title <- title
        }

        # Create and save plot
        colors <- rainbow(length(unique(label)))
        par3d(windowRect = c(0, 23, 1366, 720)) # Fullscreen
        sp <- plot3d(
            x = df$norm1, y = df$norm2, z = df$norm3,
            type = "s", col = colors, size = 0.8,
            xlab = paste0("I_", p1, sep = ""),
            ylab = paste0("I_", p2, sep = ""),
            zlab = paste0("I_", p3, sep = "")
        )
        bgplot3d({
            plot.new()
            title(main = title)
            legend("topright",
                legend = seq(length(unique(label))), pch = c(16),
                col = colors, cex = 1.2, inset = c(0.1), title = "Population"
            )
            # use here any other way you fancy to write your title
        })
        if (plot) {
            # Show plot
            print(sp)
        }

        # Return indicators
        return(list(I = I, df = df, sp = sp))
    }
}

# Auxiliary function to plot the norm_indicators depending on the result
plot_norm_indicators <- function(fun_data, method = NULL, cluster, norms = c(Inf, 2), title = NULL, plot = TRUE) {
    # Create a list of possible methods
    methods <- c("hdbscan", "pam", "kmeans", "kmeans_man")
    # Check if method is in the methods
    if (!(method %in% methods) || is.null(method)) {
        stop("method must be one of these: ", paste(methods, collapse = ", "))
    }
    # Check if cluster is NULL
    if (is.null(cluster)) stop("cluster cannot be NULL")
    # Check if norms in numeric
    if (!is.numeric(norms)) stop("norms must be numeric")
    # If title is null extract the title from the fdata object
    if (is.null(title)) {
        title <- fun_data$names$main
    }
    # Compute the norm indicators
    norm_results <- norm_indicators(fun_data, label = cluster, p = norms, title = title, plot = plot)

    return(norm_results)
}

# Plot function to contrast the users (original groups) with the clusters (found groups)
plot_users_vs_clusters <- function(users, density_clusters, distance_clusters, methods = c("hdbscan", "pam")) {
    # Create data frame of labels with columns: users, density_clusters, distance_clusters
    label_df <- data.frame(
        users = paste(users),
        density_clusters = paste(density_clusters),
        distance_clusters = paste(distance_clusters)
    )

    # Common labels for the plots
    x_lab <- "User"
    y_lab <- "Number of members"

    # Create barplot contrasting users with density_clusters
    p_users_vs_density <- ggplot(label_df, aes(x = users, fill = density_clusters)) +
        geom_bar(stat = "count", position = "dodge") +
        theme_bw() +
        labs(
            x = x_lab,
            y = y_lab,
            title = paste("Number of users per clusters from", methods[1], sep = " ")
        )

    # Create barplot contrasting users with distance_clusters
    p_users_vs_distance <- ggplot(label_df, aes(x = users, fill = distance_clusters)) +
        geom_bar(stat = "count", position = "dodge") +
        theme_bw() +
        labs(
            x = x_lab,
            y = y_lab,
            title = paste("Number of users per clusters from", methods[2], sep = " ")
        )

    # Pack plot objects
    results <- list(
        p_users_vs_density = p_users_vs_density,
        p_users_vs_distance = p_users_vs_distance
    )
    return(results)
}
