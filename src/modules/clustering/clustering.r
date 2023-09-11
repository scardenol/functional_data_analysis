#' @title Initial center selection method using squared distance as a weighted probability
#' @description This function implements the initial center selection method using squared distance as a weighted probability
#' @param fun_data A functional data object
#' @param k The number of centers
#' @return A vector of indices of the data points that are chosen as the centers
#' @examples
#' \dontrun{
#' # Load phoneme data
#' data(phoneme)
#' # Unpack functional data to cluster
#' mlearn <- phoneme$learn
#' # Choose number of clusters k
#' k <- 5
#' # Do kmeans on the data
#' out_fd1 <- kmeans.fd(mlearn, ncl = k, draw = FALSE)
#' ini_centers <- ini_center_selection(mlearn, k = k)
#' }
#' @export
#' @author scardenol based on the code from
#'  (https://github.com/ethen8181/machine-learning/blob/master/clustering_old/clustering/kmeanspp.R)
pp_initialization <- function(fun_data, k) {
    # Check fun_data is functional data
    if (!is.fdata(fun_data)) stop("fun_data is not functional data")
    # Check k is a positive integer
    if (!is.numeric(k) || k <= 0 || k %% 1 != 0) stop("k is not a positive integer")

    # Unpack data matrix from functional data
    data_mat <- fun_data$data
    # Keep the unique data
    data_mat <- unique(data_mat)
    # Get the number of data points
    n <- nrow(data_mat)
    # Initialize a vector to store the indices of the centers
    centers_idxs <- integer(k)

    # Choose the first center index randomly
    centers_idxs[1] <- sample(1:n, 1)

    # Main loop
    for (i in 1:(k - 1)) {
        # Compute the squared distance between the center and all the data points
        distances <- apply(data_mat, 1, function(x) sum((x - data_mat[centers_idxs[i], ])^2))

        # Get the next center by using the squared distance as the weighted probability,
        # starting from the second center. The measure "squared distance" for each data point
        # is the minimum of the squared distance between the data point and each center that has
        # already been generated
        if (i == 1) {
            distance <- distances
        } else {
            distance <- cbind(distance, distances)
            distance <- apply(distance, 1, min)
        }
        centers_idxs[i + 1] <- sample(1:n, 1, prob = distances / sum(distances))
    }
    return(centers_idxs)
}

# Create a pam function that works on functional data (fdata objects)
#' @title pam for functional data
#' @description pam for functional data
#' @param fun_data A functional data object
#' @param k The number of clusters
#' @param metric The metric to use for the clustering
#' @param initial_medoids A string indicating the method to use for initial medoids selection
#' whether "pp" for plus plus or "random" for random selection.
#' @return A pam object
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- pam_fd(mlearn, k = 5, metric = "euclidean")
#' }
#' @author scardenol
pam_fd <- function(fun_data, k, metric = "euclidean", initial_medoids = "pp") {
    # Error handling ----------------------------------------------------------
    # If data is empty, return an error
    if (is.null(fun_data)) stop("data is empty")
    # If k is empty, return an error
    if (is.null(k)) stop("k is empty")
    # If k is not a number, return an error
    if (!is.numeric(k)) stop("k is not a number")
    # If k is not an integer, return an error
    if (!all.equal(k, as.integer(k))) stop("k is not an integer")
    # If k is not a positive number, return an error
    if (k <= 0) stop("k is not a positive number")
    # If metric is empty, return an error
    if (is.null(metric)) stop("metric is empty")
    # If metric is not a string, return an error
    if (!is.character(metric)) stop("metric is not a string")
    # If metric is not "euclidean" or "manhattan", return an error
    if (metric != "euclidean" && metric != "manhattan") stop("metric is not euclidean or manhattan")

    # Main function -----------------------------------------------------------
    # Unpack the fdata object
    data <- fun_data$data

    # Initial medoids selection
    if (initial_medoids == "pp") {
        # Get the indices of the initial medoids
        ini_medoids <- pp_initialization(fun_data, k)
        # Compute pam on data with specified metric
        out_fd <- pam(data, k = k, metric = metric, medoids = ini_medoids)
    } else if (initial_medoids == "random") {
        # Compute pam on data with specified metric
        out_fd <- pam(data, k = k, metric = metric)
    } else {
        stop("initial_medoids has to be a string equal to 'pp' or 'random'")
    }


    # Convert medoids to fdata object
    medoids <- mat2fun(
        data = out_fd$medoids,
        title_and_labels = fun_data$names,
        names = list(row_name = "center", col_name = "X")
    )

    # Repack the pam object
    out_fd <- list(
        cluster = out_fd$clustering,
        medoids = medoids,
        id.med = out_fd$id.med,
        objective = out_fd$objective,
        isolation = out_fd$isolation,
        clusinfo = out_fd$clusinfo,
        silinfo = out_fd$silinfo,
        diss = out_fd$diss,
        call = out_fd$call,
        data = data
    )

    # Return the pam object
    return(out_fd)
}

# Create an auxiliary function to call the hdbscan function
hdbscan_fd <- function(fun_data) {
    # Compute the number of columns of the data
    cols <- ncol(fun_data$data)
    # Takes the data and minPts = columns + 1
    results <- hdbscan(fun_data$data, minPts = cols + 1)
    results$cluster <- results$cluster + 1 # Make cluster labels start from 1
    return(results)
}

# Function to gather the cluster size and average silhouette width for each cluster
# Takes a silhouette class object and prints the results
compute_summary <- function(sil_object) {
    # Summary of the silhouette object
    summary_results <- summary(sil_object)
    # Store the cluster size and average width results as a single array
    main_results <- t(rbind(
        cluster_size = summary_results$clus.sizes,
        avg_silhouette_width = summary_results$clus.avg.widths
    ))
    # Create the cluster column and convert to data frame as a side effect
    main_results <- tibble::rowid_to_column(as.data.frame(main_results), "cluster")
    # Return the results
    return(main_results)
}

# 2) Distance-based clustering to determine the optimal number of clusters k

# Iterate over multiple values of k, compute Silhouette index for each method and plot
# the results

distance_based_clustering <- function(fun_data, K = 2:10, verbose = FALSE) {
    # Check if fun_data is a functional data object
    if (!is.fdata(fun_data)) stop("fun_data is not functional data")
    # Check if K is a numerical vector
    if (!is.numeric(K)) stop("K is not numerical")
    # Check if K is a vector with values greater than 1
    if (any(K < 2)) stop("K cant have values less than 2")
    # Check if verbose is a boolean or if it is a string equal to "minimal"
    if (!is.logical(verbose) && verbose != "minimal") stop("verbose is not a boolean or 'minimal'")
    # Flag if verbose is "minimal"
    minimal <- FALSE
    if (verbose == "minimal") {
        minimal <- TRUE
        verbose <- FALSE
    }

    # Distance matrices
    dist_mat <- dist(fun_data$data, method = "euclidean")
    dist_mat_man <- dist(fun_data$data, method = "manhattan")

    # Create an empty list to store the results
    kmeans_res <- list(
        time = c(),
        sil = c(),
        con = c(),
        dun = c(),
        sil_obj = list(),
        diagram = list(),
        summary = list(),
        output = list()
    )

    # Initialize variables
    pam_res <- kmeans_res
    kmeans_man_res <- kmeans_res

    # Iterate over multiple values of k
    for (k in K) {
        if (verbose | minimal) {
            # Print current iteration
            print(paste("-------------------------", "k =", k, "-------------------------"))
        }

        # ------------------------------- k-means -----------------------------------------

        # Run and time k-means
        start_kmeans <- Sys.time() # Start timer
        out_kmeans <- kmeans.fd(fun_data, ncl = k, draw = FALSE, metric = metric.lp, par.metric = list(p = 2))
        end_kmeans <- Sys.time() # End timer

        # Compute internal validation indices
        sil_kmeans <- silhouette(out_kmeans$cluster, dist_mat) # Compute silhouette index
        con_kmeans <- connectivity(distance = dist_mat, clusters = out_kmeans$cluster) # Compute connectivity
        dun_kmeans <- dunn(distance = dist_mat, clusters = out_kmeans$cluster) # Compute Dunn index

        # Compute silhouette diagram
        diag_kmeans <- fviz_silhouette(sil_kmeans,
            print.summary = FALSE,
            subtitle = paste("Functional k-means with k = ", k, sep = "")
        )
        # Compute summary
        summary_kmeans <- compute_summary(sil_kmeans)

        # Compute average silhouette index
        si_kmeans <- mean(summary_kmeans$avg_silhouette_width)

        # Append results to list
        kmeans_res$time <- c(kmeans_res$time, end_kmeans - start_kmeans)
        kmeans_res$sil <- c(kmeans_res$sil, si_kmeans)
        kmeans_res$con <- c(kmeans_res$con, con_kmeans)
        kmeans_res$dun <- c(kmeans_res$dun, dun_kmeans)
        kmeans_res$sil_obj[[k - 1]] <- sil_kmeans
        kmeans_res$diagram[[k - 1]] <- diag_kmeans
        kmeans_res$summary[[k - 1]] <- summary_kmeans
        kmeans_res$output[[k - 1]] <- out_kmeans

        if (verbose) {
            # print progress with summary
            print("Functional k-means with Euclidean distance")
            print(summary_kmeans)
        }

        # ------------------------------- PAM -----------------------------------------

        # Run and time PAM
        start_pam <- Sys.time() # Start timer
        out_pam <- pam_fd(fun_data, k = k, metric = "euclidean", initial_medoids = "pp")
        end_pam <- Sys.time() # End timer

        # Compute internal validation indices
        sil_pam <- silhouette(out_pam$cluster, dist_mat) # Compute silhouette index
        con_pam <- connectivity(distance = dist_mat, clusters = out_pam$cluster) # Compute connectivity
        dun_pam <- dunn(distance = dist_mat, clusters = out_pam$cluster) # Compute Dunn index
        # Compute silhouette diagram
        diag_pam <- fviz_silhouette(sil_pam,
            print.summary = FALSE,
            subtitle = paste("Functional PAM with k = ", k, sep = "")
        )

        # Compute summary
        summary_pam <- compute_summary(sil_pam)

        # Compute average silhouette index
        si_pam <- mean(summary_pam$avg_silhouette_width)

        # Append results to list
        pam_res$time <- c(pam_res$time, end_pam - start_pam)
        pam_res$sil <- c(pam_res$sil, si_pam)
        pam_res$con <- c(pam_res$con, con_pam)
        pam_res$dun <- c(pam_res$dun, dun_pam)
        pam_res$sil_obj[[k - 1]] <- sil_pam
        pam_res$diagram[[k - 1]] <- diag_pam
        pam_res$summary[[k - 1]] <- summary_pam
        pam_res$output[[k - 1]] <- out_pam

        if (verbose) {
            # print progress with summary
            print("Functional PAM with Euclidean distance")
            print(summary_pam)
        }

        # ------------------------------- k-means Manhattan --------------------------------------

        # Run and time k-means
        start_kmeans_man <- Sys.time() # Start timer
        out_kmeans_man <- kmeans.fd(fun_data, ncl = k, draw = FALSE, metric = metric.lp, par.metric = list(p = 1))
        end_kmeans_man <- Sys.time() # End timer

        # Compute internal validation indices
        sil_kmeans_man <- silhouette(out_kmeans_man$cluster, dist_mat_man) # Compute silhouette index
        con_kmeans_man <- connectivity(distance = dist_mat, clusters = out_kmeans_man$cluster) # Compute connectivity
        dun_kmeans_man <- dunn(distance = dist_mat, clusters = out_kmeans_man$cluster) # Compute Dunn index
        # Compute Silhouette diagram
        diag_kmeans_man <- fviz_silhouette(sil_kmeans_man,
            print.summary = FALSE,
            subtitle = paste("Functional k-means with Manhattan distance and k = ", k, sep = "")
        )

        # Compute summary
        summary_kmeans_man <- compute_summary(sil_kmeans_man)

        # Compute average silhouette index
        si_kmeans_man <- mean(summary_kmeans_man$avg_silhouette_width)

        # Append results to list
        kmeans_man_res$time <- c(kmeans_man_res$time, end_kmeans_man - start_kmeans_man)
        kmeans_man_res$sil <- c(kmeans_man_res$sil, si_kmeans_man)
        kmeans_man_res$con <- c(kmeans_man_res$con, con_kmeans)
        kmeans_man_res$dun <- c(kmeans_man_res$dun, dun_kmeans)
        kmeans_man_res$sil_obj[[k - 1]] <- sil_kmeans_man
        kmeans_man_res$diagram[[k - 1]] <- diag_kmeans_man
        kmeans_man_res$summary[[k - 1]] <- summary_kmeans_man
        kmeans_man_res$output[[k - 1]] <- out_kmeans_man

        if (verbose) {
            # print progress with summary
            print("Functional k-means with Manhattan distance")
            print(summary_kmeans_man)
        }
    }

    # Create a list with all the results
    results <- list(
        kmeans_res = kmeans_res,
        pam_res = pam_res,
        kmeans_man_res = kmeans_man_res
    )
    return(results)
}

# Store results in data frame to use with ggplot
results_to_df <- function(results, K) {
    # Unpack results
    kmeans_res <- results$kmeans_res
    pam_res <- results$pam_res
    kmeans_man_res <- results$kmeans_man_res

    # Create a data frame with the results
    results_df <- data.frame(
        k = rep(K, 3),
        time = c(kmeans_res$time, pam_res$time, kmeans_man_res$time),
        avg_sil = c(kmeans_res$sil, pam_res$sil, kmeans_man_res$sil),
        connect = c(kmeans_res$con, pam_res$con, kmeans_man_res$con),
        di = c(kmeans_res$dun, pam_res$dun, kmeans_man_res$dun),
        method = rep(c(
            "k-means (Euclidean)", "PAM (Euclidean)", "k-means (Manhattan)"
        ), each = length(K))
    )
    return(results_df)
}
