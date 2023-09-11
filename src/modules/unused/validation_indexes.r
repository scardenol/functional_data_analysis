# Set working directory to wherever the script is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the reorder_labels function from script
if (!exists("reorder_labels", mode = "function")) source("reorder_labels.R")

# Function that computes internal and external validation indexes
#' @param fd An object of class fdata
#' @param out_fd An object of class kmeans.fd
#' @param labels A vector of labels
#' @param validation_type A string indicating the type of
#'  validation to be computed: "internal", "external" or "both". Default is "both".
#' @param verbose A boolean indicating whether to print the results or not. Default is TRUE.
#' @return A list with the internal and external validation indexes
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' validation_indexes(mlearn, out_fd1, labels)
#' }
#' @author scardenol

validation_indexes <- function(fd, out_fd, labels, validation_type = "both", verbose = TRUE) {
    # Get the number of clusters
    k <- length(unique(out_fd$cluster))
    # Get the number of elements in each cluster
    n <- table(out_fd$cluster)
    # Get the order of the clusters
    order <- order(n, decreasing = TRUE)
    # Reorder the labels
    labels <- reorder_labels(out_fd$cluster)

    # Compute the internal validation indexes
    # Compute the Davies-Bouldin index
    db <- daviesbouldin(fd, out_fd$cluster)
    # Compute the Dunn index
    dunn <- dunnindex(fd, out_fd$cluster)
    # Compute the Halinski-Harabasz index
    hh <- halinskib(fd, out_fd$cluster)

    # Compute the external validation indexes
    # Compute the Rand index
    rand <- randindex(out_fd$cluster, labels)
    # Compute the Jaccard index
    jaccard <- jaccard(out_fd$cluster, labels)
    # Compute the Fowlkes-Mallows index
    fm <- fowlkesmallows(out_fd$cluster, labels)

    # Print the results
    if (verbose) {
        cat("Internal validation indexes: \n Davies-Bouldin: ", db,
            " Dunn: ", dunn, " Halinski-Harabasz: ", hh,
            "External validation indexes:   Rand: ", rand,
            " Jaccard: ", jaccard, " Fowlkes-Mallows: ", fm,
            sep = ""
        )
    }
    # Return the results
    return(list(db = db, dunn = dunn, hh = hh, rand = rand, jaccard = jaccard, fm = fm))
}

# Internal validation indexes
#' @title Davies-Bouldin index
#' @description Computes the Davies-Bouldin index
#' @param fd An object of class fdata
#' @param clusters A vector of labels
#' @return A numeric value
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' daviesbouldin(mlearn, out_fd1$cluster)
#' }
#' @author scardenol
daviesbouldin <- function(fd, clusters) {
    # Get the number of clusters
    k <- length(unique(clusters))
    # Get the number of elements in each cluster
    n <- table(clusters)
    # Get the order of the clusters
    order <- order(n, decreasing = TRUE)
    # Reorder the labels
    clusters <- reorder_labels(clusters)
    # Compute the Davies-Bouldin index
    db <- 0
    for (i in 1:k) {
        for (j in (i + 1):k) {
            db <- db + (n[i] + n[j]) * dist(fd[clusters == i], fd[clusters == j])
        }
    }
    db <- db / (k * (k - 1))
    return(db)
}

#' @title Dunn index
#' @description Computes the Dunn index
#' @param fd An object of class fdata
#' @param clusters A vector of labels
#' @return A numeric value
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' dunnindex(mlearn, out_fd1$cluster)
#' }
#' @author scardenol
dunnindex <- function(fd, clusters) {
    # Get the number of clusters
    k <- length(unique(clusters))
    # Get the number of elements in each cluster
    n <- table(clusters)
    # Get the order of the clusters
    order <- order(n, decreasing = TRUE)
    # Reorder the labels
    clusters <- reorder_labels(clusters)
    # Compute the Dunn index
    dunn <- 0
    for (i in 1:k) {
        for (j in (i + 1):k) {
            dunn <- max(dunn, min(dist(fd[clusters == i], fd[clusters == j])))
        }
    }
    return(dunn)
}

#' @title Halinski-Harabasz index
#' @description Computes the Halinski-Harabasz index
#' @param fd An object of class fdata
#' @param clusters A vector of labels
#' @return A numeric value
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' halinskib(mlearn, out_fd1$cluster)
#' }
#' @author scardenol
halinskib <- function(fd, clusters) {
    # Get the number of clusters
    k <- length(unique(clusters))
    # Get the number of elements in each cluster
    n <- table(clusters)
    # Get the order of the clusters
    order <- order(n, decreasing = TRUE)
    # Reorder the labels
    clusters <- reorder_labels(clusters)
    # Compute the Halinski-Harabasz index
    hh <- 0
    for (i in 1:k) {
        hh <- hh + n[i] * log(n[i]) - n[i] * log(sum(dist(fd[clusters == i], fd[clusters == i])))
    }
    hh <- hh / k
    return(hh)
}

# External validation indexes
#' @title Rand index
#' @description Computes the Rand index
#' @param clusters1 A vector of labels
#' @param clusters2 A vector of labels
#' @return A numeric value
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' randindex(out_fd1$cluster, labels)
#' }
#' @author scardenol
randindex <- function(clusters1, clusters2) {
    # Get the number of clusters
    k1 <- length(unique(clusters1))
    k2 <- length(unique(clusters2))
    # Get the number of elements in each cluster
    n1 <- table(clusters1)
    n2 <- table(clusters2)
    # Get the order of the clusters
    order1 <- order(n1, decreasing = TRUE)
    order2 <- order(n2, decreasing = TRUE)
    # Reorder the labels
    clusters1 <- reorder_labels(clusters1)
    clusters2 <- reorder_labels(clusters2)
    # Compute the Rand index
    rand <- 0
    for (i in 1:k1) {
        for (j in 1:k2) {
            rand <- rand + sum(clusters1 == i & clusters2 == j) * (sum(clusters1 == i & clusters2 == j) - 1)
        }
    }
    rand <- rand / (sum(n1) * (sum(n1) - 1))
    return(rand)
}

#' @title Jaccard index
#' @description Computes the Jaccard index
#' @param clusters1 A vector of labels
#' @param clusters2 A vector of labels
#' @return A numeric value
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' jaccard(out_fd1$cluster, labels)
#' }
#' @author scardenol
jaccard <- function(clusters1, clusters2) {
    # Get the number of clusters
    k1 <- length(unique(clusters1))
    k2 <- length(unique(clusters2))
    # Get the number of elements in each cluster
    n1 <- table(clusters1)
    n2 <- table(clusters2)
    # Get the order of the clusters
    order1 <- order(n1, decreasing = TRUE)
    order2 <- order(n2, decreasing = TRUE)
    # Reorder the labels
    clusters1 <- reorder_labels(clusters1)
    clusters2 <- reorder_labels(clusters2)
    # Compute the Jaccard index
    jaccard <- 0
    for (i in 1:k1) {
        for (j in 1:k2) {
            jaccard <- jaccard + sum(clusters1 == i & clusters2 == j) / sum(clusters1 == i | clusters2 == j)
        }
    }
    jaccard <- jaccard / (k1 * k2)
    return(jaccard)
}

#' @title Fowlkes-Mallows index
#' @description Computes the Fowlkes-Mallows index
#' @param clusters1 A vector of labels
#' @param clusters2 A vector of labels
#' @return A numeric value
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' fowlkesmallows(out_fd1$cluster, labels)
#' }
fowlkesmallows <- function(clusters1, clusters2) {
    # Get the number of clusters
    k1 <- length(unique(clusters1))
    k2 <- length(unique(clusters2))
    # Get the number of elements in each cluster
    n1 <- table(clusters1)
    n2 <- table(clusters2)
    # Get the order of the clusters
    order1 <- order(n1, decreasing = TRUE)
    order2 <- order(n2, decreasing = TRUE)
    # Reorder the labels
    clusters1 <- reorder_labels(clusters1)
    clusters2 <- reorder_labels(clusters2)
    # Compute the Fowlkes-Mallows index
    fm <- 0
    for (i in 1:k1) {
        for (j in 1:k2) {
            fm <- fm + sum(clusters1 == i & clusters2 == j) / sqrt(sum(clusters1 == i) * sum(clusters2 == j))
        }
    }
    fm <- fm / (k1 * k2)
    return(fm)
}
