#' @title: Function to reorder the clusters such tha the largest cluster is 1 and so on.
#' @param clusters: A vector with the cluster labels.
#' @return: A vector with the reordered cluster labels.
#' @export
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' labels <- phoneme$classlearn
#' out_fd1 <- kmeans.fd(mlearn, ncl = 3, draw = FALSE)
#' reorder_labels(out_fd1$cluster)
#' }
#' @author scardenol
reorder_labels <- function(clusters) {
    labels <- unique(clusters)
    # Get the number of clusters
    k <- length(unique(clusters))
    # Get the number of elements in each cluster
    n <- table(clusters)
    # Get the order of the clusters
    order <- order(n, decreasing = TRUE)
    # Reorder the labels
    labels[clusters == order[1]] <- 1
    for (i in 2:k) {
        labels[clusters == order[i]] <- i
    }
    return(labels)
}
