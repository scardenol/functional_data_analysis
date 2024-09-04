# Clear environment
rm(list = ls())

# Set working directory to wherever the script is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load custom modules
source("modules/library_manager/library_manager.R")
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
  "htmltools",
  "roahd",
  "data.table",
  "rsample"
)

# Load or Install and Load packages
load_packages(packages)

# Check if packages loaded
loaded_packages <- data.frame(
  Packages = packages,
  Loaded = packages %in% loadedNamespaces()
)
loaded_packages

# Create an empty list to store results
res_hdbscan <- data.frame(
  avg_score = numeric(0),
  iteration = numeric(0)
  )

res_distance <- data.frame()

# Experiment parameters
# Seed. 31
set.seed(31)
num_iterations <- 20
counter <- 0
iteration <- 0

# Synthetic Data
N <- 100  # rows or curves
P <- 1e2 # cols or time

grid <- seq(-1, 1, length.out = P)
grid2 <- seq(-2, 2, length.out = P)
grid3 <- seq(-0.5, 0.5, length.out = P)

C1 <- exp_cov_function(grid, alpha = 0.2, beta = 0.3)
C2 <- exp_cov_function(grid2, alpha = 0.2, beta = 0.6)
C3 <- exp_cov_function(grid2, alpha = 0.2, beta = 0.9)

centerline1 <- (grid)^2-1
centerline2 <- (grid)^2
centerline3 <- (grid)^2+1

# data_folds <- vfold_cv(data, v=iterations, repeats = 1)

# Clustering
method <- "euclidean"
minpts_search <- 2:100
K <- 2:4 # number of clusters to 

while (counter < num_iterations) {
  iteration <- iteration + 1
  message(paste("Iteration =", iteration, "\n"))
  
  tryCatch({
    #####  Defining Synthetic Data #####
    
    # sample <- analysis(get_rsplit(data_folds, index = x))
    
    m <- generate_gauss_fdata(N, centerline1, C1)
    p <- generate_gauss_fdata(N, centerline2, C2)
    q <- generate_gauss_fdata(N, centerline3, C3)
    
    data <- rbind(m,p,q)
    # Convert to functional data
    
    # fd <- fdata(sample)
    fd <- fdata(data)
  
    # # Plot
    # # m data
    # plot(fd1, col="#0072B2")
    # # p data
    # lines(fd2, col="#D55E00")
    # # Add a legend to the plot
    # legend("topleft", legend=c("Data 1", "Data 2"),
    #        col=c("#0072B2", "#D55E00"), lty = 1, cex=0.8)
    
    
    # Experimentation
    
    # 0. Run hdbscan_multiple_minPts to find the optimal minPts parameter
    opt_minpts_df <- hdbscan_multiple_minPts(fd, minPts = minpts_search, method = method)
    # Compute the optimal minPts parameter
    opt_idx <- which.max(opt_minpts_df$mean_cluster_scores)
    opt_minpts <- opt_minpts_df$minPts[opt_idx]
    
    # 1. Density-based clustering with HDBSCAN
    hdbscan_res <- hdbscan_fd(fd, minPts = opt_minpts, method = method)
    
    # 2. Distance-based clustering with: custom pam, k-means (Manhattan), k-means (Euclidean)
    k_hdbscan <- length(unique(hdbscan_res$cluster)) # number of clusters from hdbscan
    distance_res <- distance_based_clustering(fd, K = K, verbose = "minimal")
    
    # Format results into a data frame
    results_df <- results_to_df(distance_res, K = K)
    results_df$iteration <- iteration # add iteration
    
    # Store results
    # Hacer un data frame para avg_membership_prob, avg_outlier_scores
    res_hdbscan <- rbind(res_hdbscan,
                         data.frame(
                           avg_score = mean(hdbscan_res$cluster_scores),
                           iteration = iteration
                         ))
    res_distance <- rbind(res_distance, results_df)
    
    counter <- counter + 1
  },  error=function(e){
    cat("ERROR:", conditionMessage(e), "\n")
    message("\nAttempting next iteration...\n")
  }
  )
  message(paste("Successful iterations =", counter, "/", num_iterations,"\n"))
}

# Visualize results: time, connectivity, dunn and silhouette
# p_time <- plot_distance_results(results_df, result_type = "time")
# p_connectivity <- plot_distance_results(results_df, result_type = "connectivity")
# p_dunn <- plot_distance_results(results_df, result_type = "dunn")
# p_silhouette <- plot_distance_results(results_df, result_type = "silhouette")

# Extract silhouette diagram for each method from the results
# p_silhouette_kmeans <- distance_res$kmeans_res$diagram
# p_silhouette_pam <- distance_res$pam_res$diagram
# p_silhouette_kmeans_man <- distance_res$kmeans_man_res$diagram

# Boxplot
theme_set(theme_grey())

# Muestra de una iteración, falta generalizar
prob_stability <- data.frame(membership_prob=hdbscan_res$membership_prob,
                   k=hdbscan_res$cluster)
outlier_glosh <- data.frame(outlier_scores=hdbscan_res$outlier_scores,
                            k=hdbscan_res$cluster)
t0 <- ggplot(prob_stability, aes(x=factor(k), y=membership_prob)) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot(show.legend = FALSE, fill="#619CFF") +
  labs(x = "k")
t1 <- ggplot(outlier_glosh, aes(x=factor(k), y=outlier_scores)) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot(show.legend = FALSE, fill="#619CFF") +
  labs(x = "k")

################################

p1 <- ggplot(res_hdbscan, aes(y=avg_score)) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot(fill="#619CFF")


p2 <- ggplot(res_distance, aes(x=method, y=time, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Clusters k")

p3 <- ggplot(res_distance, aes(x=method, y=avg_sil, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Clusters k")

p4 <- ggplot(res_distance, aes(x=method, y=connect, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Clusters k")

p5 <- ggplot(res_distance, aes(x=method, y=di, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Clusters k")

### MISSING ###
# -[ ] generalize prob_stability, outlier_glosh
# -[ ] Motivation: what is a better hdbscan metric to optimize minPts?
# -[ ] Experiment: add a mini experiment to answer the previous question
# -[ ] title: what does the metric measure and direction of values