#### Preface ####
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
res_minpts <- data.frame()

res_hdbscan <- data.frame(
  mean_cluster_scores = numeric(0),
  iteration = numeric(0)
  )

res_distance <- data.frame()

#### Experiment ####
# Start timing
start.time <- Sys.time()
# Experiment parameters

# Set directory for output
dir_name <- "experiment1"

# Random seed
set.seed(31)

# Iterations
num_iterations <- 100

# Initialize loop variables
counter <- 0
iteration <- 0

# Synthetic Data
N <- 100  # rows or curves
P <- 1e2 # cols or time

grid <- seq(-1, 1, length.out = P)
grid2 <- seq(-2, 2, length.out = P)
grid3 <- seq(-3, 3, length.out = P)

C1 <- exp_cov_function(grid, alpha = 0.2, beta = 0.3)
C2 <- exp_cov_function(grid2, alpha = 0.2, beta = 0.6)
C3 <- exp_cov_function(grid2, alpha = 0.2, beta = 0.9)

# {x^2-1, x^2, x^2+1}
centerline1 <- (grid)^2-1
centerline2 <- (grid)^2
centerline3 <- (grid)^2+1

# data_folds <- vfold_cv(data, v=iterations, repeats = 1)

# Clustering
method <- "euclidean"
minpts_search <- 2:100
K <- 2:4 # number of clusters 

while (counter < num_iterations) {
  iteration <- iteration + 1
  message(paste("Iteration =", iteration, "\n"))
  
  tryCatch({
    #####  Defining Synthetic Data #####
    
    # sample <- analysis(get_rsplit(data_folds, index = x))
    
    m <- generate_gauss_fdata(N, centerline1, C1)
    p <- generate_gauss_fdata(N, centerline2, C2)
    q <- generate_gauss_fdata(N, centerline3, C3)
    
    # Try to create an fdata object
    # If there's an error, this would throw an error faster and skip
    # a bad iteration of the cycle
    test <- c(fdata(m), fdata(p), fdata(q))
    
    data <- rbind(m,p,q)
    # Convert to functional data
    
    # fd <- fdata(sample)
    fd <- fdata(data)
    
    
    # Experimentation
    message("Running density-based algorithms...")
    # 0. Run hdbscan_multiple_minPts to find the optimal minPts parameter
    opt_minpts_df <- hdbscan_multiple_minPts(fd, minPts = minpts_search, method = method)
    # Compute the optimal minPts parameter
    opt_idx <- which.max(opt_minpts_df$mean_cluster_scores)
    opt_minpts <- opt_minpts_df$minPts[opt_idx]
    
    # 1. Density-based clustering with HDBSCAN
    hdbscan_res <- hdbscan_fd(fd, minPts = opt_minpts, method = method)
    
    message("Running distance-based algorithms...")
    # 2. Distance-based clustering with: custom pam, k-means (Manhattan), k-means (Euclidean)
    k_hdbscan <- length(unique(hdbscan_res$cluster)) # number of clusters from hdbscan
    distance_res <- distance_based_clustering(fd, K = K, verbose = FALSE)
    
    message("Storing results...")
    # Format results into a data frame
    results_df <- results_to_df(distance_res, K = K)
    results_df$iteration <- iteration # add iteration
    
    # Store results
    # Hacer un data frame para avg_membership_prob, avg_outlier_scores
    opt_minpts_df$iteration <- iteration
    res_minpts <- rbind(res_minpts, opt_minpts_df)
    res_hdbscan <- rbind(res_hdbscan,
                         data.frame(
                           mean_cluster_scores = mean(hdbscan_res$cluster_scores),
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

# End time
end.time <- Sys.time()

#### Extract results ####

# Plot the synthetic data
# m data
plot(fdata(m), col="#0072B2", ylim=c(-3,3), main='')
# p data
lines(fdata(p), col="#D55E00")
# q data
lines(fdata(q), col="#FF3E00")
# Add a legend to the plot
legend("topleft", legend=c("Data 1", "Data 2", "Data 3"),
       col=c("#0072B2", "#D55E00", "#FF3E00"), lty = 1, cex=0.8)
p_synthetic <- recordPlot()

formatted_data <- cbind.data.frame(
  curve = c(rep("m",P), rep("p",P), rep("q",P)),
  time = c(col(data)),
  value = c(data))
ggplot(formatted_data, aes(x=time, y=value, color=curve)) +
  geom_line(aes(group=interaction(time, curve)))

# General results for hdbscan
gm_df <- select(
  res_minpts,
  "mean_cluster_scores",
  "mean_membership_prob",
  "mean_outlier_scores")

# Melt to 2D for easier plotting
gm_df <- reshape2::melt(gm_df, id.vars=NULL)

# Optimal results for hdbscan per iteration
# Max mean_cluster_scores
opt_cs <- res_minpts[, c("mean_cluster_scores", "iteration", "num_clusters")] %>%
  group_by(iteration) %>%
  top_n(1, mean_cluster_scores) %>%
  as.data.frame()

# Max mean_membership_prob
opt_mp <- res_minpts[, c("mean_membership_prob", "iteration", "num_clusters")] %>%
  group_by(iteration) %>%
  top_n(1, mean_membership_prob) %>%
  as.data.frame()

# Min mean_outlier_scores
opt_os <- res_minpts[, c("mean_outlier_scores", "iteration", "num_clusters")] %>%
  group_by(iteration) %>%
  top_n(1, mean_outlier_scores) %>%
  as.data.frame()

# Bind column-wise based on the iteration column
opt_df <- cbind(
  opt_cs[, c("iteration", "mean_cluster_scores")],
  mean_membership_prob=opt_mp$mean_membership_prob,
  mean_outlier_scores=opt_os$mean_outlier_scores
)
# Melt into a 2D data frame using the iteration column as id
opt_df_melted <- reshape2::melt(opt_df, id.vars=1)
# Add iteration column
opt_df_melted$iteration <- rep(opt_df$iteration, 3)
 
# Number of clusters data frame
clusters_df <- data.frame(
  metric=c(rep("mean_cluster_score", num_iterations),
           rep("mean_membership_prob", num_iterations),
           rep("mean_outlier_scores", num_iterations)),
  num_clusters=c(opt_cs$num_clusters,
                 opt_mp$num_clusters,
                 opt_os$num_clusters)
  )

# Boxplot
theme_set(theme_grey())

# Metric values overall
p_gm <- ggplot(gm_df, aes(y=value, fill=variable)) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  labs(fill="metric",
       title="HDBSCAN internal metrics")

# Optimal values for metrics per iteration
p_im <- ggplot(opt_df_melted, aes(y=value, fill=variable)) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  labs(fill="metric",
       title="HDBSCAN internal metrics optimal values")

# Cluster proportion per metric over all iterations
p_cp <- ggplot(clusters_df, aes(x=factor(num_clusters), group=metric, fill=metric)) +
  geom_bar(aes(y = ..prop..),
           position = position_dodge()) +
  labs(title="HDBSCAN frequency of resulting clusters")

################################


p_t <- ggplot(res_distance, aes(x=method, y=time, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Cluster k",
       title="Time for each for each method",
       subtitle="Lower is better")

p_si <- ggplot(res_distance, aes(x=method, y=avg_sil, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Cluster k",
       title="Sillhouette index for each method",
       subtitle="Cluster definition (separation, compactness, and cohession) [-1,1], higher is better")

p_ci <- ggplot(res_distance, aes(x=method, y=connect, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Clusters k",
       title="Connectivity index for each method",
       subtitle="Cluster cohesion [0,∞), higher is better")

p_di <- ggplot(res_distance, aes(x=method, y=di, fill=factor(k))) +
  stat_boxplot(geom ='errorbar', coef=NULL) +
  geom_boxplot() +
  facet_wrap(~k) +
  labs(fill="Clusters k",
       title="Dunn index for each method",
       subtitle="Cluster separation [0,∞], higher is better")

#### Export plot results ####

plot_list <- list(
  p_synthetic= p_synthetic,
  p_gm = p_gm,
  p_im = p_im,
  p_cp = p_cp,
  p_t = p_t,
  p_si = p_si,
  p_ci = p_ci,
  p_di = p_di
)

# Save the plot_list as an R object
write_object(plot_list, file_name = "plot_list", dir_name = dir_name)

# Read the plot_list from the R object
plot_list <- readRDS(paste(dir_name, "/plot_list.RDS", sep = ""))

# Save each plot from plot_list as an interactive html file
write_html_plots(plot_list, dir_name = dir_name, width = 1300, height = 600)

### MISSING ###
# -[x] generalize prob_stability, outlier_glosh
# -[x] Motivation: what is a better hdbscan metric to optimize minPts?
# -[x] Experiment: add a mini experiment to answer the previous question
# -[x] title: make plots pretty
# -[x] export: refactor the code to output the plot results to a dir

time.taken <- difftime(end.time, start.time, units='mins')
message(paste("Execution time =", time.taken[[1]], "min"))