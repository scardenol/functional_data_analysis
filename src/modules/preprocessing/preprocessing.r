# Function to get column classes
get_column_classes <- function(df) {
  sapply(df, class)
}

# Check if the columns of the data are the same in name and class
check_columns <- function(users) {
  print("Checking the columns for all users...")
  # Get column names for all users
  users_colnames <- t(sapply(users, colnames))
  # Check if the columns names are the same
  len <- length(users) - 1
  same_colnames <- sum(duplicated(users_colnames)) == len
  # Get column class for all users
  users_colclasses <- t(t(lapply(users, get_column_classes)))
  # Check if the columns names are the same
  same_colclasesses <- sum(duplicated(users_colclasses)) == len
  if (same_colnames & same_colclasesses) {
    print("The columns are the same:")
    results <- t(t(lapply(users[[1]], mode)))
    colnames(results) <- c("class")
    print(results)
  } else {
    # Raise error
    stop("The columns are not the same")
  }
}

# Print dimensions for every user
print_dim <- function(users) {
  results <- t(as.data.frame(lapply(users, dim)))
  colnames(results) <- c("rows", "cols")
  return(results)
}

# Create a simple preprocessing function that takes the data and: drops the r_value column,
# separates the timestamp column into date and time columns, and separates the time column into hour column
preprocess <- function(data, timestamp_col = "record_timestamp", drop_col = "name") {
  # drop r_value column for now
  drop_idx <- grep(drop_col, colnames(data))
  data <- data[, -drop_idx]
  # separate timestamp column timestamp_col into "date" and "time" columns
  data <- separate(data, timestamp_col, c("date", "time"), sep = " ")
  # Supress possible warning from separate
  defaultW <- getOption("warn")
  options(warn = -1)
  # Separate "time" column into "hour" column
  data <- separate(data, "time", c("hour"), sep = ":")
  # Restore warning option
  options(warn = defaultW)
  return(data)
}

# Preprocess all users
preprocess_users <- function(users, timestamp_col = "record_timestamp", drop_col = "name") {
  for (i in seq_along(users)) {
    users[[i]] <- preprocess(users[[i]], timestamp_col, drop_col)
  }
  return(users)
}

# Filter data by start_date dplyr style (assume default date format)
filter_by_date <- function(users, start_date = "2022-09-01") {
  for (i in seq_along(users)) {
    users[[i]] <- users[[i]] %>% filter(as.Date(date, format = "%Y-%m-%d") >= start_date)
  }
  return(users)
}

# Remove users with value of 0 for 80% of all dates. The input is a list of data.frames.
remove_zero_users <- function(users, interval = c(0, 1), freq = 0.8) {
  # Get the number of rows for each user
  rows <- lapply(users, nrow)
  # Get the unique number of hours per date
  hours <- length(unique(users[[1]][["hour"]]))
  # Get the number of allowed hours to be 0
  zero_hours <- round(hours * freq)
  # Group each data.frame by date and count the number of times the value is 0 for each date
  group <- lapply(users, group_by, date)
  count <- lapply(group, summarise, count = sum(value >= interval[1] & value <= interval[2]))
  # Count the number of times the previous count is >= zero_hours but preserve the date column
  cond <- lapply(count, function(x) x$count >= zero_hours)
  # Filter each count by its corresponding condition
  count <- mapply(function(x, y) x[y, ], count, cond, SIMPLIFY = FALSE)
  # Each element of count is a tibble of date and count, remove the matching dates for each
  # corresponding user
  for (i in seq_along(count)) {
    users[[i]] <- users[[i]] %>% filter(!date %in% count[[i]][["date"]])
  }
  # Return the filtered users
  return(users)
}

# Check how many complete days of data we have for each user vs the
# total amount of days
# Create a function to check this
check_days_user <- function(user, user_name_col = "id_service") {
  # Extract the user name from the provided column
  user_name <- user[user_name_col][1, ]
  # Create table for the dates
  user_table <- table(user$date)
  # Sum the number of times the records cover an entire day (24-hour records)
  complete_days <- sum(user_table == 24)
  # Sum the total number recorded of days (whole or not)
  days <- length(table(user$date))
  # Return the results as a data frame to be append multiple results
  return(as.data.frame(list(
    "user" = user_name,
    "num_24h_days" = complete_days,
    "total_days" = days
  )))
}

# Apply the check_days_user function for a list of users
check_days <- function(users, user_name_col = "id_service") {
  days_df <- data.frame()
  for (i in seq_along(users)) {
    days_df <- rbind(days_df, check_days_user(users[[i]], user_name_col))
  }
  return(days_df)
}

# Filter users by matching dates
matching_dates <- function(users, days_df, verbose = TRUE) {
  # Find the user with the least number of 24-hour recorded days
  idx <- which.min(days_df[, 2])
  # Users have different lengths, so we filter by the user idx
  dates <- names(table(users[[idx]][["date"]]))
  # Check which dates match the user idx dates and are 24h
  cond <- list()
  for (i in seq_along(users)) {
    cond[[i]] <- table(users[[i]][["date"]])[dates] == 24
  }
  # Check row wise if all users match on each date
  matching_dates_table <- Reduce(`&`, cond)
  # Count the number of days where user have matching dates for 24-hour records
  if (verbose) {
    print("# days where users have matching dates for 24h records")
    print(table(matching_dates_table == TRUE))
  }
  # Get the dates that match for all users
  matching_dates <- names(matching_dates_table[matching_dates_table == TRUE])

  # Filter every user by the dates that match for all users
  for (i in seq_along(users)) {
    users[[i]] <- users[[i]] %>% filter(date %in% matching_dates)
  }
  # Pack list with results
  results <- list(users = users, dates = matching_dates, table = matching_dates_table)
  return(results)
}

# Create function to convert data into an fdata object with its labels
data_to_fd <- function(data, id_col = "id_service") {
  # Initialize variables
  curves <- c()
  clusters <- c()
  col_names <- unique(data$hour)
  dates <- c()
  i <- 1

  for (current_date in unique(data$date)) {
    # Filter data by current date
    data_by_date <- data %>% filter(date == current_date)
    # Get current user (cluster)
    current_user <- unique(data_by_date[id_col])
    # Store the value vector as the current curve
    curve <- t(data_by_date$value)
    # Set column names
    colnames(curve) <- col_names
    # Store the value for the curve in the curves matrix, fill with NA if there
    # are missing hours
    curves <- rbind.fill(
      as.data.frame(curves),
      as.data.frame(curve)
    )
    # Store date of current curve in the dates vector
    dates <- rbind(dates, unique(data_by_date$date))
    # Store the current user in a cluster vector
    clusters <- rbind(clusters, c(current_user))
    i <- i + 1
  }
  # Create fdata object arguments
  argvals <- 1:ncol(curves)
  rangeval <- c(head(argvals, 1), tail(argvals, 1))
  main <- "Daily Energy consumption on 2022"
  xlab <- "Hour"
  ylab <- "Active energy (kWh)"
  names <- list(
    main = main,
    xlab = xlab,
    ylab = ylab
  )

  # Convert to fdata
  fun_data <-
    fdata(
      mdata = curves,
      argvals = argvals,
      rangeval = rangeval,
      names = names
    )

  # Return fdata object and labels
  return(list(fun_data = fun_data, cluster = clusters))
}

# Convert each user's data to an fdata object with its labels
df_to_fdata <- function(users, id_col = "id_service") {
  for (i in seq_along(users)) {
    users[[i]] <- data_to_fd(users[[i]], id_col)
  }
  return(users)
}

# Create a function to bind a list of fdata objects into a single fdata object and
# a list of labels into a single vector of labels
bind_fdata <- function(user_results) {
  # Initialize variables
  data <- c()
  clusters <- c()
  # Iterate through each user's resulting list of (fdata, cluster)
  for (user in user_results) {
    # Extract the fdata and cluster from the list
    user_fd <- user$fun_data
    user_cluster <- user$cluster
    # Bind the data from the fdata object to the data matrix
    data <- rbind(data, user_fd$data)
    # Bind the cluster to the clusters vector
    clusters <- rbind(clusters, user_cluster)
  }
  # Create fdata object arguments
  # The arguments are the same for every user, so we can just use the first user's
  # fdata object
  argvals <- user_results[[1]]$fun_data$argvals
  rangeval <- user_results[[1]]$fun_data$rangeval
  names <- user_results[[1]]$fun_data$names
  # Convert to fdata
  fun_data <-
    fdata(
      mdata = data,
      argvals = argvals,
      rangeval = rangeval,
      names = names
    )
  # Create a vector of users as the current clusters
  users <- clusters
  # Convert the clusters to numeric integer sequence starting from 1
  clusters <- as.numeric(as.factor(unlist(clusters)))
  # Return fdata object and labels
  return(list(
    fun_data = fun_data,
    cluster = clusters,
    users = users
  ))
}

# Create a function that takes an fdata$data object and converts it to long format
fdata_to_long <- function(data, str_dates = dates) {
  # Number of columns of fun_data$data and number of dates
  cols <- ncol(data) # number of columns of the fdata object
  ndates <- length(str_dates) # number of dates

  # Convert fdata to long format
  users_lf <- as.data.frame(data)
  users_lf <- users_lf %>% gather(Hour, Value)
  # Count number of resulting rows
  rows <- nrow(users_lf)
  ncurves <- rows / cols # number of curves to plot
  # Add date column
  users_lf$date <- unlist(unlist(do.call(rbind, replicate(rows / ndates, as.data.frame(str_dates), simplify = FALSE))))
  # We need to creat a label column that would act as a curve index
  users_lf$label <- unlist(unlist(do.call(rbind, replicate(cols, as.data.frame(1:ncurves), simplify = FALSE))))

  return(users_lf)
}

# Create a function that scales data with standarization row-wise
scale_data <- function(data) {
  # Compute mean and sd row wise
  rm <- apply(data, 1, mean)
  rs <- apply(data, 1, sd)
  # Standarize data per row
  data <- sweep(sweep(data, 1, rm, "-"), 1, rs, "/")
  # Create a list with the data, row means, and row sd
  results <- list(
    scaled_data = data,
    rm = rm,
    rs = rs
  )
  return(results)
}

# Create a function that unscales data with standarization row-wise
unscale_data <- function(data, mean = rm, sd = rs) {
  # Unscale data per row
  data <- sweep(sweep(data, 1, rs, "*"), 1, rm, "+")
  return(data)
}

# Create a function that computes the deepest or most central curve and creates a band by adding and
# substracting a single standard deviation. The default curves and upper and lower curves are
# : mean, mean + sd, mean - sd
get_deepest_band <- function(data) {
  # Compute the mean curve (mean column wise)
  mean_curve <- apply(data, 2, mean)
  # Compute the standard deviation curve (sd column wise)
  sd_curve <- apply(data, 2, sd)
  # Compute the upper curve
  upper_curve <- mean_curve + sd_curve
  # Compute the lower curve
  lower_curve <- mean_curve - sd_curve
  # Create a list with the mean, upper and lower curves
  results <- list(
    mean = mean_curve,
    upper = upper_curve,
    lower = lower_curve
  )
  return(results)
}

# Create a function that computes the deepest band for each user.
# It takes as input the fdata$data data and the users vector of labels.
get_deepest_bands <- function(data, users) {
  # Get the unique users id
  ids <- unlist(unique(users))
  # Initialize variables
  i <- 1 # Counter for naming
  results <- list() # List to store results
  # Iterate through each user
  for (id in ids) {
    # Find which rows of users are equal to the current id
    idx <- which(users == id)
    # Compute the deepest band for the current user
    deepest_band <- get_deepest_band(data[idx, ])
    # Set the current user name based on the counter
    name <- paste("user", i, sep = "")
    # Add the index of the current user to the deepest_band list
    deepest_band[["curves_idx"]] <- idx
    # Add the user name to the deepest_band list
    deepest_band[["user"]] <- id
    # Add the deepest band to the results list
    results[[name]] <- deepest_band
    # Increment counter
    i <- i + 1
  }
  return(results)
}

# Create a function that filters the data by the deepest bands.
# The functions takes the data, the deepest bands, and the freq percentage.
# The freq percentage is the percentage of the time the data must be within the deepest band.
# The default freq percentage is 0.75, which means we aim for the 25% deepest curves.
# The function returns the indexes of the rows that are within the deepest bands.
filter_data <- function(data, deepest_bands, freq = 0.75) {
  # Initialize variables
  results <- list(filtered_data = c(), curves_idx = c())
  # Iterate through each deepest band
  for (db in deepest_bands) {
    # Unpack the upper and lower curves, and the curves_idx
    upper <- db$upper
    lower <- db$lower
    curves_idx <- db$curves_idx
    # Filter data by the curves_idx
    filtered_data <- data[curves_idx, ]
    # row-wise sum the number of times filtered_data >= lower and <= upper
    cond <- apply(filtered_data, 1, function(x) sum(x >= lower & x <= upper))
    # Convert the freq percentage to a number of rows
    freq_rows <- round(length(upper) * freq)
    # Get the indexes of the rows that are >= freq_rows
    temp_idx <- which(cond >= freq_rows)
    # Compute the indexes of the original data using curves_idx
    idx <- curves_idx[temp_idx]
    # Filter data by the indexes
    filtered_data <- data[idx, ]
    # Add the filtered_data and indexes to the results list
    results$filtered_data <- rbind(results$filtered_data, filtered_data)
    results$curves_idx <- c(results$curves_idx, idx)
  }
  return(results)
}

# Function to format filtered long format data
format_filtered_long_data <- function(data_lf, group_by = users_id, filter_by = filter_idx) {
  # Copy long format data
  users_lf <- data_lf
  # Get number of original columns
  cols <- length(unique(users_lf$Hour))
  # Add group id column
  users_lf$id <- unlist(do.call(rbind, replicate(cols, as.data.frame(group_by), simplify = FALSE)))
  # Filter long format data
  users_lf <- users_lf[users_lf$label %in% filter_by, ] # filter long format data
  return(users_lf)
}
