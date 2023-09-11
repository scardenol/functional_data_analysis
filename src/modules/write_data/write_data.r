# Function that takes and R object and saves it as a RDS file in a directory
write_object <- function(object, file_name = NULL, dir_name = NULL) {
    # If object is not provided, stop the function
    if (missing(object)) stop("Please provide an object to save.")
    # If the file name is not provided, use the object name
    if (is.null(file_name)) {
        file_name <- deparse(substitute(object))
    }
    # If the directory name is not provided, use the current directory
    dir_name <- paste0(dir_name, "/", sep = "")
    # Create a directory if it doesn't exist
    if (!dir.exists(dir_name)) {
        dir.create(dir_name)
    }

    # Save the object as a RDS file
    saveRDS(object, file = paste0(dir_name, file_name, ".RDS"))
}

# Iterate over the a list of plots and save each plot as an html file.
# Please provide a width and height a little bit bigger smaller than screen resolution.
write_html_plots <- function(plot_list, dir_name = NULL, width = 1300, height = 600) {
    # If plot_list is not provided, stop the function
    if (missing(plot_list)) stop("Please provide a list of plots to save.")
    # If the directory name is not provided, use the current directory
    dir_name <- paste0(dir_name, "/", sep = "")
    # Create a directory if dir_name is not null and it doesn't exist
    if (!is.null(dir_name) && !dir.exists(dir_name)) {
        dir.create(dir_name)
    }
    # Check if width and height are numeric
    if (!is.numeric(width) || !is.numeric(height)) {
        stop("Please provide a numeric width and height.")
    }
    # Initialize counter
    counter <- 0
    for (plot in names(plot_list)) {
        # if plot element is not a ggplot object, skip it
        if (!inherits(plot_list[[plot]], "gg")) {
            next
        }
        # Update counter
        counter <- counter + 1
        # Adjust height and width of the plot
        converted_plotly <- ggplotly(plot_list[[plot]], width = width, height = height)
        plotly_widget <- as_widget(converted_plotly)
        save_html(plotly_widget, file = paste(dir_name, counter, "_", plot, ".html", sep = ""))
    }
}
