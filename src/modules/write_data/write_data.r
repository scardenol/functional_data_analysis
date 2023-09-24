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

# Function that takes a list of plots, and creates one html file with all the plots instead of one html file per plot
write_html_plots_one_file <- function(
    plot_list, dir_name = NULL, file_name = NULL, width = 1300,
    height = 600) {
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
    # If file_name is not provided, use the name of the passed plot_list
    if (is.null(file_name)) {
        file_name <- deparse(substitute(plot_list))
    }
    # Use the name of the passed plot_list as the filename for the html file
    filename <- deparse(substitute(plot_list))
    # Convert each ggplot object to plotly object with width and height arguments
    plotly_list <- lapply(plot_list, function(p) ggplotly(p, width = 1300, height = 600))
    # Create a list of divs for each plotly object
    div_list <- lapply(plotly_list, function(p) div(p, style = "display:inline-block"))
    # Combine all divs into a single HTML file
    html_file <- htmltools::tagList(div_list)
    # Save the html file
    save_html(html_file, file = paste(dir_name, file_name, ".html", sep = ""))
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
        # Update counter
        counter <- counter + 1
        # if plot element is a list call the write_html_plots_one_file function and continue
        if (!is.ggplot(plot_list[[counter]])) {
            file_name <- paste(counter, "_", plot, sep = "")
            write_html_plots_one_file(plot_list[[counter]], dir_name = dir_name, file_name = file_name, width = width, height = height)
            next
        }
        # Adjust height and width of the plot
        converted_plotly <- ggplotly(plot_list[[plot]], width = width, height = height)
        plotly_widget <- as_widget(converted_plotly)
        save_html(plotly_widget, file = paste(dir_name, counter, "_", plot, ".html", sep = ""))
    }
}
