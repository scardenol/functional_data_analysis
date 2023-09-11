# Function that takes a data matrix an converts it to an fdata object with the
# names of the original functional data
#' @title Convert a matrix to an fdata object
#' @description This function takes a matrix and converts it to an fdata object
#' with the names of the original functional data.
#' @param data A matrix with the data to be converted to an fdata object.
#' @param title_and_labels A list with the names of the fdata object.
#' @param names A list with the names of the rows and columns of the data matrix.
#' @return An fdata object with the names of the original functional data.
#' @examples
#' \dontrun{
#' library(fda.usc, quietly = TRUE)
#' data(phoneme)
#' mlearn <- phoneme$learn
#' mat2fun(mlearn$data, title_and_labels = mlearn$title_and_labels) #'
#' }
#' @export
#' @author scardenol
mat2fun <- function(data, title_and_labels, names = list(row_name = "", col_name = "")) {
    # Error handling ----------------------------------------------------------
    # If data is empty, return an error
    if (is.null(data)) stop("data is empty")
    # If title_and_labels is empty, return an error
    if (is.null(title_and_labels)) stop("title_and_labels is empty")
    # If data is not a matrix, return an error
    if (!is.matrix(data)) stop("data is not a matrix")

    # Main function -----------------------------------------------------------
    # Unpack the names of the data matrix
    row_name <- names$row_name
    col_name <- names$col_name
    # Create the names for the rows and columns of the data matrix
    data_rownames <- paste(row_name, seq(1:nrow(data)), sep = "")
    data_colnames <- paste(col_name, seq(1:ncol(data)), sep = "")
    # Set the names of the data matrix
    colnames(data) <- data_colnames
    rownames(data) <- data_rownames
    # Create the argvals and rangeval for the fdata object
    argvals <- 1:ncol(data)
    rangeval <- c(head(argvals, 1), tail(argvals, 1))
    # Set the names for the fdata object (main, xlab, ylab)
    main <- title_and_labels$main
    xlab <- title_and_labels$xlab
    ylab <- title_and_labels$ylab
    # Create the list of names
    label_names <- list(main = main, xlab = xlab, ylab = ylab)

    # Convert object to fdata
    fun_data <- fdata(
        mdata = data, argvals = argvals,
        rangeval = rangeval, names = label_names
    )

    # Return the fdata object
    return(fun_data)
}
