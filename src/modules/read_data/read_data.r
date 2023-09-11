# ---- Read input files ----
# Read multiple users (input) data excel files. The files come from the same source,
# which means:
# - They are identically stored
# - They require the same configuration
#______________________________________________________________________________

# Take a list of excel file names, read each file and store them in a list
read_files <- function(file_names, file_path, col_range = NULL) {
  # Initialize file list and counter
  files <- list()
  i <- 0
  for (file in file_names) {
    i <- i + 1
    name <- paste("user", i, sep = "")
    files[[name]] <- read_excel(path = paste(file_path, file, sep = ""),
                                range = col_range)
  }
  # Return named list of files
  return(files)
}

#______________________________________________________________________________