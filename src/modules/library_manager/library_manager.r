# ---- Handle packages ----
#______________________________________________________________________________

# Load or install and load a package
load_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load or install and load a list of packages
load_packages <- function(packages) {
  temp <- lapply(packages, load_package)
}

#______________________________________________________________________________