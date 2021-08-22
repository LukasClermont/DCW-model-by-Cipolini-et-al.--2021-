#packages <- c("gridExtra","quadprog","Matrix","ggplot2", "R.matlab", "tidyverse", "moments", "ggthemes", "lubridate")
packages <- c("R.matlab", #Read Matlab file
              "quadprog",
              "ggthemes", #for ggplot
              "ggplot2",
              "tidyverse"
              ) #quadratic optimization for restiction 

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
