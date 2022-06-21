### src.R: basic functions that all scripts will require

# Define folders for data, plotting, source files, and scripts
require("folderfun")
setff("Data", "data/")
setff("Plots", "plots/")
setff("Src", "src/")
setff("Scripts", "scripts/")

# Install bandit R package
require("devtools")
devtools::install_github('Nth-iteration-labs/contextual')
library(contextual)

# Standardized plot dimensions
save_png = function(filename, ...){
    png(filename, width=1280, height=768, res=120, ...)
}
