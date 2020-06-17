library("devtools")
library("roxygen2")
library("Rtools")

# make a package directory
setwd("C:/Users/me/surfdrive/R-packages")

# make the package directory
# create("CanvasBSTAT")

# make the documentation
getwd()
setwd("CanvasBSTAT")
document()

# install the package
setwd("..")
install("CanvasBSTAT")

# to githup
# install_github('CanvasBSTAT','github_username')
