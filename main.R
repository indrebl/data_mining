# Remove everything from environment
rm(list = ls())

# Package name
package.names <- c("data.table")

# Loading packages
for (pkg_name in package.names) {
  if (!require(pkg_name, character.only = TRUE)) {
    install.packages(pkg_name)
    library(pkg_name, character.only = TRUE)
  } else {
    library(pkg_name, character.only = TRUE)
  }
}

rm(pkg_name, package.names)

# Loading data
expenditures.data <- read.csv("Data/Consumption_expenditures.csv")
individual.data <- read.csv("Data/Individual_data.csv")
