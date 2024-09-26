# Remove everything from environment
rm(list = ls())

# Package name
package.names <- c("data.table", "dplyr")

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
expenditures.data <- data.table(read.csv("Data/Consumption_expenditures.csv"))
individual.data <- data.table(read.csv("Data/Individual_data.csv"))
#-------------------------------------------------------------------------------
# Exploratory data analysis for expenditures data

# Columns to leave (household identification id and only large groups of products are left)
col.names <- c("hh_ident", colnames(expenditures.data)[nchar(colnames(expenditures.data)) == 4])
expenditures.data <- expenditures.data[, ..col.names]

# 1)
# Checking for duplicates of household id
dupl <- expenditures.data[, .N, by = hh_ident] %>%
  .[N > 1, ]
dupl # No duplicates (good)

# 2)
# Checking for NA values
na.count <- expenditures.data[, lapply(.SD, function(x) sum(is.na(x)))] # No NA values are observed (good)

# 3)
# Histograms for each categorie
num_cols <- ncol(expenditures.data[, -1])
par(mfrow = c(ceiling(num_cols/4), 4)) 

# Looping through each column and plotting a histogram
for (col in colnames(expenditures.data[, -1])) {
  hist(expenditures.data[[col]], 
       main = paste("Histogram of", col), 
       xlab = col, 
       col = "lightblue", 
       border = "black")
}

# Resetting to default plotting layout
par(mfrow = c(1, 1))

# 4)
# Box plots for each categorie
for (col in colnames(expenditures.data[, -1])) {
  # Store the box plot for each category in the list
  boxplot(expenditures.data[[col]], 
          main = paste("Boxplot of", col),
          xlab = col, 
          col="lightblue", 
          border="black")
}





 

