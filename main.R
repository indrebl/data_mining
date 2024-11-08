# Remove everything from environment
rm(list = ls())

# Package name
package.names <- c("data.table", "dplyr", "readxl")

# Loading packages
for (pkg_name in package.names) {
  if (!require(pkg_name, character.only = TRUE)) {
    install.packages(pkg_name)
    library(pkg_name, character.only = TRUE)
  } else {
    library(pkg_name, character.only = TRUE)
  }
}

# Sourcing functions file
source("functions.R")

rm(pkg_name, package.names)
#-------------------------------------------------------------------------------
# Loading data
expenditures.data <- data.table(read.csv("Data/Consumption_expenditures.csv"))
individual.data <- data.table(read.csv("Data/Individual_data.csv"))

# Loading explanations
expenditures.data.expl <- data.table(read_excel("Data/Consumption_expenditures_expl.xlsx"))
individual.data.expl <- data.table(read_excel("Data/Individual_data_expl.xlsx"))

# Modifications
individual.data.expl$`Kintamojo pavadinimas` <- tolower(individual.data.expl$`Kintamojo pavadinimas`)

expenditures.data.expl$`Kintamojo aprašymas` <- gsub(" ", "_", expenditures.data.expl$`Kintamojo aprašymas`)
setnames(expenditures.data.expl, c("Kintamojo pavadinimas", "Kintamojo aprašymas"), c("Var_name", "Var_expl"))

individual.data.expl1 <- na.omit(individual.data.expl[, 1:2])
individual.data.expl1$`Kintamojo aprašymas` <- gsub(" ", "_", individual.data.expl1$`Kintamojo aprašymas`)
setnames(individual.data.expl1, c("Kintamojo pavadinimas", "Kintamojo aprašymas"), c("Var_name", "Var_expl"))

individual.data.expl2 <- individual.data.expl[!is.na(individual.data.expl$`Kintamojo varianto aprašymas`), c(1, 3:4)]
individual.data.expl2$`Kintamojo varianto aprašymas` <- gsub(" ", "_", individual.data.expl2$`Kintamojo varianto aprašymas`)
setnames(individual.data.expl2, c("Kintamojo pavadinimas", "Kintamojo variantų kodas", "Kintamojo varianto aprašymas"), 
                                c("Var_name", "Code", "Var_expl"))
individual.data.expl2[, Var_name := Var_name[!is.na(Var_name)][cumsum(!is.na(Var_name))]]

# Columns to leave from expenditures data (household identification id and only large groups of products are left)
col.names <- c("hh_ident", colnames(expenditures.data)[nchar(colnames(expenditures.data)) == 4])
expenditures.data <- expenditures.data[, ..col.names]

# Adding explanations to expenditures data
# Checking whether we have explanations for all columns
setdiff(colnames(expenditures.data), expenditures.data.expl$Var_name) # No differences, meaning we have all necessary variable names

col.names.old <- colnames(expenditures.data)
colnames.expl <- expenditures.data.expl[Var_name %in% col.names.old]
setnames(expenditures.data, colnames.expl$Var_name, colnames.expl$Var_expl)

# Adding explanations to individual data
# Checking whether we have explanations for all columns
setdiff(colnames(individual.data), individual.data.expl1$Var_name) # No differences, meaning we have all necessary variable names (object id is just observation number)

# Changing observations values 
# Don't change, keep for factors

# for (var in unique(individual.data.expl2$Var_name)) {
#   expl_subset <- individual.data.expl2[Var_name == var]
#   case_when_logic <- create_case_when(expl_subset, var)
#   individual.data <- individual.data %>%
#     mutate(!!var := eval(parse(text = case_when_logic)))
# }

col.names.old <- colnames(individual.data)
colnames.expl <- individual.data.expl1[Var_name %in% col.names.old]
setnames(individual.data, colnames.expl$Var_name, colnames.expl$Var_expl)
#-------------------------------------------------------------------------------
# Exploratory data analysis for expenditures data

# 1)
# Checking for duplicates of household id
setnames(expenditures.data, "Namų_ūkio_identifikatorius", "hh_ident")

dupl <- expenditures.data[, .N, by = hh_ident] %>%
  .[N > 1, ]
dupl # No duplicates (good)

# 2)
# Checking for NA values
na.count <- expenditures.data[, lapply(.SD, function(x) sum(is.na(x)))]
na.count # No NA values are observed (good)

# 3)
# Histograms for each categorie
par(mar = c(2, 2, 2, 2)) 
par(mfrow = c(3, 5))

for (col in colnames(expenditures.data[, -c("hh_ident")])) {
  hist(expenditures.data[[col]], 
       main = col, 
       xlab = col, 
       col = "lightblue", 
       border = "black",
       cex.main = 0.8)
}


selected_cols <- colnames(expenditures.data)[!colnames(expenditures.data) %in% "hh_ident"][1:14]

zero_counts <- expenditures.data[, lapply(.SD, function(x) sum(x == 0)), .SDcols = selected_cols]

# Calculate total counts for the first 15 columns
total_counts <- nrow(expenditures.data)

# Convert zero counts to a data frame
zero_counts_df <- data.frame(Category = names(zero_counts), 
                             Zero_Counts = unlist(zero_counts)
)

# Calculate percentage of zero values
zero_counts_df$Percentage_Zeros <- (zero_counts_df$Zero_Counts / total_counts) * 100
print(zero_counts_df)

#=======================
# Near zero-variance. According to this, Svietimo paslaugos could be removed due to near zero variance
library(caret)
nzv = nearZeroVar(expenditures.data[, -1], saveMetrics = TRUE)
nzv

#=======================#=======================TRUE

# 4)
# Box plots for each category
par(mar = c(2, 2, 2, 2)) 
par(mfrow = c(3, 5))

for (col in colnames(expenditures.data[, -c("hh_ident")])) {
  # Store the box plot for each category in the list
  boxplot(expenditures.data[[col]], 
          main = col,
          xlab = col, 
          col="lightblue", 
          border="black",
          cex.main = 0.8)
}

# For most categories, expenditures are concentrated in a lower range.
# However, there are a lot of outliers (households that have unusually high expenditures)

# 5)
# Summary statistics for each categorie nad total expenditures
summ.stats <- data.table(summary(expenditures.data[, -c("hh_ident")]))

# 6)
# Correlation between categories
cor.result <- cor(expenditures.data[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)"), with = FALSE], use = "complete.obs")
# Checking correlation - there's no multicollinearity exceeding 50%.
findCorrelation(cor(expenditures.data[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)")]), cutoff = .5)

# 7)
# Proportions of expenses
# for (col in colnames(expenditures.data[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)")])) {
#   expenditures.data[, (paste0(col, "_prop")) := get(col)/`Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)`]
# }
# 
# par(mar = c(2, 2, 2, 2)) 
# par(mfrow = c(3, 5))
# 
# prop.col.names <- grep("prop$", colnames(expenditures.data), value = TRUE)
# for (col in prop.col.names) {
#   hist(expenditures.data[[col]], 
#        main = col, 
#        xlab = col, 
#        col = "lightblue", 
#        border = "black",
#        cex.main = 0.8)
# }
#### Why is it changed to proportions? Looks like a mistake

# 8)
# Grouping households based on their spending habits
set.seed(123)
k <- 2  # 2 clusters selected
# scale function standardizes data (mean is 0, st dev is 1)
kmeans.res <- kmeans(scale(expenditures.data[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)")]), centers = k)
expenditures.data.clust <- copy(expenditures.data)
expenditures.data.clust[, cluster := kmeans.res$cluster]
 
# Visualizing clusters
pca.result <- prcomp(scale(expenditures.data[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)")]), center = TRUE, scale. = TRUE)
# Extracting first two principal components
pca.data <- data.frame(PC1 = pca.result$x[, 1], PC2 = pca.result$x[, 2])
# Adding cluster information
pca.data$cluster <- factor(expenditures.data.clust$cluster)

colors <- c("firebrick1", "cornflowerblue","darkolivegreen2", "darkblue", "seagreen4")
cluster.colors <- colors[as.numeric(pca.data$cluster)]

par(mfrow = c(1, 1))
plot(pca.data$PC1, pca.data$PC2, 
     col = cluster.colors, 
     pch = 19, 
     xlab = "Principal Component 1", 
     ylab = "Principal Component 2", 
     main = "K-Means Clustering of Households (3 Clusters)")

legend("topright", legend = levels(pca.data$cluster), 
       col = colors, pch = 19, title = "Cluster")










