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
hist(individ[, 9], main = "Age", 
     col = "lightblue", 
     border = "black",
     cex.main = 0.8)

library(miscset)
ggplotGrid(ncol = 4,
           lapply(colnames(individ[, -c(3, 9)]),
                  function(col) {
                    ggplot(individ, aes_string(col)) + 
                      geom_bar(fill = "lightblue", color = "black", size=0.2) +
                      theme_classic()
                  }))


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

# ----------------------------------------------------------------------------
# Near zero-variance. According to this, Svietimo paslaugos could be removed due to near zero variance
library(caret)
nzv = nearZeroVar(expenditures.data[, -1], saveMetrics = TRUE)
nzv

# ----------------------------------------------------------------------------

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
<<<<<<< Updated upstream
findCorrelation(cor(expenditures.data[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)")]), cutoff = .5)
=======
findCorrelation(cor(expenditures.data[, -c("hh_ident")]))
library(corrplot)
copy_names = colnames(expenditures.data[, -"hh_ident"])
copy_names
short_name_vec = c("Total expenses", "Food", "Alcohol, tobacco", "Education", "Hospitality", "Financial services",
                   "Personal care", "Apparel", "Housing, utilities", "Home maintenance", "Health", "Transport",
                   "Communication", "Recreation")
exp_copy = copy(expenditures.data[, -"hh_ident"])
setnames(exp_copy, colnames(exp_copy), short_name_vec)
exp_copy
cor_exp = cor(exp_copy)
cor_exp
dev.off()
corrplot(cor_exp, "square", "lower")


par(mar = c(2, 2, 2, 2)) 
par(mfrow = c(3, 5))

for (col in colnames(exp_copy)) {
  hist(exp_copy[[col]], 
       main = col, 
       xlab = col, 
       col = "lightblue", 
       border = "black",
       cex.main = 1.2)
}
hist(individ[, 9], main = "Age", 
     col = "lightblue", 
     border = "black",
     cex.main = 1.2)
>>>>>>> Stashed changes

# 7)
expenditures.prop = copy(expenditures.data)
summary(expenditures.data)
# Proportions of expenses
for (col in colnames(expenditures.prop[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)")])) {
  expenditures.prop[, (paste0(col, "_prop")) := get(col)/`Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)`]
}

par(mar = c(2, 2, 2, 2))
par(mfrow = c(3, 5))

prop.col.names <- grep("prop$", colnames(expenditures.prop), value = TRUE)
for (col in prop.col.names) {
  hist(expenditures.prop[[col]],
       main = col,
       xlab = col,
       col = "lightblue",
       border = "black",
       cex.main = 0.8)
}

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


# ----------------------------------------------------------------------------
# Some more data pre-processing
library(tidyverse)
library(factoextra)
library(cluster)

# Working on individuals
# Remove unnecessary columns, convert to factors
individ <- subset(individual.data, select = -c(`Motinos_gimimo_šalis`,
                                               `Tėvo_(įtėvio,_patėvio,_globėjo)_asmens_numeris`,
                                               `Motinos_(įmotės,_pamotės,_globėjos)_asmens_numeris`,
                                               `Gimimo_šalis`,
                                               `Sutuoktinio_(-ės),_sugyventinio_(-ės)_asmens_numeris`,
                                               `Tėvo_gimimo_šalis`,
                                               `Pagrindinės_pilietybės_šalis`,
                                               `Gyvena_su_partneriu`,
                                               `Šiuo_metu_mokosi`,
                                               `Dabartinis_formaliojo_švietimo_ar_mokymo_veiklos_lygis`,
                                               `Pagrindinis_darbas:_profesija`,
                                               `objectid`))

individ <- as.data.frame(individ)

<<<<<<< Updated upstream
# Renaming for easier access
orig_names = colnames(copy(individ))
setnames(individ, colnames(individ), c("education", "employment", "hh_ident",
                                       "eiles_nr", "gender", "employment_type",
                                       "job_contract", "status_in_house",
                                       "marital", "age"))

=======
summary(individ)
>>>>>>> Stashed changes
# Which columns should be changed to factors
factor_cols <- c("education", "employment", "gender", "employment_type",
                 "job_contract", "status_in_house",
                 "marital")

individ = individ %>%
  mutate(across(factor_cols, as.factor))

factor_levels = lapply(individ[factor_cols], levels)
factor_levels

# Changing factor level 8 to 0. Means "Not applicable". Note - not the same as missing
levels(individ$education) <- c(1, 2, 3, 0)
levels(individ$employment) <- c(1, 2, 3, 4, 5, 6, 0)
levels(individ$employment_type) <- c(1, 2, 3, 4, 0)
levels(individ$job_contract) <- c(1, 2, 0)


# There are multiple subjects from one household, but there is only one line for expenses
# Creating a new variable "household_size" to denote how many subjects participated from the same 
# household. This will ~roughly~ be the num of dependents. 
# If multiple subjects under one household - only the head of household will be left

individ <- individ %>%
  group_by(hh_ident) %>%
  mutate(household_size = n()) %>%
  ungroup()
summary(individ)

# Filter unique hh_ident, if not unique, keep only status_in_house = 1 (head)

individ_uniq <- individ %>%
  group_by(hh_ident) %>%
  filter(n() == 1 | status_in_house == 1) %>%
  slice(1) %>%
  ungroup()

individ %>% summarise(count = n_distinct(hh_ident))
individ_uniq %>% summarise(count = n_distinct(hh_ident))
summary(individ_uniq)       

# not sure if situation is better to keep only the first appearances in unique hh_ident
summary(individ %>% distinct(hh_ident, .keep_all = TRUE))

# Need to figure out the best approach

# Reduce the df even more, because some columns now seem redundant
individ_uniq <- subset(individ_uniq, select = -c(eiles_nr, status_in_house))

# Merge both dataframes - expenses and demographics
merger <- merge(expenditures.data, individ_uniq, by = "hh_ident")
summary(merger)


# ----------------------------------------------------------------------------
# K-means with original data

# creating dummy variables for factors:
dummy <- dummyVars(" ~ .", data = merger)
new_data <- data.frame(predict(dummy, newdata = merger))
constant_columns <- which(apply(new_data, 2, function(col) length(unique(col)) == 1))
constant_columns
new_data <- new_data[, -constant_columns]
summary(new_data)

# Finding optimal number of clusters
# Seems that the optimal number of clusters is 3. 
# Silhouette
fviz_nbclust(new_data, kmeans, method = "silhouette")
# Within sum of squares - Elbow method
fviz_nbclust(new_data, kmeans, method = "wss")
# Gap statistic - did not converge for me, takes a really long time
# fviz_nbclust(new_data, kmeans, method = "gap_stat")

# Perform K-means
set.seed(0)
k3 = kmeans(new_data, centers = 3, nstart = 20)
# Visualize clusters
fviz_cluster(k3, data = new_data)

# Evaluate clusters
# Cluster sizes
k3$size

# Silhouette score
ss <- silhouette(k3$cluster, dist(new_data))
mean(ss[,3])

# Average silhouette score for different number of clusters
avg_silhouette <- function(k){
  km <- kmeans(new_data, centers = k, nstart = 20)
  ss <- silhouette(km$cluster, dist(new_data))
  return(mean(ss[,3]))
}

# Could try clustering with 2 or 4, but that would yield worse ss

avg_sil_values <- map_dbl(2:15, avg_silhouette)
par(mar=c(5, 5, 5, 2))
plot(2:15, avg_sil_values, main = "Average silhouette score per cluster",
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters k",
     ylab = "Average Silhouette score")
dev.off()

# Investigate the differences, try to come up with cluster profile
clust_result <- copy(new_data)
clust_result$kmeans3 <- k3$cluster
cluster1 <- clust_result[clust_result$kmeans3 == 1,]
cluster2 <- clust_result[clust_result$kmeans3 == 2,]
cluster3 <- clust_result[clust_result$kmeans3 == 3,]

# Some notes on code - in the pre-processing step near zero variance 
# could be deleted. Also, consider normalizing - scaling and centering - 
# continuous data. Look into PCA. 

# ----------------------------------------------------------------------------
# DBSCAN with original data
library(dbscan)
kNNdistplot(new_data, k = 5)
abline(h = 550, lty=2)

db <- dbscan(new_data, eps = 550, minPts = 5)
print(db)
# With standardized data:
fviz_cluster(db, data = new_data, stand = TRUE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())
# With not standardized data:
fviz_cluster(db, data = new_data, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())
?fviz_cluster

clust_result$dbscan2 <- db$cluster

# ----------------------------------------------------------------------------
# set up PCA
# Question - what to do with categorical variables? Are there any challenges 
# with one-hot encoded data?

pca <- prcomp(new_data, center = TRUE, scale. = TRUE)
summary(pca)

var_explained <- pca$sdev^2 / sum(pca$sdev^2)

qplot(c(1:39), var_explained) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance explained") +
  ggtitle("Scree plot") +
  ylim(0, 1)

# I'll choose first 15 PCs to explain ~72% of variance
data_pca <- as.data.frame(-pca$x[,1:15])

# Choose optimal number of clusters
# 2 or 3
fviz_nbclust(data_pca, kmeans, method = "wss")
# 2
fviz_nbclust(data_pca, kmeans, method = "silhouette")

# ----------------------------------------------------------------------------
# Perform K-means with pca transformed data
set.seed(0)
k2_pca <- kmeans(data_pca, centers = 2, nstart = 20)
fviz_cluster(k2_pca, data = data_pca)

k3_pca <- kmeans(data_pca, centers = 3, nstart = 20)
fviz_cluster(k3_pca, data = data_pca)

# Evaluate clusters
k2_pca$size
k3_pca$size

# K-means results seem to be worse with PCA than without.
ss_pca2 <- silhouette(k2_pca$cluster, dist(data_pca))
mean(ss_pca2[,3])

ss_pca3 <- silhouette(k3_pca$cluster, dist(data_pca))
mean(ss_pca3[,3])

