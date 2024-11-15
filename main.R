# Remove everything from environment
rm(list = ls())

# Package name
package.names <- c("data.table", "dplyr", "readxl", "ggplot2", "randomForest")

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
findCorrelation(cor(expenditures.data[, -c("hh_ident", "Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)")]), cutoff = .5)

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

# Renaming for easier access
orig_names = colnames(copy(individ))
setnames(individ, colnames(individ), c("education", "employment", "hh_ident",
                                       "eiles_nr", "gender", "employment_type",
                                       "job_contract", "status_in_house",
                                       "marital", "age"))

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

# Reduce the df even more, because some columns now seem redundant
individ_uniq <- subset(individ_uniq, select = -c(eiles_nr, status_in_house))

# Merge both dataframes - expenses and demographics
merger <- merge(expenditures.data, individ_uniq, by = "hh_ident")
summary(merger)

# Reduced data
merger.reduced <- merger[, .(`Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)`,
                      education,
                      employment,
                      gender,
                      employment_type,
                      job_contract,
                      marital,
                      age,
                      household_size)]

# ----------------------------------------------------------------------------
# 1. K-means with original data
# ----------------------------------------------------------------------------
merger <- merger[, -c("hh_ident")]
# creating dummy variables for factors:
dummy <- dummyVars(" ~ .", data = merger)
new_data <- data.frame(predict(dummy, newdata = merger))
constant_columns <- which(apply(new_data, 2, function(col) length(unique(col)) == 1))
constant_columns
new_data <- new_data[, -constant_columns]
summary(new_data)

# Finding optimal number of clusters
# Seems that the optimal number of clusters is 2 (before column hh_ident was not excluded optimal number of clusters was 3). 
# Silhouette
fviz_nbclust(new_data, kmeans, method = "silhouette")
# Within sum of squares - Elbow method
fviz_nbclust(new_data, kmeans, method = "wss")
# Gap statistic - did not converge for me, takes a really long time
# fviz_nbclust(new_data, kmeans, method = "gap_stat")

# Perform K-means
set.seed(0)
k2 = kmeans(new_data, centers = 2, nstart = 20)
# Visualize clusters
fviz_cluster(k2, data = new_data)

# Evaluate clusters
# Cluster sizes
k2$size

# Silhouette score
ss <- silhouette(k2$cluster, dist(new_data))
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
clust_result$kmeans2 <- k2$cluster
cluster1 <- clust_result[clust_result$kmeans2 == 1,]
cluster2 <- clust_result[clust_result$kmeans2 == 2,]

# MEASURING IMPORTANCE WITH randoForest
k2_copy <- k2  # Create a copy of the k2 object (cluster assignment only)

# Create a copy of new_data to keep the original data intact
new_data_copy <- new_data

# Assign cluster labels to the copy of new_data (do not change original new_data)
new_data_copy$Cluster <- as.factor(k2_copy$cluster)

# Train random forest model to predict clusters based on other features
rf_model <- randomForest(Cluster ~ ., data = new_data_copy, importance = TRUE, ntree = 200)

# Extract feature importance
importance_rf <- importance(rf_model, type = 2)  # type = 2 gives Mean Decrease in Accuracy

# Print the importance to check
print(importance_rf)

importance_df <- as.data.frame(importance_rf)  # Convert importance to data frame
importance_df$Feature <- rownames(importance_df)  # Add feature names as a new column

# Now, importance_df should have 'Feature' and 'MeanDecreaseGini' columns
colnames(importance_df) <- c("MeanDecreaseGini", "Feature")  # Rename columns for clarity

# Sort importance by 'MeanDecreaseGini'
importance_df <- importance_df[order(importance_df$MeanDecreaseGini, decreasing = TRUE), ]


# Plotting feature importance
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance for K-means Clusters (Random Forest)",
       x = "Features",
       y = "Importance (Mean Decrease in Gini)") +
  theme_minimal()



#  Choose the top N important features (e.g., top 5 features)
top_features <- rownames(importance_df)[1:6]  # Modify this number as needed (e.g., top 5 features)

# Add cluster labels to the original data (or copy)
new_data_copy$Cluster <- as.factor(k2_copy$cluster)

# Filter the data to include only the top important features and Cluster
data_for_boxplots <- new_data_copy[, c(top_features, "Cluster")]

# Reshape the data into a long format for ggplot (melt the data)
library(reshape2)
data_long <- melt(data_for_boxplots, id.vars = "Cluster", variable.name = "Feature", value.name = "Value")

#  Create boxplots for the top features, grouped by cluster
ggplot(data_long, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  facet_wrap(~ Feature, scales = "free_y") +  # Separate boxplots for each feature
  labs(title = "Boxplots of Top Features by K-means Clusters",
       x = "Cluster",
       y = "Feature Value") +
  theme_minimal()




# Some notes on code - in the pre-processing step near zero variance 
# could be deleted. Also, consider normalizing - scaling and centering - 
# continuous data. Look into PCA. 

# ----------------------------------------------------------------------------
# 2. K-means with original data + PCA
# ----------------------------------------------------------------------------
pca <- prcomp(new_data, center = TRUE, scale. = TRUE)
summary(pca)

var_explained <- pca$sdev^2 / sum(pca$sdev^2)

qplot(c(1:38), var_explained) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance explained") +
  ggtitle("Scree plot") +
  ylim(0, 1)

# I'll choose first 15 PCs to explain ~72% of variance
# data_pca <- as.data.frame(-pca$x[,1:15]) # why negative sign is added?
data_pca <- as.data.frame(pca$x[,1:15])

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
fviz_pca_ind(pca, habillage = k2_pca$cluster, label = "none", addEllipses = TRUE)

k3_pca <- kmeans(data_pca, centers = 3, nstart = 20)
fviz_cluster(k3_pca, data = data_pca)
fviz_pca_ind(pca, habillage = k3_pca$cluster, label = "none", addEllipses = TRUE)

# Evaluate clusters
k2_pca$size
k3_pca$size

# K-means results seem to be worse with PCA than without.
ss_pca2 <- silhouette(k2_pca$cluster, dist(data_pca))
mean(ss_pca2[,3])

ss_pca3 <- silhouette(k3_pca$cluster, dist(data_pca))
mean(ss_pca3[,3])

# ----------------------------------------------------------------------------
# 3. K-means with original data + standardization
# ----------------------------------------------------------------------------

# Columns to standardize
columns_to_scale <- c("Visos_namų_ūkio_vartojimo_išlaidos_(mėnesinės)",
                      "Maistas_ir_nealkoholiniai_gėrimai",
                      "Alkoholiniai_gėrimai,_tabakas_ir_narkotikai",
                      "Švietimo_paslaugos",
                      "Restoranai_ir_apgyvendinimo_paslaugos",
                      "Draudimas_ir_finansinės_paslaugos",
                      "Asmens_priežiūra,_socialinė_apsauga_ir_įvairios_prekės_ir_paslaugos",
                      "Apranga_ir_avalynė",
                      "Būstas,_vanduo,_elektra,_dujos_ir_kitas_kuras",
                      "Būsto_apstatymo,_namų_ūkio_įranga_ir_kasdienė_namų_priežiūra",
                      "Sveikata",
                      "Transportas",
                      "Informacija_ir_ryšiai",
                      "Poilsis,_sportas_ir_kultūra",
                      "household_size",
                      "age")

# Scale the selected columns (standardization)
merger[, intersect(columns_to_scale, names(merger)) := lapply(.SD, scale), .SDcols = intersect(columns_to_scale, names(merger))]

# creating dummy variables for factors:
dummy <- dummyVars(" ~ .", data = merger)
new_data <- data.frame(predict(dummy, newdata = merger))
constant_columns <- which(apply(new_data, 2, function(col) length(unique(col)) == 1))
constant_columns
new_data <- new_data[, -constant_columns]
summary(new_data)

# Finding optimal number of clusters
# Seems that the optimal number of clusters is 2. 
# Silhouette
fviz_nbclust(new_data, kmeans, method = "silhouette")
# Within sum of squares - Elbow method
fviz_nbclust(new_data, kmeans, method = "wss")
# Gap statistic - did not converge for me, takes a really long time
# fviz_nbclust(new_data, kmeans, method = "gap_stat")

# Perform K-means
set.seed(0)
k2 = kmeans(new_data, centers = 2, nstart = 20)
# Visualize clusters
fviz_cluster(k2, data = new_data)

# Evaluate clusters
# Cluster sizes
k2$size

# Silhouette score
ss <- silhouette(k2$cluster, dist(new_data))
mean(ss[,3])

# Average silhouette score for different number of clusters
avg_silhouette <- function(k){
  km <- kmeans(new_data, centers = k, nstart = 20)
  ss <- silhouette(km$cluster, dist(new_data))
  return(mean(ss[,3]))
}

# highest score with 2 clusters
avg_sil_values <- map_dbl(2:15, avg_silhouette)
par(mar=c(5, 5, 5, 2))
plot(2:15, avg_sil_values, main = "Average silhouette score per cluster",
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters k",
     ylab = "Average Silhouette score")
dev.off()

# ----------------------------------------------------------------------------
# 4. K-means with reduced data
# ----------------------------------------------------------------------------
# creating dummy variables for factors:
dummy <- dummyVars(" ~ .", data = merger.reduced)
new_data <- data.frame(predict(dummy, newdata = merger.reduced))
constant_columns <- which(apply(new_data, 2, function(col) length(unique(col)) == 1))
constant_columns
new_data <- new_data[, -constant_columns]
summary(new_data)

# Finding optimal number of clusters
# Seems that the optimal number of clusters is 2. 
# Silhouette
fviz_nbclust(new_data, kmeans, method = "silhouette")
# Within sum of squares - Elbow method
fviz_nbclust(new_data, kmeans, method = "wss")
# Gap statistic - did not converge for me, takes a really long time
# fviz_nbclust(new_data, kmeans, method = "gap_stat")

# Perform K-means
set.seed(0)
k2 = kmeans(new_data, centers = 2, nstart = 20)
# Visualize clusters
fviz_cluster(k2, data = new_data)

# Evaluate clusters
# Cluster sizes
k2$size

# Silhouette score
ss <- silhouette(k2$cluster, dist(new_data))
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

# ----------------------------------------------------------------------------
# 5. K-means with reduced data + PCA
# ----------------------------------------------------------------------------
pca <- prcomp(new_data, center = TRUE, scale. = TRUE)
summary(pca)

var_explained <- pca$sdev^2 / sum(pca$sdev^2)

qplot(c(1:length(var_explained)), var_explained) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance explained") +
  ggtitle("Scree plot") +
  ylim(0, 1)

# I'll choose first 16 PCs to explain ~99% of variance
data_pca <- as.data.frame(pca$x[,1:16])

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
fviz_pca_ind(pca, habillage = k2_pca$cluster, label = "none", addEllipses = TRUE)

k3_pca <- kmeans(data_pca, centers = 3, nstart = 20)
fviz_cluster(k3_pca, data = data_pca)
fviz_pca_ind(pca, habillage = k3_pca$cluster, label = "none", addEllipses = TRUE)

# Evaluate clusters
k2_pca$size
k3_pca$size

# K-means results seem to be worse with PCA than without.
ss_pca2 <- silhouette(k2_pca$cluster, dist(data_pca))
mean(ss_pca2[,3])

ss_pca3 <- silhouette(k3_pca$cluster, dist(data_pca))
mean(ss_pca3[,3])

# ----------------------------------------------------------------------------
# 6. K-means with reduced data + standardization
# ----------------------------------------------------------------------------
# Scale the selected columns (standardization)
merger.reduced[, intersect(columns_to_scale, names(merger.reduced)) := lapply(.SD, scale), .SDcols = intersect(columns_to_scale, names(merger.reduced))]

# creating dummy variables for factors:
dummy <- dummyVars(" ~ .", data = merger.reduced)
new_data <- data.frame(predict(dummy, newdata = merger.reduced))
constant_columns <- which(apply(new_data, 2, function(col) length(unique(col)) == 1))
constant_columns
new_data <- new_data[, -constant_columns]
summary(new_data)

# Finding optimal number of clusters
# Seems that the optimal number of clusters is 2. 
# Silhouette
fviz_nbclust(new_data, kmeans, method = "silhouette")
# Within sum of squares - Elbow method
fviz_nbclust(new_data, kmeans, method = "wss")
# Gap statistic - did not converge for me, takes a really long time
# fviz_nbclust(new_data, kmeans, method = "gap_stat")

# Perform K-means
set.seed(0)
k2 = kmeans(new_data, centers = 2, nstart = 20)
# Visualize clusters
fviz_cluster(k2, data = new_data)

# Evaluate clusters
# Cluster sizes
k2$size

# Silhouette score
ss <- silhouette(k2$cluster, dist(new_data))
mean(ss[,3])

# Average silhouette score for different number of clusters
avg_silhouette <- function(k){
  km <- kmeans(new_data, centers = k, nstart = 20)
  ss <- silhouette(km$cluster, dist(new_data))
  return(mean(ss[,3]))
}

# highest score with 2 clusters
avg_sil_values <- map_dbl(2:15, avg_silhouette)
par(mar=c(5, 5, 5, 2))
plot(2:15, avg_sil_values, main = "Average silhouette score per cluster",
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters k",
     ylab = "Average Silhouette score")
dev.off()

#-------------------------------------------------------------------------------















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


