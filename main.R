# Remove everything from environment
rm(list = ls())

# Package name
package.names <- c("data.table", "dplyr", "readxl", "ggplot2", "randomForest", "reshape2", "caret", "tidyverse",
                   "factoextra", "cluster", "dbscan", "miscset", "cowplot")

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
# Summary statistics for each category nad total expenditures
summ.stats <- data.table(summary(expenditures.data[, -c("hh_ident")]))

# 6)
# Correlation between categories
cor.result <- cor(expenditures.data[, -c("hh_ident", "All_household_consumption_expenses_(monthly)"), with = FALSE], use = "complete.obs")
# Checking correlation - there's no multicollinearity exceeding 50%.
findCorrelation(cor(expenditures.data[, -c("hh_ident", "All_household_consumption_expenses_(monthly)")]), cutoff = .5)

# 7)
expenditures.prop = copy(expenditures.data)
summary(expenditures.data)
# Proportions of expenses
for (col in colnames(expenditures.prop[, -c("hh_ident", "All_household_consumption_expenses_(monthly)")])) {
  expenditures.prop[, (paste0(col, "_prop")) := get(col)/`All_household_consumption_expenses_(monthly)`]
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
kmeans.res <- kmeans(scale(expenditures.data[, -c("hh_ident", "All_household_consumption_expenses_(monthly)")]), centers = k)
expenditures.data.clust <- copy(expenditures.data)
expenditures.data.clust[, cluster := kmeans.res$cluster]
 
# Visualizing clusters
pca.result <- prcomp(scale(expenditures.data[, -c("hh_ident", "All_household_consumption_expenses_(monthly)")]), center = TRUE, scale. = TRUE)
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
                                               `objectid`,
                                               `Namų_ūkio_nario_eilės_Nr.`))

individ <- as.data.frame(individ)
# Which columns should be changed to factors
factor_cols <- c("education", "employment", "gender", "employment_type",
                 "job_contract", "status_in_house",
                 "marital")

individ <- individ %>%
  mutate(across(factor_cols, as.factor))

factor_levels <- lapply(individ[factor_cols], levels)
factor_levels

# Changing factor level 8 to 0. Means "Not applicable". Note - not the same as missing
levels(individ$education) <- c(1, 2, 3, 0)
levels(individ$employment) <- c(1, 2, 3, 4, 5, 6, 0)
levels(individ$employment_type) <- c(1, 2, 3, 4, 0)
levels(individ$job_contract) <- c(1, 2, 0)

#-------------------------------------------------------------------------------
individ.copy <- copy(individ)

title_map <- list(
  "All_household_consumption_expenses_(monthly)" = "Total expenses",
  "Food_and_non-alcoholic_beverages" = "Food",
  "Alcoholic_beverages,_tobacco,_and_drugs" = "Alcohol, tobacco",
  "Education_services" = "Education",
  "Restaurants_and_accommodation_services" = "Hospitality",
  "Insurance_and_financial_services" = "Financial services",
  "Personal_care,_social_protection,_and_miscellaneous_goods_and_services" = "Personal care",
  "Clothing_and_footwear" = "Apparel",
  "Housing,_water,_electricity,_gas,_and_other_fuels" = "Housing, utilities",
  "Furnishings,_household_equipment,_and_routine_home_maintenance" = "Home maintenance",
  "Health" = "Health",
  "Transport" = "Transport",
  "Information_and_communication" = "Communication",
  "Recreation,_sports,_and_culture" = "Recreation"
)

par(mar = c(2, 2, 2, 2)) 
par(mfrow = c(3, 5))

for (col in colnames(expenditures.data[, -c("hh_ident")])) {
  
  title <- ifelse(!is.null(title_map[[col]]), title_map[[col]], col)
  
  # Plot the histogram
  hist(expenditures.data[[col]], 
       main = title, 
       xlab = title, 
       col = "lightblue", 
       border = "black", 
       cex.main = 0.8) 
}
hist(individ.copy[, 9], main = "Age", 
     col = "lightblue", 
     border = "black",
     cex.main = 0.8)

# Bar plots of categorical values
mapping <- list(
  education = c(
    "1" = "Lower", 
    "2" = "Intermediate", 
    "3" = "Higher", 
    "0" = "Not applicable"
  ),
  employment = c(
    "1" = "Employed person", 
    "2" = "Unemployed", 
    "3" = "Retired", 
    "4" = "Not working due to health issues", 
    "5" = "Student", 
    "6" = "Economically inactive", 
    "0" = "Not applicable"
  ),
  gender = c(
    "1" = "Male", 
    "2" = "Female"
  ),
  employment_type = c(
    "1" = "Self-employed with employees", 
    "2" = "Self-employed without employees", 
    "3" = "Paid employee", 
    "4" = "Unpaid family business worker", 
    "0" = "Not applicable"
  ),
  job_contract = c(
    "1" = "Permanent contract", 
    "2" = "Temporary contract", 
    "0" = "Not applicable"
  ),
  status_in_house = c(
    "1" = "Head of household", 
    "2" = "Spouse/partner", 
    "3" = "Child/stepchild", 
    "4" = "Parent/parent-in-law", 
    "5" = "Other relative", 
    "6" = "Non-relative"
  ),
  marital = c(
    "1" = "Single", 
    "2" = "Married", 
    "3" = "Widowed", 
    "4" = "Divorced"
  )
)

# Function to relabel x-axis
relabel_plot <- function(plot, col) {
  plot +
    scale_x_discrete(labels = mapping[[col]]) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate for readability
}

# Function to format column names
format_column_name <- function(col) {
  gsub("_", " ", tools::toTitleCase(col))
}

# Define the specific order of columns
ordered_columns <- c("education", "gender", "marital", "employment", "employment_type", "job_contract", "status_in_house")

# Create bar plots with consistent visual dimensions
plots <- lapply(ordered_columns, function(col) {
  ggplot(individ.copy, aes_string(col)) +
    geom_bar(fill = "lightblue", color = "black", size = 0.2) +
    theme_classic() +
    xlab(format_column_name(col)) +
    ylab("Count") +
    scale_x_discrete(labels = mapping[[col]]) +
    theme(
      axis.text.x = element_text(angle = 65, hjust = 1),
      plot.margin = unit(c(1, 1, 1, 1), "cm") # Adjust margins for uniformity
    )
})

# Arrange plots in a uniform grid
cowplot::plot_grid(plotlist = plots, ncol = 4, align = "hv")
#-------------------------------------------------------------------------------
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
individ_uniq <- subset(individ_uniq, select = -c(status_in_house))

# Merge both dataframes - expenses and demographics
merger <- merge(expenditures.data, individ_uniq, by = "hh_ident")[, -c("hh_ident", "Education_services")] 
summary(merger)

# plots for report--------------------------------------------------------------
cont.var <- c("All_household_consumption_expenses_(monthly)",
              "Food_and_non-alcoholic_beverages",
              "Alcoholic_beverages,_tobacco,_and_drugs",
              "Restaurants_and_accommodation_services",
              "Insurance_and_financial_services",
              "Personal_care,_social_protection,_and_miscellaneous_goods_and_services",
              "Clothing_and_footwear",
              "Housing,_water,_electricity,_gas,_and_other_fuels",
              "Furnishings,_household_equipment,_and_routine_home_maintenance",
              "Health",
              "Transport",
              "Information_and_communication",
              "Recreation,_sports,_and_culture",
              "household_size",
              "age")

title_map <- list(
  "All_household_consumption_expenses_(monthly)" = "Total expenses",
  "Food_and_non-alcoholic_beverages" = "Food",
  "Alcoholic_beverages,_tobacco,_and_drugs" = "Alcohol, tobacco",
  "Restaurants_and_accommodation_services" = "Hospitality",
  "Insurance_and_financial_services" = "Financial services",
  "Personal_care,_social_protection,_and_miscellaneous_goods_and_services" = "Personal care",
  "Clothing_and_footwear" = "Apparel",
  "Housing,_water,_electricity,_gas,_and_other_fuels" = "Housing, utilities",
  "Furnishings,_household_equipment,_and_routine_home_maintenance" = "Home maintenance",
  "Health" = "Health",
  "Transport" = "Transport",
  "Information_and_communication" = "Communication",
  "Recreation,_sports,_and_culture" = "Recreation",
  "household_size" = "Household size",
  "age" = "Age"
)

par(mfrow = c(ceiling(length(cont.var) / 5), 5)) 


for (col in cont.var) {
  
  title <- title_map[[col]]
  
  
  if (col == "age") {
    hist(merger[[col]], 
         main = title, 
         xlab = col, 
         col = "lightblue", 
         xlim = c(0, 100)) 
  } else {
    
    max_val <- ceiling(max(merger[[col]], na.rm = TRUE)) 
    hist(merger[[col]], 
         main = title, 
         xlab = col, 
         col = "lightblue", 
         xlim = c(0, max_val)) 
  }
}

par(mfrow = c(1, 1))

# Categorical
non_cont_vars <- setdiff(names(merger), cont.var)

# Create bar plots with consistent visual dimensions
mapping <- mapping[!names(mapping) %in% "status_in_house"]
ordered_columns <- ordered_columns[ordered_columns != "status_in_house"]
plots <- lapply(ordered_columns, function(col) {
  ggplot(merger[, ..non_cont_vars], aes_string(col)) +
    geom_bar(fill = "lightblue", color = "black", size = 0.2) +
    theme_classic() +
    xlab(format_column_name(col)) +
    ylab("Count") +
    scale_x_discrete(labels = mapping[[col]]) +
    theme(
      axis.text.x = element_text(angle = 65, hjust = 1),
      plot.margin = unit(c(1, 1, 1, 1), "cm") # Adjust margins for uniformity
    )
})

# Arrange plots in a uniform grid
cowplot::plot_grid(plotlist = plots, ncol = 4, align = "hv")


# Reset plotting parameters
par(mfrow = c(1, 1))
#-------------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# 1. K-means with original data
# ----------------------------------------------------------------------------
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
k3 <- kmeans(new_data, centers = 3, nstart = 20)
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

# MEASURING IMPORTANCE WITH randoForest
k3_copy <- k3  # Create a copy of the k2 object (cluster assignment only)

# Create a copy of new_data to keep the original data intact
new_data_copy <- new_data

# Assign cluster labels to the copy of new_data (do not change original new_data)
new_data_copy$Cluster <- as.factor(k3_copy$cluster)

# Train random forest model to predict clusters based on other features
rf_model <- randomForest(Cluster ~ ., data = new_data_copy, importance = TRUE, ntree = 200)


# Extract feature importance
importance_rf <- importance(rf_model, type = 1)  # type = 2 gives Mean Decrease in Accuracy

# Print the importance to check
print(importance_rf)

importance_df <- as.data.frame(importance_rf)  # Convert importance to data frame
importance_df$Feature <- rownames(importance_df)  # Add feature names as a new column

# Now, importance_df should have 'Feature' and 'MeanDecreaseGini' columns
colnames(importance_df) <- c("MeanDecreaseAccuracy", "Feature")  # Rename columns for clarity

importance_df <- importance_df[order(importance_df$MeanDecreaseAccuracy, decreasing = TRUE), ]


# Define groups of related dummy variables
variable_groups <- list(
  "education" = c("education.1", "education.2", "education.3"),
  "employment" = c("employment.1", "employment.2", "employment.3", "employment.4", "employment.5", "employment.6"),
  "gender" = c("gender.1", "gender.2"),
  "employment_type" = c("employment_type.1", "employment_type.2", "employment_type.3", "employment_type.0"),
  "job_contract" = c("job_contract.1", "job_contract.2", "job_contract.0"),
  "marital" = c("marital.1", "marital.2", "marital.3", "marital.4")
)
# Sum importances for grouped variables
summed_importance <- importance_df %>%
  rowwise() %>%
  mutate(Feature = case_when(
    Feature %in% variable_groups$education ~ "education",
    Feature %in% variable_groups$employment ~ "employment",
    Feature %in% variable_groups$gender ~ "gender",
    Feature %in% variable_groups$employment_type ~ "employment_type",
    Feature %in% variable_groups$job_contract ~ "job_contract",
    Feature %in% variable_groups$marital ~ "marital",
    TRUE ~ Feature # Keep other variables as their own group
  )) %>%
  group_by(Feature) %>%
  summarise(MeanDecreaseAccuracy = sum(MeanDecreaseAccuracy)) %>%
  ungroup()
# Sort importance by 'MeanDecreaseAccuracy'
summed_importance <- summed_importance[order(summed_importance$MeanDecreaseAccuracy, decreasing = TRUE), ]
# Plotting feature importance
ggplot(summed_importance, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance for K-means Clusters (Random Forest)",
       x = "Features",
       y = "Importance (Mean Decrease in Accuracy)") +
  theme_minimal()


# # Plotting feature importance
# ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   coord_flip() +
#   labs(title = "Feature Importance for K-means Clusters (Random Forest)",
#        x = "Features",
#        y = "Importance (Mean Decrease in Accuracy)") +
#   theme_minimal()

#  Choose the top N important features (e.g., top 5 features)
top_features <- rownames(importance_df)[1:12]  # Modify this number as needed (e.g., top 5 features)

# Add cluster labels to the original data (or copy)
new_data_copy$Cluster <- as.factor(k3_copy$cluster)

# Filter the data to include only the top important features and Cluster
data_for_boxplots <- new_data_copy[, c(top_features, "Cluster")]

# Reshape the data into a long format for ggplot (melt the data)
data_long <- melt(data_for_boxplots, id.vars = "Cluster", variable.name = "Feature", value.name = "Value")

#  Create boxplots for the top features, grouped by cluster
ggplot(data_long, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  facet_wrap(~ Feature, scales = "free_y") +  # Separate boxplots for each feature
  labs(title = "Boxplots of Top Features by K-means Clusters",
       x = "Cluster",
       y = "Feature Value") +
  theme_minimal()

  
# Extract relevant groups from variable_groups
employment_columns <- variable_groups$employment
employment_type_columns <- variable_groups$employment_type
marital_columns <- variable_groups$marital

# Ensure Cluster is treated as a factor (if not already)
new_data_copy$Cluster <- as.factor(new_data_copy$Cluster)

# Function to calculate relative ratios for a group of columns
calculate_ratios <- function(data, group_columns, cluster_column = "Cluster") {
  ratios <- data.frame(Cluster = character(),
                       Variable = character(),
                       RelativeRatio = numeric())
  
  for (cluster in levels(data[[cluster_column]])) {
    cluster_data <- data[data[[cluster_column]] == cluster, group_columns]
    category_totals <- colSums(cluster_data, na.rm = TRUE)
    total <- sum(category_totals)
    
    for (var in names(category_totals)) {
      relative_ratio <- category_totals[var] / total
      ratios <- rbind(ratios, data.frame(
        Cluster = cluster,
        Variable = var,
        RelativeRatio = relative_ratio
      ))
    }
  }
  
  return(ratios)
}

# Calculate ratios for each group
employment_ratios <- calculate_ratios(new_data_copy, employment_columns)
employment_type_ratios <- calculate_ratios(new_data_copy, employment_type_columns)
marital_ratios <- calculate_ratios(new_data_copy, marital_columns)

# Plotting function for categorical variables
plot_ratios <- function(ratios, title) {
  ggplot(ratios, aes(x = Variable, y = RelativeRatio, fill = Cluster)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = title,
         x = "Variable",
         y = "Relative Ratio") +
    theme_minimal() +
    scale_fill_discrete(name = "Cluster") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create a mapping for employment variables with their English meanings
employment_labels <- c(
  "employment.1" = "Employed person",
  "employment.2" = "Unemployed",
  "employment.3" = "Old-age or early retirement pensioner",
  "employment.4" = "Person not working due to long-term health condition",
  "employment.5" = "Student or pupil",
  "employment.6" = "Homemaker or other economically inactive person"
)

# Update the plotting function to replace variable names with labels
plot_ratios_with_labels <- function(ratios, labels, title) {
  ratios$Variable <- factor(ratios$Variable, levels = names(labels), labels = labels)
  
  ggplot(ratios, aes(x = Variable, y = RelativeRatio, fill = Cluster)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = title,
         x = "Category",
         y = "Relative Ratio") +
    theme_minimal() +
    scale_fill_discrete(name = "Cluster") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create a mapping for employment variables with their English meanings
employment_labels <- c(
  "employment.1" = "Employed",
  "employment.2" = "Unemployed",
  "employment.3" = "Pensioner (age)",
  "employment.4" = "Not working (health)",
  "employment.5" = "Student",
  "employment.6" = "Homemaker etc."
)


# Plot graphs with updated labels for employment
plot_employment <- plot_ratios_with_labels(employment_ratios, employment_labels, 
                                           "Relative Ratios of Employment Categories per Cluster")

# The employment_type and marital plots remain unchanged as they have no translations
plot_employment_type <- plot_ratios(employment_type_ratios, "Relative Ratios of Employment Types per Cluster")
plot_marital <- plot_ratios(marital_ratios, "Relative Ratios of Marital Status per Cluster")

# Display the plots
print(plot_employment)
print(plot_employment_type)
print(plot_marital)


  



# Some notes on code - in the pre-processing step near zero variance 
# could be deleted. Also, consider normalizing - scaling and centering - 
# continuous data. Look into PCA. 

# ----------------------------------------------------------------------------
# 2. K-means with original data + PCA
# ----------------------------------------------------------------------------
pca <- prcomp(new_data, center = TRUE, scale. = TRUE)
summary(pca)

var_explained <- pca$sdev^2 / sum(pca$sdev^2)

qplot(c(1:length(new_data)), var_explained) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance explained") +
  ggtitle("Scree plot") +
  ylim(0, 1)

# I'll choose first 15 PCs to explain ~72% of variance
# data_pca <- as.data.frame(-pca$x[,1:15]) # why negative sign is added?
data_pca <- as.data.frame(pca$x[,1:5])

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
columns_to_scale <- c("All_household_consumption_expenses_(monthly)",
                      "Food_and_non-alcoholic_beverages",
                      "Alcoholic_beverages,_tobacco,_and_drugs",
                      "Restaurants_and_accommodation_services",
                      "Insurance_and_financial_services",
                      "Personal_care,_social_protection,_and_miscellaneous_goods_and_services",
                      "Clothing_and_footwear",
                      "Housing,_water,_electricity,_gas,_and_other_fuels",
                      "Furnishings,_household_equipment,_and_routine_home_maintenance",
                      "Health",
                      "Transport",
                      "Information_and_communication",
                      "Recreation,_sports,_and_culture",
                      "household_size",
                      "age")

# Scale the selected columns (standardization)
merger_scaled <- copy(merger)
merger_scaled[, intersect(columns_to_scale, names(merger_scaled)) := lapply(.SD, scale), .SDcols = intersect(columns_to_scale, names(merger_scaled))]

# creating dummy variables for factors:
dummy <- dummyVars(" ~ .", data = merger_scaled)
new_data <- data.frame(predict(dummy, newdata = merger_scaled))
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
k3 <- kmeans(new_data, centers = 3, nstart = 20)
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

# highest score with 2 clusters
avg_sil_values <- map_dbl(2:15, avg_silhouette)
par(mar=c(5, 5, 5, 2))
plot(2:15, avg_sil_values, main = "Average silhouette score per cluster",
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters k",
     ylab = "Average Silhouette score")
dev.off()

# ----------------------------------------------------------------------------
# DBSCAN with original data
merger <- merge(expenditures.data, individ_uniq, by = "hh_ident")[, -c("hh_ident", "Education_services")]
dummy <- dummyVars(" ~ .", data = merger)
new_data <- data.frame(predict(dummy, newdata = merger))
constant_columns <- which(apply(new_data, 2, function(col) length(unique(col)) == 1))
constant_columns
new_data <- new_data[, -constant_columns]
summary(new_data)

# Pang Ning book 528p
# https://www.sefidian.com/2022/12/18/how-to-determine-epsilon-and-minpts-parameters-of-dbscan-clustering/
data_dim <- dim(new_data)[2]
kNNdistplot(new_data, k = data_dim + 1) 
abline(h = 600, lty=2)

db <- dbscan(new_data, eps = 600, minPts = data_dim + 1)
print(db)
# With standardized data:
fviz_cluster(db, data = new_data, stand = TRUE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())
# With not standardized data:
fviz_cluster(db, data = new_data, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())

# ----------------------------------------------------------------------------
# DBSCAN with original data + PCA 
pca <- prcomp(new_data, center = TRUE, scale. = TRUE)
summary(pca)

var_explained <- pca$sdev^2 / sum(pca$sdev^2)

qplot(c(1:length(new_data)), var_explained) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance explained") +
  ggtitle("Scree plot") +
  ylim(0, 1)

# I'll choose first 15 PCs to explain ~72% of variance
# data_pca <- as.data.frame(-pca$x[,1:15]) # why negative sign is added?
data_pca <- as.data.frame(pca$x[,1:5])

data_dim <- dim(data_pca)[2]
kNNdistplot(data_pca, k = data_dim + 1) 
abline(h = 2, lty=2)

db <- dbscan(data_pca, eps = 2, minPts = data_dim + 1)
print(db)
# With standardized data:
fviz_cluster(db, data = data_pca, stand = TRUE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())
# With not standardized data:
fviz_cluster(db, data = data_pca, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())
# ----------------------------------------------------------------------------
# DBSCAN with original data + sclaed

# Columns to standardize
columns_to_scale <- c("All_household_consumption_expenses_(monthly)",
                      "Food_and_non-alcoholic_beverages",
                      "Alcoholic_beverages,_tobacco,_and_drugs",
                      "Restaurants_and_accommodation_services",
                      "Insurance_and_financial_services",
                      "Personal_care,_social_protection,_and_miscellaneous_goods_and_services",
                      "Clothing_and_footwear",
                      "Housing,_water,_electricity,_gas,_and_other_fuels",
                      "Furnishings,_household_equipment,_and_routine_home_maintenance",
                      "Health",
                      "Transport",
                      "Information_and_communication",
                      "Recreation,_sports,_and_culture",
                      "household_size",
                      "age")

# Scale the selected columns (standardization)
merger.scaled <- copy(merger)
merger.scaled <- merger.scaled[, intersect(columns_to_scale, names(merger.scaled)) := lapply(.SD, scale), .SDcols = intersect(columns_to_scale, names(merger.scaled))]

# creating dummy variables for factors:
dummy <- dummyVars(" ~ .", data = merger.scaled)
new_data <- data.frame(predict(dummy, newdata = merger.scaled))
constant_columns <- which(apply(new_data, 2, function(col) length(unique(col)) == 1))
constant_columns
new_data <- new_data[, -constant_columns]
summary(new_data)

data_dim <- dim(new_data)[2]
kNNdistplot(new_data, k = data_dim + 1) 
abline(h = 20, lty=2)

db <- dbscan(new_data, eps = 20, minPts = data_dim + 1)
print(db)
# With standardized data:
fviz_cluster(db, data = new_data, stand = TRUE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())
# With not standardized data:
fviz_cluster(db, data = new_data, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", ggtheme = theme_classic())


