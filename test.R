library(tidyverse)

source("main.R")

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
                                               `Dabartinis_formaliojo_švietimo_ar_mokymo_veiklos_lygis`))

individ <- as.data.frame(individ)

# Renaming for easier access
orig_names = colnames(individ)
setnames(individ, colnames(individ), c("educ", "employment", "hh_ident",
                                       "eiles_nr", "gender", "empl_status",
                                       "job_contract", "status_in_house",
                                       "marital", "prof", "objectid", "age"))

# Which columns should be changed to factors
factor_cols <- c("educ", "employment", "gender", "empl_status",
                "job_contract", "status_in_house",
                "marital", "prof")

individ = individ %>%
  mutate(across(factor_cols, as.factor))

factor_levels = lapply(individ[factor_cols], levels)
factor_levels

# Changing factor level 8 to 0. Means "Not applicable". Note - not the same as missing
levels(individ$educ) <- c(1, 2, 3, 0)
levels(individ$employment) <- c(1, 2, 3, 4, 5, 6, 0)
levels(individ$empl_status) <- c(1, 2, 3, 4, 0)
levels(individ$job_contract) <- c(1, 2, 0)
levels(individ$prof) <- c(1, 2, 3, 4, 5, 6, 7, 8, 0, 99)
individ$prof[individ$prof == 99] = NA
individ$prof = droplevels(individ$prof)


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
# But then according to employment status there are only retired ppl :D 
# On the other hand, contracts are normal :D 

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
