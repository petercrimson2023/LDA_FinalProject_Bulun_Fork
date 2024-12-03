load("data.RData")
library(tidyverse)
library(lme4)
library(nlme)
library(mice)

# Clean data and create groups first
clean_data <- data %>%
  # First reclassify Race
  mutate(
    Race = case_when(
      Race == "Black/African American" ~ "Black",
      Race == "White" ~ "White",
      TRUE ~ "Other"  # All other categories combined
    ),
    # Convert Race to factor with specific reference level
    Race = factor(Race, levels = c("Black", "White", "Other")),
    # Then clean Visit
    Visit = case_when(
      Visit == "Enrollment" ~ "Month0",
      Visit == "Run-in FU Randomization" ~ "Randomization",
      Visit == "Month 6 Visit" ~ "Month6",
      Visit == "Month 12 Visit" ~ "Month12", 
      Visit == "Month 18 Visit" ~ "Month18",
      Visit == "Month 24 Visit" ~ "Month24",
      Visit == "Month 30 Visit" ~ "Month30"
    ),
    # Create AL groups
    AL_group = cut(
      AL,
      breaks = c(-Inf, 24.5, 26, 27.5, Inf),
      labels = c("Group1", "Group2", "Group3", "Group4"),
      include.lowest = TRUE
    )
  )

# Split into groups first
group1_data <- clean_data %>% filter(AL_group == "Group1")
group2_data <- clean_data %>% filter(AL_group == "Group2")
group3_data <- clean_data %>% filter(AL_group == "Group3")

# Function to prepare data for imputation
prepare_for_imputation <- function(data) {
  wide_data <- data %>%
    select(PtID, Visit, AL, TrtGroup, MotherMyop, FatherMyop, Sex, Race, EyeColor, AgeAsofEnrollDt, genetic, Age) %>%
    pivot_wider(
      id_cols = c(PtID, TrtGroup, MotherMyop, FatherMyop, Sex, Race, EyeColor, AgeAsofEnrollDt, genetic, Age),
      names_from = Visit,
      values_from = AL,
      names_prefix = "AL_"
    ) %>% 
    select(-AL_Randomization)
  return(wide_data)
}

# Function to set up and run imputation
run_imputation <- function(wide_data) {
  # Create prediction matrix
  pred_mat <- make.predictorMatrix(wide_data)
  
  # Modify prediction matrix for temporal ordering
  pred_mat["AL_Month0", c("AL_Month6","AL_Month12","AL_Month18","AL_Month24","AL_Month30")] <- 0
  pred_mat["AL_Month6", c("AL_Month12","AL_Month18","AL_Month24","AL_Month30")] <- 0
  pred_mat["AL_Month12", c("AL_Month18","AL_Month24","AL_Month30")] <- 0
  pred_mat["AL_Month18", c("AL_Month24","AL_Month30")] <- 0
  pred_mat["AL_Month24", "AL_Month30"] <- 0
  
  # Set imputation methods
  meth <- make.method(wide_data)
  meth[grep("AL_", names(meth))] <- "pmm"
  
  # Perform imputation
  imp <- mice(wide_data, 
              m = 20,
              maxit = 20,
              method = meth,
              predictorMatrix = pred_mat,
              seed = 123)
  
  return(imp)
}

# Function to convert wide to long format
to_long <- function(data) {
  data %>%
    pivot_longer(
      cols = starts_with("AL_"),
      names_to = "Visit",
      values_to = "AL",
      names_prefix = "AL_"
    ) %>%
    mutate(
      Visit_numeric = case_when(
        Visit == "Month0" ~ 0,
        Visit == "Month6" ~ 6,
        Visit == "Month12" ~ 12,
        Visit == "Month18" ~ 18,
        Visit == "Month24" ~ 24,
        Visit == "Month30" ~ 30
      )
    )
}

# Process each group separately
process_group <- function(group_data, group_name) {
  print(paste("Processing", group_name))
  
  # Prepare data for imputation
  wide_data <- prepare_for_imputation(group_data)
  
  # Run imputation
  imp <- run_imputation(wide_data)
  
  # Store models for this group
  group_models <- vector("list", 20)
  
  # Fit models for each imputed dataset
  for(i in 1:20) {
    imp_data <- complete(imp, i)
    long_data <- to_long(imp_data)
    
    tryCatch({
      group_models[[i]] <- lmer(AL ~ Visit_numeric * as.factor(genetic) * TrtGroup + Sex +
                                  Race + EyeColor + AgeAsofEnrollDt +
                                  (Visit_numeric | PtID),
                                data = long_data,
                                na.action = na.omit)
    }, error = function(e) {
      print(paste("Error in", group_name, "imputation", i, ":", e$message))
      return(NULL)
    })
  }
  
  return(group_models)
}

# Process all groups
models_group1 <- process_group(group1_data, "Group 1")
models_group2 <- process_group(group2_data, "Group 2")
models_group3 <- process_group(group3_data, "Group 3")

# Combine models into list structure for existing extract_group_results function
all_models <- list()
for(i in 1:5) {
  all_models[[i]] <- list(
    group1 = models_group1[[i]],
    group2 = models_group2[[i]],
    group3 = models_group3[[i]]
  )
}

# Get results for each group
results_group1 <- extract_group_results(all_models, "group1")
results_group2 <- extract_group_results(all_models, "group2")
results_group3 <- extract_group_results(all_models, "group3")

# Combine all results
all_results <- rbind(results_group1, results_group2, results_group3)

# Print results
print(all_results, digits = 3)

# Create visualization
ggplot(all_results %>%
         filter(term %in% c("TrtGroup", "Visit_numeric", "genetic", 
                            "Visit_numeric:genetic")) %>%
         mutate(term = factor(term, 
                              levels = c("TrtGroup", "Visit_numeric", 
                                         "genetic", "Visit_numeric:genetic"))), 
       aes(x = group, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Key Coefficient Estimates by AL Group",
       x = "AL Group",
       y = "Estimate with 95% CI",
       color = "Parameter")