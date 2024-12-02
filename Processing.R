library(dplyr)
library(ggplot2)

# Part A. Data Import and merge

# Load VisitData and PtRoster datasets
VisitData <- read.table("Data Tables/MTS1ClinicTesting.txt", 
                        header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)
head(VisitData)

PtRoster <- read.table("Data Tables/MTS1PtRoster.txt", 
                       header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)
head(PtRoster)

# Load the additional dataset for parental myopia and other demographic information
DemoMedOcuHx <- read.table("Data Tables/MTS1DemoMedOcuHx.txt", 
                           header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)
head(DemoMedOcuHx)

#TODO: Handling baseline and run-in randomization 
# VisitData$Visit == 'Run-in FU Randomization'
# VisitData$Visit == 'Enrollment'
# if enrollment 

# Step 1: Calculate SER for each eye
# Left Eye (OS)
VisitData$S_OS <- rowMeans(VisitData[, c("AutoRef1SphOS", "AutoRef2SphOS", "AutoRef3SphOS")], na.rm = TRUE)
VisitData$C_OS <- rowMeans(VisitData[, c("AutoRef1CylOS", "AutoRef2CylOS", "AutoRef3CylOS")], na.rm = TRUE)
VisitData$SER_OS <- VisitData$S_OS + (VisitData$C_OS / 2)

# Right Eye (OD)
VisitData$S_OD <- rowMeans(VisitData[, c("AutoRef1SphOD", "AutoRef2SphOD", "AutoRef3SphOD")], na.rm = TRUE)
VisitData$C_OD <- rowMeans(VisitData[, c("AutoRef1CylOD", "AutoRef2CylOD", "AutoRef3CylOD")], na.rm = TRUE)
VisitData$SER_OD <- VisitData$S_OD + (VisitData$C_OD / 2)

# Step 2: Average SER across both eyes for each visit (i.e., time j)
VisitData$SER <- rowMeans(VisitData[, c("SER_OS", "SER_OD")], na.rm = TRUE)

# Step 3: Merge data with PtRoster and DemoMedOcuHx datasets
# Merging VisitData with PtRoster on Patient ID
MergedData <- merge(VisitData, PtRoster, by = "PtID")

# Merging the result with DemoMedOcuHx on Patient ID
MergedData <- merge(MergedData, DemoMedOcuHx, by = "PtID")

# Step 4: Create necessary variables
# Time (Visit) is already in VisitData as 'Visit'
# Group/Treatment assignment
MergedData$TreatmentGroup <- MergedData$TrtGroup

# Parental myopia status (investigating variables)
MergedData$is_mother_has_myopia <- MergedData$MotherMyop
MergedData$is_father_has_myopia <- MergedData$Father

# Step 5: Filter for patients with more than 5 visits without NA SER
FilteredData <- MergedData %>%
  filter(!is.na(SER)) %>%  # Remove rows where SER is NA
  group_by(PtID) %>%       # Group by PtID
  filter(n() > 5) %>%      # Keep only groups (patients) with more than 5 valid visits
  ungroup()                # Remove grouping to return to a regular data frame

# Get unique PtID for filtered dataset
UniquePtID <- unique(FilteredData$PtID)


# Part 2. Spagetti Plot

# Sample 10 unique patients from each treatment group
SampledPatients_trt <- FilteredData %>%
  filter(TrtGroup == "Atropine") %>%
  distinct(PtID, .keep_all = TRUE) %>%
  slice_sample(n = 10) %>%
  select(PtID)

SampledPatients_ctrl <- FilteredData %>%
  filter(TrtGroup == "Placebo") %>%
  distinct(PtID, .keep_all = TRUE) %>%
  slice_sample(n = 10) %>%
  select(PtID)

SampledPatients <- bind_rows(SampledPatients_trt, SampledPatients_ctrl)

# Filter the original data to include only records for the sampled patients
SampledData <- FilteredData %>%
  filter(PtID %in% SampledPatients$PtID)

# Set line types based on the treatment group
SampledData <- SampledData %>%
  mutate(LineType = ifelse(TrtGroup == "Atropine", "solid", "dashed"))

# Define the order of visits for consistent x-axis ordering
visit_order <- c("Enrollment", "Month 6 Visit", "Month 12 Visit", 
                 "Month 18 Visit", "Month 24 Visit", "Month 30 Visit")

# Convert Visit to a factor with specified order
SampledData <- SampledData %>%
  filter(Visit != "Run-in FU Randomization") %>%
  mutate(Visit = factor(Visit, levels = visit_order))

# Create the spaghetti plot with distinct line types and colors for each patient
ggplot(SampledData, aes(x = Visit, y = SER, group = PtID, color = as.factor(PtID))) +
  geom_line(aes(linetype = LineType), size = 0.5, alpha = 0.7) +  # Line for each patient with line type by group
  geom_point(size = 2, shape = 1) +                                        # Add points at each SER measurement
  labs(title = "SER Progression Over Time by Treatment Group",
       x = "Visit",
       y = "SER",
       color = "Patient ID",
       linetype = 'LineType') +
  scale_x_discrete(drop = FALSE) +                              # Ensure all visits appear on the x-axis
  scale_linetype_manual(values = c("solid", "dashed")) +        
  theme_minimal() +
  theme(legend.position = "none")                               # Optionally hide the legend if there are too many patients



# Calculate and add mean profiles
MeanProfileData <- MergedData %>%
  filter(Visit != "Run-in FU Randomization") %>%
  group_by(Visit, TrtGroup) %>%
  summarize(Mean_SER = mean(SER, na.rm = TRUE), .groups = 'drop')

# Combined plot with individual trajectories and mean profiles
ggplot() +
  # Individual trajectories
  geom_line(data = SampledData, 
            aes(x = Visit, y = SER, group = PtID, color = as.factor(PtID), 
                linetype = TrtGroup),
            size = 0.5, alpha = 0.7) +
  geom_point(data = SampledData,
             aes(x = Visit, y = SER, color = as.factor(PtID)),
             size = 2, shape = 1) +
  # Mean profiles
  geom_line(data = MeanProfileData,
            aes(x = Visit, y = Mean_SER, group = TrtGroup, linetype = TrtGroup),
            size = 1.2) +
  geom_point(data = MeanProfileData,
             aes(x = Visit, y = Mean_SER, color = TrtGroup),
             size = 3, shape = 21, fill = "white") +
  # Labels and theme
  labs(title = "SER Progression Over Time by Treatment Group",
       x = "Visit",
       y = "SER",
       color = "Patient ID",
       linetype = "Treatment Group") +
  scale_x_discrete(drop = FALSE) +
  theme_minimal() +
  theme(legend.position = "right")

### Displaying five patients

# only leave the last observation, which is the 5th visit
SampledData %>%
  select(
    PtID,
    SER,
    Sex,
    TreatmentGroup,
    Race,
    EyeColor,
    AgeAsofEnrollDt,
    is_mother_has_myopia,
    is_father_has_myopia,
    Visit
  ) %>%
  group_by(PtID) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(PtID) %>%
  select(-Visit) %>% # then rename the column , ageasenrolldt as age and Eyecolor as iris_color
  rename(age = AgeAsofEnrollDt, iris_color = EyeColor) %>%
  head(5) %>%
  knitr::kable()


# key summary statistics for demographics

MergedData %>% 
  select(PtID,
         Sex,
         TreatmentGroup,
         Race,
         EyeColor,
         AgeAsofEnrollDt,
         is_mother_has_myopia,
         is_father_has_myopia) %>%
  rename(age = AgeAsofEnrollDt, iris_color = EyeColor) %>%
  group_by(PtID) %>% 
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(-PtID) -> Demographics

Demographics$age %>% summary()
Demographics %>%
  select(-age) %>%
  apply(.,2,table) %>%
  knitr::kable()


#### clean dataset with selected variables

data <- MergedData[,c("PtID","Visit","SER","TrtGroup","MotherMyop","FatherMyop","Sex","Race","EyeColor","AgeAsofEnrollDt")]

mother_gene <- ifelse(data$MotherMyop=="Yes",1,0)
father_gene <- ifelse(data$FatherMyop=="Yes",1,0)
data$genetic <- mother_gene+father_gene

data$Age <- floor(data$AgeAsofEnrollDt)


# save as Rdata

save(data,file="data.Rdata")



clean_data <- data %>%
  mutate(Visit = case_when(
    Visit == "Enrollment" ~ "Month0",
    Visit == "Run-in FU Randomization" ~ "Randomization",
    Visit == "Month 6 Visit" ~ "Month6",
    Visit == "Month 12 Visit" ~ "Month12", 
    Visit == "Month 18 Visit" ~ "Month18",
    Visit == "Month 24 Visit" ~ "Month24",
    Visit == "Month 30 Visit" ~ "Month30"
  ))

# 2. 转换为宽格式
wide_data <- clean_data %>%
  select(PtID, Visit, SER, TrtGroup, MotherMyop, FatherMyop, Sex, Race, EyeColor, AgeAsofEnrollDt, genetic, Age) %>%
  pivot_wider(
    id_cols = c(PtID, TrtGroup, MotherMyop, FatherMyop, Sex, Race, EyeColor, AgeAsofEnrollDt, genetic, Age),
    names_from = Visit,
    values_from = SER,
    names_prefix = "SER_"
  ) %>% select(-SER_Randomization)

view(wide_data)

# check missingness

wide_data %>% select(
  SER_Month6,
  SER_Month12,
  SER_Month18,
  SER_Month24,
  SER_Month30
) %>% apply(.,2,function(x) sum(is.na(x)))


# start imputing missing values

library(mice)

# install naniar if not installed
if (!requireNamespace("naniar", quietly = TRUE)) {
  install.packages("naniar")
}
library(naniar)

# Create prediction matrix for longitudinal data

# 1. Set up prediction matrix
pred_mat <- make.predictorMatrix(wide_data)

# 2. Modify prediction matrix to ensure temporal ordering
# Set future time points to 0 to prevent them from being used in prediction
# This ensures we only use past and present data to impute missing values
pred_mat["SER_Month0", c("SER_Month6","SER_Month12","SER_Month18","SER_Month24","SER_Month30")] <- 0
pred_mat["SER_Month6", c("SER_Month12","SER_Month18","SER_Month24","SER_Month30")] <- 0
pred_mat["SER_Month12", c("SER_Month18","SER_Month24","SER_Month30")] <- 0
pred_mat["SER_Month18", c("SER_Month24","SER_Month30")] <- 0
pred_mat["SER_Month24", "SER_Month30"] <- 0

# 3. Set imputation methods
meth <- make.method(wide_data)
# Set method for all SER columns to normal distribution
# Since SER is a continuous variable, we use normal distribution for imputation
meth[grep("SER_", names(meth))] <- "pmm"  

# 4. Perform multiple imputation
imp <- mice(wide_data, 
            m = 10,           # Generate 10 imputed datasets
            maxit = 50,      # Number of iterations
            method = meth,   # Specified imputation methods
            predictorMatrix = pred_mat,  # Custom prediction matrix
            seed = 123)      # Set seed for reproducibility

# 5. Check imputation quality
# Examine distributions of imputed values versus observed values
densityplot(imp)
# Check convergence of imputation algorithm
plot(imp)

# 6. Get completed datasets
# Get all imputed datasets in long format
completed_data <- complete(imp, action = "long")  
# Or get a single imputed dataset
completed_data_1 <- complete(imp, 1)  # Get first imputed dataset



# Optional: Sensitivity Analyses
# A. Using Predictive Mean Matching instead of normal distribution
meth_pmm <- meth
meth_pmm[grep("SER_", names(meth))] <- "pmm"  
imp_pmm <- mice(wide_data, 
                m = 5,
                method = meth_pmm,
                predictorMatrix = pred_mat)

# B. Compare different numbers of imputations
imp_5 <- mice(wide_data, m = 5, pred = pred_mat)
imp_10 <- mice(wide_data, m = 10, pred = pred_mat)

# C. Last Observation Carried Forward (LOCF) for sensitivity analysis
locf_data <- wide_data %>%
  mutate(
    across(starts_with("SER_"), 
           ~if_else(is.na(.), lag(., order_by = as.numeric(gsub("SER_Month", "", cur_column()))), .))
  )







