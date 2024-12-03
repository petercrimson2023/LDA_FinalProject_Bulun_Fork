library(tidyverse)
library(nlme)
library(lme4)

# Load data
load("data.RData")

# 1. Clean and preprocess data
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
    log_SER = log(-SER)
  )

# TrtGroup SER Month 18 - N(-1,0.5)
# 
# len_18 = length(which(clean_data$TrtGroup != "Placebo" & clean_data$Visit == "Month18"))
# 
# clean_data[which(clean_data$TrtGroup != "Placebo" 
#                  & clean_data$Visit == "Month18"), "SER"] = clean_data[which(clean_data$TrtGroup != "Placebo" 
#                                                                              & clean_data$Visit == "Month18"), "SER"] + rnorm(len_18, -4, 0.5)
# # TrtGroup SER Month 24 - N(-1.5,0.5)
# 
# len_24 = length(which(clean_data$TrtGroup != "Placebo" & clean_data$Visit == "Month24"))
# 
# clean_data[which(clean_data$TrtGroup != "Placebo" 
#                  & clean_data$Visit == "Month24"), "SER"] = clean_data[which(clean_data$TrtGroup != "Placebo" 
#                                                                              & clean_data$Visit == "Month24"), "SER"] + rnorm(len_24, -6, 0.5)
# 
# # TrtGroup SER Month 30 - N(-2,1)
# 
# len_30 = length(which(clean_data$TrtGroup != "Placebo" & clean_data$Visit == "Month30"))
# 
# clean_data[which(clean_data$TrtGroup != "Placebo" 
#                  & clean_data$Visit == "Month30"), "SER"] = clean_data[which(clean_data$TrtGroup != "Placebo" 
#                                                                              & clean_data$Visit == "Month30"), "SER"] + rnorm(len_30, -8, 1)
# 

# 2. Convert to wide format
wide_data <- clean_data %>%
  select(PtID, Visit, SER, TrtGroup, MotherMyop, FatherMyop, Sex, Race, EyeColor, AgeAsofEnrollDt, genetic, Age) %>%
  pivot_wider(
    id_cols = c(PtID, TrtGroup, MotherMyop, FatherMyop, Sex, Race, EyeColor, AgeAsofEnrollDt, genetic, Age),
    names_from = Visit,
    values_from = SER,
    names_prefix = "SER_"
  ) %>% 
  select(-SER_Randomization)  # Remove randomization visit

# only study for white people

#wide_data %>% filter(Race != "White") -> wide_data


# 3. 定义时间顺序并进行LOCF
# 首先将NaN转换为NA
wide_data <- wide_data %>%
  mutate(across(starts_with("SER_"), ~if_else(is.nan(.x), NA_real_, .x)))

# 定义时间顺序
time_order <- c("Month0", "Month6", "Month12", "Month18", "Month24", "Month30")
ser_cols <- paste0("SER_", time_order)

# 按照时间顺序进行LOCF
imputed_data <- wide_data
for(id in unique(wide_data$PtID)) {
  # 获取当前病人的数据
  patient_row <- which(imputed_data$PtID == id)
  
  # 初始化last_valid_value
  last_valid_value <- NA
  
  # 按时间顺序遍历SER列
  for(col in ser_cols) {
    current_value <- imputed_data[patient_row, col]
    
    # 如果当前值不是NA，更新last_valid_value
    if(!is.na(current_value)) {
      last_valid_value <- current_value
    } 
    # 如果当前值是NA且有上一个有效值，进行填充
    else if(!is.na(last_valid_value)) {
      imputed_data[patient_row, col] <- last_valid_value
    }
  }
}

view(imputed_data)
view(wide_data)

#imputed_data %>% select(-SER_Month0,-SER_Month6,-SER_Month12,-SER_Month18) -> imputed_data

# 4. Convert back to long format for analysis
long_data <- imputed_data %>%
  pivot_longer(
    cols = starts_with("SER_"),
    names_to = "Visit",
    values_to = "SER",
    names_prefix = "SER_"
  ) %>%
  # Convert Visit to actual month numbers
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

# 添加18个月后的spline项
long_data <- long_data %>%
  mutate(
    # 创建18个月后的时间变量
    Time_after_18 = pmax(0, Visit_numeric - 18)  # 18个月之前为0，之后为实际经过的月数
  )

# 拟合新的混合效应模型，包含spline项和交互效应
model_spline <- lme(
  fixed = SER ~ TrtGroup * (genetic + EyeColor) +  # 原有的交互项
    AgeAsofEnrollDt + Visit_numeric + Race * Sex +  # 原有的主效应
    Time_after_18 +  # spline主效应
    TrtGroup:Time_after_18 +  # Treatment与spline的交互
    Time_after_18:genetic +    # Genetic与spline的交互
    Time_after_18:EyeColor +   # EyeColor与spline的交互
    TrtGroup:Time_after_18:genetic +  # 三阶交互
    TrtGroup:Time_after_18:EyeColor,  # 三阶交互
  random = ~ Visit_numeric | PtID,
  data = long_data,
  na.action = na.omit
)

# 获取模型结果
summary_table <- summary(model_spline)$tTable
final_results <- data.frame(
  term = rownames(summary_table),
  estimate = summary_table[,"Value"],
  std.error = summary_table[,"Std.Error"],
  statistic = summary_table[,"t-value"],
  p.value = summary_table[,"p-value"]
)

view(final_results)

# 查看特定的交互效应
interaction_terms <- final_results %>%
  filter(grepl("Time_after_18", term))

# 打印交互效应结果
print("18个月后的Spline项及其交互效应:")
print(interaction_terms)

view(interaction_terms)

# 可选：计算不同组别在18个月后的斜率
# 比如，对于不同治疗组的总体斜率：
slope_effects <- final_results %>%
  filter(term %in% c("Time_after_18", "TrtGroup:Time_after_18"))

print("\n各治疗组在18个月后的总体斜率:")
print(slope_effects)
