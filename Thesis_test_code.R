
#Set working directory
setwd("/Users/ajlasijercic/Desktop")

# Load necessary library
library(dplyr)
library(tidyverse)
library(psych)
install.packages("car")
library(car)
source("process.R")
install.packages("lavaan")
install.packages("semTools")
library(lavaan)
library(semTools)
library(e1071)
# Load data sets
final_osog_data <-read.csv("final_last_osog_experiment.csv")
qualtrics_final_data<-read.csv("final_last_qualtrics.csv")

# Merge datasets using an inner join, only including rows with matching identifiers
merged_data <- merge(final_osog_data, qualtrics_final_data, by.x = "Participant", by.y = "ResponseId", all = FALSE)

#Remove participants who did not consent, did not finish survey & failed attention check (Conformism.scale_10)
merged_data <- merged_data %>%
  filter(Consent=="I consent") %>%
  filter(Finished=="True") %>%
  filter(Conformism.scale_10=="Neither agree nor disagree")

# Identifying not relevant columns 
not_relevant_columns <- c("Item[0-9]+_(Serving_size|Serving_size_grams|Calories_from_fat|Calories|Caloric_density|Total_fat|Saturated_fat|Trans_fat|Poly_fat|Mono_fat|Cholesterol|Sodium|Potassium|Carbs|Fiber|Sugar|Protein|Starpoints)",
                          "Total_(Serving_size_grams|Calories_from_fat|Calories|Caloric_density|Total_fat|Saturated_fat|Trans_fat|Poly_fat|Mono_fat|Cholesterol|Sodium|Potassium|Carbs|Fiber|Sugar|Protein|Starpoints)",
                          "Prolific", "Consent", "Q[0-9]+_",
                          "StartDate", "EndDate", "Status", "IPAddress", 
                          "Duration..in.seconds.", "RecordedDate", "RecipientLastName", "RecipientFirstName", 
                          "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", 
                          "DistributionChannel", "UserLanguage",
                          "Timer_(First.Click|Last.Click|Page.Submit|Click.Count)", "Code", "Finished", "Progress")

# Search not relevant columns
not_relevant_columns_search_pattern <- paste(not_relevant_columns, collapse = "|")

# Find columns that match the search
columns_to_remove <- grep(not_relevant_columns_search_pattern, names(merged_data), value = TRUE)

# Remove the columns from the dataset
merged_data_cleaned <- merged_data %>% select(-all_of(columns_to_remove))

######### Data is clean, now counting of F&V

# Step 1: see subcategories 
# Initialise an empty vector to store subcategories
all_subcategories <- c()

# Loop through each column name in the dataset
for (col_name in names(merged_data_cleaned)) {
  # Check if the column name contains "Subcategory"
  if (grepl("Subcategory", col_name)) {
    # Concatenate the values from this column with the existing subcategories
    all_subcategories <- c(all_subcategories, merged_data_cleaned[[col_name]])
  }
}

# Remove any NA values
all_subcategories <- na.omit(all_subcategories)

# Get unique subcategories
unique_subcategories <- unique(all_subcategories)

# Print the unique subcategories
unique_subcategories

#Convert item quantities to numeric (replace NA with 0)
for (i in 1:25) {
  quantity_col <- paste0("Item", i, "_Quantity")
  merged_data_cleaned[[quantity_col]] <- as.numeric(as.character(merged_data_cleaned[[quantity_col]]))
  # Replace NA with 0
  merged_data_cleaned[[quantity_col]][is.na(merged_data_cleaned[[quantity_col]])] <- 0
}

# Make dummy variables 
# Define the target subcategories (F&V)
target_subcategories <- c("Fresh Vegetables", "Fresh Fruit", "Fruit", "Vegetables", "Canned Vegetables")

# Loop through each item subcategory column
for (i in 1:25) { # Adjust this number if the actual count of item columns differs
  # Create a new dummy variable column for each item subcategory column
  merged_data_cleaned[[paste0("Item", i, "_dummy")]] <- as.integer(merged_data_cleaned[[paste0("Item", i, "_Subcategory")]] %in% target_subcategories)
}

# Calculate the total fruits and vegetables column
merged_data_cleaned <- merged_data_cleaned %>%
  mutate(total_fruits_vegetables = rowSums(across(matches('Item[0-9]+_Quantity'), 
                                                  ~ . * get(gsub('Quantity', 'dummy', cur_column())))))

# Rename randomisation column
merged_data_cleaned <- merged_data_cleaned %>%
  rename(Norm_Condition = FL_5_DO)

# Count the occurrences of each group norm
group_counts <- table(merged_data_cleaned$Norm_Condition)

# Extract the counts for the three groups of interest
descriptive_count <- group_counts["Descriptive"]
provincial_count <- group_counts["Provincial"]
nonorm_count <- group_counts["Nonorm"]

# Counts
cat("Descriptive count:", descriptive_count, "\n")
cat("Provincial count:", provincial_count, "\n")
cat("Nonorm count:", nonorm_count, "\n")

#Randomisation check
  #income
contingency_table_income <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Income)
contingency_table_income
chi_square_income <- chisq.test(contingency_table_income)
chi_square_income
  #gender
contingency_table_gender <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Gender)
contingency_table_gender
chi_square_gender <- chisq.test(contingency_table_gender)
chi_square_gender
  #age
contingency_table_age <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Age)
contingency_table_age
chi_square_age <- chisq.test(contingency_table_age)
chi_square_age
  #nationality
contingency_table_nationality <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Nationality)
contingency_table_nationality
chi_square_nationality <- chisq.test(contingency_table_nationality)
chi_square_nationality
  #nationality
contingency_table_nationality <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Nationality)
contingency_table_nationality
chi_square_nationality <- chisq.test(contingency_table_nationality)
chi_square_nationality
  #education
contingency_table_education <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Education)
contingency_table_education
chi_square_education <- chisq.test(contingency_table_education)
chi_square_education

#Normality assumption
shapiro.test(merged_data_cleaned$total_fruits_vegetables)
hist(merged_data_cleaned$total_fruits_vegetables, freq = FALSE, breaks = 30)
curve(dnorm(x, mean = mean(merged_data_cleaned$total_fruits_vegetables), sd = sd(merged_data_cleaned$total_fruits_vegetables)), add = TRUE) #add density curve
    # Not normally distributed 
  # Log transformation to fix the violation 
merged_data_cleaned$log_f_v <- log(merged_data_cleaned$total_fruits_vegetables+1) #add 1 because error log(0)... shift whole graph by one unit
shapiro.test(merged_data_cleaned$log_f_v)
hist(merged_data_cleaned$log_f_v, freq = FALSE, breaks = 30)
curve(dnorm(x, mean = mean(merged_data_cleaned$log_f_v), sd = sd(merged_data_cleaned$log_f_v)), add = TRUE)
  # Remove outliers
# Plot initial boxplot
boxplot(merged_data_cleaned$log_f_v, ylim= c(0, 5), range = 1)
# Calculate Q1, Q3, and IQR
q1 <- quantile(merged_data_cleaned$log_f_v, 0.25, na.rm = TRUE)
q3 <- quantile(merged_data_cleaned$log_f_v, 0.75, na.rm = TRUE)
iqr <- q3 - q1
# Calculate outlier thresholds
outlier_high_val <- q3 + 1.5 * iqr
outlier_low_val <- q1 - 1.5 * iqr
# Print IQR and thresholds
cat("IQR:", iqr, "\n")
cat("Outlier High Threshold:", outlier_high_val, "\n")
cat("Outlier Low Threshold:", outlier_low_val, "\n")
# Plot boxplot with whiskers
bp <- boxplot(merged_data_cleaned$log_f_v, ylim = c(0, 5), range = 1, main = "Boxplot with Internal Whisker Check")
cat("Upper whisker from boxplot:", bp$stats[5,], "\n")
# Print summary statistics before filtering
cat("Summary before filtering:\n")
print(summary(merged_data_cleaned$log_f_v))
cat("Standard deviation before filtering:", sd(merged_data_cleaned$log_f_v, na.rm = TRUE), "\n")
print(outlier_high_val)
print(outlier_low_val)
### Remove outliers
merged_data_rm_outliers <- merged_data_cleaned %>% 
  filter(log_f_v <= 3.33 & log_f_v >= 0.169899)
# Print summary statistics after filtering
cat("Summary after filtering:\n")
print(summary(merged_data_rm_outliers$log_f_v))
cat("Standard deviation after filtering:", sd(merged_data_rm_outliers$log_f_v, na.rm = TRUE), "\n")
# Plot boxplot after removing outliers
boxplot(merged_data_rm_outliers$log_f_v, ylim = c(0, 5), range = 1, main = "Boxplot after Removing Outliers")
  # Check normality without outliers
shapiro.test(merged_data_rm_outliers$log_f_v)
hist(merged_data_rm_outliers$log_f_v, freq = FALSE, breaks = 30)
curve(dnorm(x, mean = mean(merged_data_rm_outliers$log_f_v), sd = sd(merged_data_rm_outliers$log_f_v)), add = TRUE)

# Perform Skewness & Kurtosis test to see if distribution improved
skewness_val <- skewness(merged_data_cleaned$log_f_v, na.rm = TRUE)
kurtosis_val <- kurtosis(merged_data_cleaned$log_f_v, na.rm = TRUE)
cat("Skewness:", skewness_val, "\n")
cat("Kurtosis:", kurtosis_val, "\n")

# Perform Skewness & Kurtosis test to see if distribution improved
skewness_val_no_outliers <- skewness(merged_data_rm_outliers$log_f_v, na.rm = TRUE)
kurtosis_val_no_outliers <- kurtosis(merged_data_rm_outliers$log_f_v, na.rm = TRUE)
cat("Skewness:", skewness_val_no_outliers, "\n") #has improved
cat("Kurtosis:", kurtosis_val_no_outliers, "\n") #has improved

# Equal variance assumption
leveneTest(total_fruits_vegetables ~ as.factor(Norm_Condition), data = merged_data_rm_outliers) #Variance of DV is eqaul across all IVs
# var.equal = TRUE

# Descriptive statistics 
  # Total
total_respondents <- nrow(merged_data_rm_outliers)
print(total_respondents)
  # Social norm nudges
nudge_counts <- table(merged_data_rm_outliers$Norm_Condition)
print(nudge_counts)
nudge_proportions <- prop.table(nudge_counts)
print(nudge_proportions)
  # Gender
gender_counts <- table(merged_data_rm_outliers$Gender)
print(gender_counts)
gender_proportions <- prop.table(gender_counts)
print(gender_proportions)
  # Age
psych::describe(as.numeric(merged_data_rm_outliers$Age))
  # Income 
income_counts <- table(merged_data_rm_outliers$Income)
print(income_counts)
income_proportions <- prop.table(income_counts)
print(income_proportions)
  # Education
edu_counts <- table(merged_data_rm_outliers$Education)
print(edu_counts)
edu_proportions <- prop.table(edu_counts)
print(edu_proportions)
  # Quantity of F&V
psych::describe(as.numeric(merged_data_rm_outliers$Quantity, rm.na = TRUE))
  # Conformity
psych::describe(as.numeric(merged_data_rm_outliers$Avg_conformity))

# ANOVA 
anova_output <- merged_data_rm_outliers %>%
  dplyr::group_by(Norm_Condition) %>%
  dplyr::summarise(
    mean_log_fruits_vegetables = mean(log_f_v, na.rm = TRUE),
    sd_log_fruits_vegetables = sd(log_f_v, na.rm = TRUE)
  )

print(anova_output)

anova_result <- aov(log_f_v ~ Norm_Condition, data = merged_data_rm_outliers)
summary(anova_result)

  # Mean per condition
mean_per_condition <- merged_data_cleaned %>%
  group_by(Norm_Condition) %>%
  summarise(mean_fruits_vegetables = mean(log_f_v, na.rm = TRUE))
print(mean_per_condition)
  # Visualise 
custom_colors <- c("Nonorm" = "#66c2a5", "Descriptive" = "lightgreen", "Provincial" = "forestgreen")
anova_visual <- ggplot(merged_data_rm_outliers, aes(x = Norm_Condition, y = log_f_v, fill = Norm_Condition)) +
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(x = "Norm Condition", y = "Log(Fruits and Vegetables)", title = "ANOVA: Log(Fruits and Vegetables) by Norm Condition") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),  # Bold x-axis title
    axis.title.y = element_text(face = "bold", size = 12)   # Bold y-axis title
  )

anova_visual
ggsave("Mean_and_SD_of_Fruits_and_Vegetables_by_Norm_Condition.jpeg", plot = anova_visual, width = 10, height = 6, units = "in", dpi = 300)

# Preparing for the PROCESS Regression
  # Conformism scale
  # Make scale  numerical
merged_data_rm_outliers$Conformism.scale_1 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_1, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_2 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_2, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_3 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_3, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_4 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_4, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_5 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_5, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_6 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_6, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_7 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_7, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_8 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_8, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_9 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_9, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_10 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_10, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_11 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_11, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_12 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_12, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
#Recode values of item 7 ,9, and 12 of the scale (currently reversed)
merged_data_rm_outliers$Conformism.scale_7 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_7, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1)
merged_data_rm_outliers$Conformism.scale_9 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_9, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1)
merged_data_rm_outliers$Conformism.scale_12 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_1, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1)
  #Select item 1-9 & 11-12 for the conformity scale (exclude (10) attention check)
Conformity_scale <- merged_data_rm_outliers %>%
  select(Conformism.scale_1, Conformism.scale_2, Conformism.scale_3, Conformism.scale_4, Conformism.scale_5, Conformism.scale_6, Conformism.scale_7, Conformism.scale_8, Conformism.scale_9, Conformism.scale_11, Conformism.scale_12)
  # Compute Cronbach Alpha 
alpha_conformity <- psych::alpha(Conformity_scale)
alpha_conformity$total #Scale is 0.6606792, which is below threshold of 0.7
alpha_conformity$alpha.drop #See which item can be dropped to improve the reliability of the scale 
    #Dropping item 12 will improve the Cronbach alpha (0.7664477>0.7) 
    #Could be because it is reversed, and people are more sensitive to phrasing 
  # Creating a new scale, dropping conformity_scale_12
Conformity_scale <- merged_data_rm_outliers %>%
  select(Conformism.scale_1, Conformism.scale_2, Conformism.scale_3, Conformism.scale_4, Conformism.scale_5, Conformism.scale_6, Conformism.scale_7, Conformism.scale_8, Conformism.scale_9, Conformism.scale_11)
alpha_conformity <- alpha(Conformity_scale)
alpha_conformity$total #Now it is fixed!
  #Compute the average row mean
merged_data_rm_outliers$Avg_conformity <- rowMeans(Conformity_scale)
mean(merged_data_rm_outliers$Avg_conformity)
rm(list = ls())

#Set working directory
setwd("/Users/ajlasijercic/Desktop")

# Load necessary library
library(dplyr)
library(tidyverse)
library(psych)
install.packages("car")
library(car)
source("process.R")
install.packages("lavaan")
install.packages("semTools")
library(lavaan)
library(semTools)
library(e1071)
# Load data sets
final_osog_data <-read.csv("final_last_osog_experiment.csv")
qualtrics_final_data<-read.csv("final_last_qualtrics.csv")

# Merge datasets using an inner join, only including rows with matching identifiers
merged_data <- merge(final_osog_data, qualtrics_final_data, by.x = "Participant", by.y = "ResponseId", all = FALSE)

#Remove participants who did not consent, did not finish survey & failed attention check (Conformism.scale_10)
merged_data <- merged_data %>%
  filter(Consent=="I consent") %>%
  filter(Finished=="True") %>%
  filter(Conformism.scale_10=="Neither agree nor disagree")

# Identifying not relevant columns 
not_relevant_columns <- c("Item[0-9]+_(Serving_size|Serving_size_grams|Calories_from_fat|Calories|Caloric_density|Total_fat|Saturated_fat|Trans_fat|Poly_fat|Mono_fat|Cholesterol|Sodium|Potassium|Carbs|Fiber|Sugar|Protein|Starpoints)",
                          "Total_(Serving_size_grams|Calories_from_fat|Calories|Caloric_density|Total_fat|Saturated_fat|Trans_fat|Poly_fat|Mono_fat|Cholesterol|Sodium|Potassium|Carbs|Fiber|Sugar|Protein|Starpoints)",
                          "Prolific", "Consent", "Q[0-9]+_",
                          "StartDate", "EndDate", "Status", "IPAddress", 
                          "Duration..in.seconds.", "RecordedDate", "RecipientLastName", "RecipientFirstName", 
                          "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", 
                          "DistributionChannel", "UserLanguage",
                          "Timer_(First.Click|Last.Click|Page.Submit|Click.Count)", "Code", "Finished", "Progress")

# Search not relevant columns
not_relevant_columns_search_pattern <- paste(not_relevant_columns, collapse = "|")

# Find columns that match the search
columns_to_remove <- grep(not_relevant_columns_search_pattern, names(merged_data), value = TRUE)

# Remove the columns from the dataset
merged_data_cleaned <- merged_data %>% select(-all_of(columns_to_remove))

######### Data is clean, now counting of F&V

# Step 1: see subcategories 
# Initialise an empty vector to store subcategories
all_subcategories <- c()

# Loop through each column name in the dataset
for (col_name in names(merged_data_cleaned)) {
  # Check if the column name contains "Subcategory"
  if (grepl("Subcategory", col_name)) {
    # Concatenate the values from this column with the existing subcategories
    all_subcategories <- c(all_subcategories, merged_data_cleaned[[col_name]])
  }
}

# Remove any NA values
all_subcategories <- na.omit(all_subcategories)

# Get unique subcategories
unique_subcategories <- unique(all_subcategories)

# Print the unique subcategories
unique_subcategories

#Convert item quantities to numeric (replace NA with 0)
for (i in 1:25) {
  quantity_col <- paste0("Item", i, "_Quantity")
  merged_data_cleaned[[quantity_col]] <- as.numeric(as.character(merged_data_cleaned[[quantity_col]]))
  # Replace NA with 0
  merged_data_cleaned[[quantity_col]][is.na(merged_data_cleaned[[quantity_col]])] <- 0
}

# Make dummy variables 
# Define the target subcategories (F&V)
target_subcategories <- c("Fresh Vegetables", "Fresh Fruit", "Fruit", "Vegetables", "Canned Vegetables")

# Loop through each item subcategory column
for (i in 1:25) { # Adjust this number if the actual count of item columns differs
  # Create a new dummy variable column for each item subcategory column
  merged_data_cleaned[[paste0("Item", i, "_dummy")]] <- as.integer(merged_data_cleaned[[paste0("Item", i, "_Subcategory")]] %in% target_subcategories)
}

# Calculate the total fruits and vegetables column
merged_data_cleaned <- merged_data_cleaned %>%
  mutate(total_fruits_vegetables = rowSums(across(matches('Item[0-9]+_Quantity'), 
                                                  ~ . * get(gsub('Quantity', 'dummy', cur_column())))))

# Rename randomisation column
merged_data_cleaned <- merged_data_cleaned %>%
  rename(Norm_Condition = FL_5_DO)

# Count the occurrences of each group norm
group_counts <- table(merged_data_cleaned$Norm_Condition)

# Extract the counts for the three groups of interest
descriptive_count <- group_counts["Descriptive"]
provincial_count <- group_counts["Provincial"]
nonorm_count <- group_counts["Nonorm"]

# Counts
cat("Descriptive count:", descriptive_count, "\n")
cat("Provincial count:", provincial_count, "\n")
cat("Nonorm count:", nonorm_count, "\n")

#Randomisation check
  #income
contingency_table_income <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Income)
contingency_table_income
chi_square_income <- chisq.test(contingency_table_income)
chi_square_income
  #gender
contingency_table_gender <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Gender)
contingency_table_gender
chi_square_gender <- chisq.test(contingency_table_gender)
chi_square_gender
  #age
contingency_table_age <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Age)
contingency_table_age
chi_square_age <- chisq.test(contingency_table_age)
chi_square_age
  #nationality
contingency_table_nationality <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Nationality)
contingency_table_nationality
chi_square_nationality <- chisq.test(contingency_table_nationality)
chi_square_nationality
  #education
contingency_table_education <- table(merged_data_cleaned$Norm_Condition, merged_data_cleaned$Education)
contingency_table_education
chi_square_education <- chisq.test(contingency_table_education)
chi_square_education

# Descriptive statistics 
# Total
total_respondents <- nrow(merged_data_cleaned)
print(total_respondents)
# Social norm nudges
nudge_counts <- table(merged_data_cleaned$Norm_Condition)
print(nudge_counts)
nudge_proportions <- prop.table(nudge_counts)
print(nudge_proportions)*100
# Gender
gender_counts <- table(merged_data_cleaned$Gender)
print(gender_counts)
gender_proportions <- prop.table(gender_counts)
print(gender_proportions)*100
# Age
psych::describe(as.numeric(merged_data_cleaned$Age))
# Income 
income_counts <- table(merged_data_cleaned$Income)
print(income_counts)
income_proportions <- prop.table(income_counts)
print(income_proportions)*100
# Education
edu_counts <- table(merged_data_cleaned$Education)
print(edu_counts)
edu_proportions <- prop.table(edu_counts)
print(edu_proportions)*100

#Normality assumption
shapiro.test(merged_data_cleaned$total_fruits_vegetables)
hist(merged_data_cleaned$total_fruits_vegetables, freq = FALSE, breaks = 30)
curve(dnorm(x, mean = mean(merged_data_cleaned$total_fruits_vegetables), sd = sd(merged_data_cleaned$total_fruits_vegetables)), add = TRUE) #add density curve
    # Not normally distributed 
  # Log transformation to fix the violation 
merged_data_cleaned$log_f_v <- log(merged_data_cleaned$total_fruits_vegetables+1) #add 1 because error log(0)... shift whole graph by one unit
shapiro.test(merged_data_cleaned$log_f_v)
hist(merged_data_cleaned$log_f_v, freq = FALSE, breaks = 30)
curve(dnorm(x, mean = mean(merged_data_cleaned$log_f_v), sd = sd(merged_data_cleaned$log_f_v)), add = TRUE)
  # Remove outliers
# Plot initial boxplot
boxplot(merged_data_cleaned$log_f_v, ylim= c(0, 5), range = 1)
# Calculate Q1, Q3, and IQR
q1 <- quantile(merged_data_cleaned$log_f_v, 0.25, na.rm = TRUE)
q3 <- quantile(merged_data_cleaned$log_f_v, 0.75, na.rm = TRUE)
iqr <- q3 - q1
# Calculate outlier thresholds
outlier_high_val <- q3 + 1.5 * iqr
outlier_low_val <- q1 - 1.5 * iqr
# Print IQR and thresholds
cat("IQR:", iqr, "\n")
cat("Outlier High Threshold:", outlier_high_val, "\n")
cat("Outlier Low Threshold:", outlier_low_val, "\n")
# Plot boxplot with whiskers
bp <- boxplot(merged_data_cleaned$log_f_v, ylim = c(0, 5), range = 1, main = "Boxplot with Internal Whisker Check")
cat("Upper whisker from boxplot:", bp$stats[5,], "\n")
# Print summary statistics before filtering
cat("Summary before filtering:\n")
print(summary(merged_data_cleaned$log_f_v))
cat("Standard deviation before filtering:", sd(merged_data_cleaned$log_f_v, na.rm = TRUE), "\n")
print(outlier_high_val)
print(outlier_low_val)
### Remove outliers
merged_data_rm_outliers <- merged_data_cleaned %>% 
  filter(log_f_v <= 3.33 & log_f_v >= 0.169899)
# Print summary statistics after filtering
cat("Summary after filtering:\n")
print(summary(merged_data_rm_outliers$log_f_v))
cat("Standard deviation after filtering:", sd(merged_data_rm_outliers$log_f_v, na.rm = TRUE), "\n")
# Plot boxplot after removing outliers
boxplot(merged_data_rm_outliers$log_f_v, ylim = c(0, 5), range = 1, main = "Boxplot after Removing Outliers")
  # Check normality without outliers
shapiro.test(merged_data_rm_outliers$log_f_v)
hist(merged_data_rm_outliers$log_f_v, freq = FALSE, breaks = 30)
curve(dnorm(x, mean = mean(merged_data_rm_outliers$log_f_v), sd = sd(merged_data_rm_outliers$log_f_v)), add = TRUE)

# Perform Skewness & Kurtosis test to see if distribution improved (prior data)
skewness_val <- skewness(merged_data_cleaned$log_f_v, na.rm = TRUE)
kurtosis_val <- kurtosis(merged_data_cleaned$log_f_v, na.rm = TRUE)
cat("Skewness:", skewness_val, "\n")
cat("Kurtosis:", kurtosis_val, "\n")

# Perform Skewness & Kurtosis test to see if distribution improved (after outliers are removed data)
skewness_val_no_outliers <- skewness(merged_data_rm_outliers$log_f_v, na.rm = TRUE)
kurtosis_val_no_outliers <- kurtosis(merged_data_rm_outliers$log_f_v, na.rm = TRUE)
cat("Skewness:", skewness_val_no_outliers, "\n") #has improved
cat("Kurtosis:", kurtosis_val_no_outliers, "\n") #has improved

subject_count_pre <- merged_data_cleaned %>%
  group_by(Norm_Condition) %>%
  summarise(count = n())
subject_count_pre

subject_count_rm <- merged_data_rm_outliers %>%
  group_by(Norm_Condition) %>%
  summarise(count = n())
subject_count_rm

# Equal variance assumption
leveneTest(total_fruits_vegetables ~ as.factor(Norm_Condition), data = merged_data_rm_outliers) #Variance of DV is eqaul across all IVs
# var.equal = TRUE

  # Conformity
psych::describe(as.numeric(merged_data_rm_outliers$Avg_conformity))

# Quantity of F&V
psych::describe(as.numeric(merged_data_cleaned$Quantity, rm.na = TRUE))
merged_data_rm_outliers$Quantity <- as.numeric(merged_data_rm_outliers$Quantity)

# ANOVA 
anova_output <- merged_data_rm_outliers %>%
  dplyr::group_by(Norm_Condition) %>%
  dplyr::summarise(
    mean_log_fruits_vegetables = mean(log_f_v, na.rm = TRUE),
    sd_log_fruits_vegetables = sd(log_f_v, na.rm = TRUE)
  )

print(anova_output)

anova_result <- aov(log_f_v ~ Norm_Condition, data = merged_data_rm_outliers)
summary(anova_result)

  # Mean per condition
mean_per_condition <- merged_data_cleaned %>%
  group_by(Norm_Condition) %>%
  summarise(mean_fruits_vegetables = mean(log_f_v, na.rm = TRUE))
print(mean_per_condition)
  # Visualise 
custom_colors <- c("Nonorm" = "#66c2a5", "Descriptive" = "lightgreen", "Provincial" = "forestgreen")
anova_visual <- ggplot(merged_data_rm_outliers, aes(x = Norm_Condition, y = log_f_v, fill = Norm_Condition)) +
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(x = "Norm Condition", y = "Log(Fruits and Vegetables)", title = "ANOVA: Log(Fruits and Vegetables) by Norm Condition") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),  # Bold x-axis title
    axis.title.y = element_text(face = "bold", size = 12)   # Bold y-axis title
  )

anova_visual
ggsave("Mean_and_SD_of_Fruits_and_Vegetables_by_Norm_Condition.jpeg", plot = anova_visual, width = 10, height = 6, units = "in", dpi = 300)

# Preparing for the PROCESS Regression
  # Conformism scale
  # Make scale  numerical
merged_data_rm_outliers$Conformism.scale_1 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_1, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_2 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_2, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_3 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_3, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_4 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_4, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_5 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_5, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_6 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_6, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_7 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_7, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_8 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_8, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_9 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_9, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_10 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_10, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_11 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_11, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
merged_data_rm_outliers$Conformism.scale_12 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_12, "Strongly agree"=7, "Agree"=6, "Somewhat agree"=5, "Neither agree nor disagree"=4, "Somewhat disagree"=3, "Disagree"=2, "Strongly disagree"=1)
#Recode values of item 7 ,9, and 12 of the scale (currently reversed)
merged_data_rm_outliers$Conformism.scale_7 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_7, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1)
merged_data_rm_outliers$Conformism.scale_9 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_9, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1)
merged_data_rm_outliers$Conformism.scale_12 <- dplyr::recode(merged_data_rm_outliers$Conformism.scale_1, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1)
  #Select item 1-9 & 11-12 for the conformity scale (exclude (10) attention check)
Conformity_scale <- merged_data_rm_outliers %>%
  select(Conformism.scale_1, Conformism.scale_2, Conformism.scale_3, Conformism.scale_4, Conformism.scale_5, Conformism.scale_6, Conformism.scale_7, Conformism.scale_8, Conformism.scale_9, Conformism.scale_11, Conformism.scale_12)
  # Compute Cronbach Alpha 
alpha_conformity <- psych::alpha(Conformity_scale)
alpha_conformity$total #Scale is 0.6606792, which is below threshold of 0.7
alpha_conformity$alpha.drop #See which item can be dropped to improve the reliability of the scale 
    #Dropping item 12 will improve the Cronbach alpha (0.7664477>0.7) 
    #Could be because it is reversed, and people are more sensitive to phrasing 
  # Creating a new scale, dropping conformity_scale_12
Conformity_scale <- merged_data_rm_outliers %>%
  select(Conformism.scale_1, Conformism.scale_2, Conformism.scale_3, Conformism.scale_4, Conformism.scale_5, Conformism.scale_6, Conformism.scale_7, Conformism.scale_8, Conformism.scale_9, Conformism.scale_11)
alpha_conformity <- alpha(Conformity_scale)
alpha_conformity$total #Now it is fixed!
  #Compute the average row mean
merged_data_rm_outliers$Avg_conformity <- rowMeans(Conformity_scale)
mean(merged_data_rm_outliers$Avg_conformity)
sd(merged_data_rm_outliers$Avg_conformity)
min(merged_data_rm_outliers$Avg_conformity)
max(merged_data_rm_outliers$Avg_conformity)
merged_data_rm_outliers$Avg_conformity <- as.numeric(merged_data_rm_outliers$Avg_conformity)

# PROCESS
  #Provincial against no norm
df_Provincial_vs_Nonorm <- subset(merged_data_rm_outliers, Norm_Condition %in% c("Provincial", "Nonorm"))
df_Provincial_vs_Nonorm$Provincial_vs_Nonorm <- ifelse(df_Provincial_vs_Nonorm$Norm_Condition %in% c("Provincial"), 1, 0)
process (data = df_Provincial_vs_Nonorm, y = "log_f_v", x = "Provincial_vs_Nonorm", w = "Avg_conformity", cov=c("Quantity"), model = 1, jn=1, seed=123)
    ##Graph
ggplot(df_Provincial_vs_Nonorm, aes(x = Avg_conformity,                                       
                                    y = log_f_v,                                           
                                    color = as.factor(Provincial_vs_Nonorm),                                                
                                    shape = as.factor(Provincial_vs_Nonorm))) +                                       
  geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, fill = "gray80") +    
  labs(x = "Average level of Conformity", y = "Log(Fruits & Vegetables)") +                       
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +                  
  scale_color_manual(name = "Social Norm Condition",                                     
                     labels = c("Provincial Norm", "No Norm"),                    
                     values = c("forestgreen", "lightgreen")) +                              
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

  #Descriptive against no norm
df_Descriptive_vs_Nonorm <- subset(merged_data_rm_outliers, Norm_Condition %in% c("Descriptive", "Nonorm"))
df_Descriptive_vs_Nonorm$Descriptive_vs_Nonorm <- ifelse(df_Descriptive_vs_Nonorm$Norm_Condition %in% c("Descriptive"), 1, 0)
process (data = df_Descriptive_vs_Nonorm, y = "log_f_v", x = "Descriptive_vs_Nonorm", w = "Avg_conformity", cov=c("Quantity"), model = 1, jn=1, seed= 123)
    ##Graph
ggplot(df_Descriptive_vs_Nonorm, aes(x = Avg_conformity,                                       
                                    y = log_f_v,                                           
                                    color = as.factor(Descriptive_vs_Nonorm),                                                
                                    shape = as.factor(Descriptive_vs_Nonorm))) +                                       
  geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, fill = "gray80") +    
  labs(x = "Average level of Conformity", y = "Log(Fruits & Vegetables)") +                       
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +                  
  scale_color_manual(name = "Social Norm Condition",                                     
                     labels = c("Descriptive Norm", "No Norm"),                    
                     values = c("forestgreen", "lightgreen")) +                              
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))


# PROCESS model 1
  #One model 
##recalculating ###
# Assuming merged_data_rm_outliers is your data frame and Norm_Condition 
merged_data_rm_outliers <- merged_data_rm_outliers %>%
  mutate(Condition_numbers = case_when(
    Norm_Condition == "Nonorm" ~ 0,
    Norm_Condition == "Provincial" ~ 1,
    Norm_Condition == "Descriptive" ~ 2,
    TRUE ~ NA_real_  # Handle cases where Norm_Condition doesn't match any of the specified values
  ))

process (data = merged_data_rm_outliers, y = "log_f_v", x = "Condition_numbers", w = "Avg_conformity", cov=c("Quantity"), model = 1, jn=1, mcx = 1, seed= 123)
