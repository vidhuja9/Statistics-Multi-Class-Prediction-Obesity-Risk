setwd("C:\\Users\\Vidhu\\Desktop\\ps Assignments")
getwd()

my_data <- read.csv("data.csv")  # Import dataset
set.seed(123)  # For reproducibility
sample_rows <- my_data[sample(nrow(my_data), 200), ]  # Randomly select 200 rows
head(sample_rows)


write.csv(sample_rows, "sample_200_rows.csv", row.names = FALSE)

#------------------------------Gender Distribution----------------------------------------------

# Data for gender distribution
gender <- c("Male", "Female")
counts <- c(106, 94)

# Create pie chart
pie(counts, labels = paste(gender, " (", round(counts/sum(counts)*100, 2), "%)"), 
    main = "Figure 3.4.2.1: Pie Chart Gender (200 Records)", 
    col = c("#1f77b4", "#ff7f0e"))  # Blue for Male, Orange for Female

# Save the pie chart as a PNG (optional)
png("gender_pie_chart_200.png")
pie(counts, labels = paste(gender, " (", round(counts/sum(counts)*100, 2), "%)"), 
    main = "Figure 3.4.2.1: Pie Chart Gender (200 Records)", 
    col = c("#1f77b4", "#ff7f0e"))
dev.off()

#------------------------------------------------------------------------------------------------

#-------------------------------------------Age Distribution-------------------------------------

# Categorize Age into four levels
sample_rows$Age_Category <- cut(sample_rows$Age, 
                                breaks = c(-Inf, 18, 24, 30, Inf), 
                                labels = c("Less than 18", "18-24", "25-30", "Above 30"),
                                right = FALSE)

# Calculate age distribution
age_distribution <- table(sample_rows$Age_Category)
age_proportion <- prop.table(age_distribution) * 100

# Display age distribution
cat("\nFrequency Distribution of Age Categories (200 Records):\n")
print(age_distribution)
cat("\nPercentage Distribution of Age Categories (200 Records):\n")
print(age_proportion)

# Create age table (Table 3.4.2.2)
age_table <- data.frame(
  Age_Category = c("Less than 18", "18-24", "25-30", "Above 30"),
  Count = as.numeric(age_distribution),
  Percentage = as.numeric(age_proportion)
)

# Display age table
cat("\nTable 3.4.2.2: Age Table\n")
print(age_table)

# Save age table
write.csv(age_table, "age_table_3.4.2.2.csv", row.names = FALSE)

# Age pie chart
age_categories <- c("Less than 18", "18-24", "25-30", "Above 30")
age_counts <- as.numeric(age_distribution)
png("age_pie_chart_200.png")
pie(age_counts, 
    labels = paste(age_categories, " (", round(age_counts/sum(age_counts)*100, 2), "%)"), 
    main = "Figure 3.4.2.2: Pie Chart Age (200 Records)", 
    col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))
dev.off()
#------------------------------------------------------------------------------------------------

#------------------------------------Obesity category Distribution-------------------------------
--
  # CORRECTED: Use NObeyesdad column instead of Weight_Category
  obesity_categories <- c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", 
                          "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III")

# Calculate obesity distribution from sample_rows using NObeyesdad column
obesity_distribution <- table(factor(sample_rows$NObeyesdad, levels = obesity_categories))
total_sample <- nrow(sample_rows)  # Total sample size: 200
percentages <- round(prop.table(obesity_distribution) * 100, 2)

# Create pie chart
pie(as.numeric(obesity_distribution), 
    labels = paste(obesity_categories, " (", percentages, "%)"),
    main = "Figure: Pie Chart of Obesity Categories (200 Records)",
    col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2"))

# Save the pie chart as a PNG
png("obesity_pie_chart_200.png")
pie(as.numeric(obesity_distribution),
    labels = paste(obesity_categories, " (", percentages, "%)"),
    main = "Figure: Pie Chart of Obesity Categories (200 Records)",
    col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2"))
dev.off()

# Create a data frame for the distribution
obesity_table <- data.frame(
  Category = obesity_categories,
  Count = as.numeric(obesity_distribution),
  Percentage = percentages
)

# Display the table
cat("\nObesity Category Distribution (200 Records):\n")
print(obesity_table)

# Save the table as a CSV
write.csv(obesity_table, "obesity_table_200.csv", row.names = FALSE)


#----------------------------------------------------------------------------------------------------------------

install.packages(c("dplyr", "psych", "skimr", "Hmisc"))
library(dplyr)
library(skimr)

#------------------------------Numerical or categorical table --------------------------------------------------
# Get numerical variables
numerical_vars <- names(sample_rows)[sapply(sample_rows, is.numeric)]

categorical_vars <- names(sample_rows)[sapply(sample_rows, function(x) is.character(x) | is.factor(x))]

# Find the maximum length between the two lists
max_length <- max(length(numerical_vars), length(categorical_vars))

# Create the table with proper length handling
variable_table <- data.frame(
  Numerical = c(numerical_vars, rep("", max_length - length(numerical_vars))),
  Categorical = c(categorical_vars, rep("", max_length - length(categorical_vars)))
)

# Display the table
cat("Variable Types Summary\n")
cat("=====================\n")
print(variable_table, row.names = FALSE)

# Save the table
write.csv(variable_table, "variable_types_table.csv", row.names = FALSE)

#---------------------------------------------------------------------------------------------------------------

#----------------------------Categorical Attributes---------------------------------------------------------------------------
#------------------------Family History of obesity--------------------------------------
#------------------------------------Family History of Obesity Distribution-------------------
# Define family history categories
family_history_categories <- c("yes", "no")

# Calculate family history distribution from sample_rows - CORRECTED COLUMN NAME
family_history_distribution <- table(factor(sample_rows$family_history_with_overweight,
                                            levels = family_history_categories))

total_sample <- nrow(sample_rows) # Total sample size: 200
percentages <- round(prop.table(family_history_distribution) * 100, 2)

# Create pie chart
pie(as.numeric(family_history_distribution),
    labels = paste(family_history_categories, " (", percentages, "%)"),
    main = "Figure: Pie Chart of Family History of Overweight (200 Records)",
    col = c("#1f77b4", "#ff7f0e")) # Blue for yes, Orange for no

# Save the pie chart as a PNG
png("family_history_pie_chart_200.png")
pie(as.numeric(family_history_distribution),
    labels = paste(family_history_categories, " (", percentages, "%)"),
    main = "Figure: Pie Chart of Family History of Overweight (200 Records)",
    col = c("#1f77b4", "#ff7f0e"))
dev.off()

# Create a data frame for the distribution
family_history_table <- data.frame(
  Category = family_history_categories,
  Count = as.numeric(family_history_distribution),
  Percentage = percentages
)

# Display the table
cat("\nFamily History of Overweight Distribution (200 Records):\n")
print(family_history_table)

# Save the table as a CSV
write.csv(family_history_table, "family_history_table_200.csv", row.names = FALSE)

# Interpretation
cat("\nInterpretation: This shows the proportion of individuals with family history of overweight/obesity, highlighting potential genetic/environmental factors.\n")
#---------------------------------------------------------------------------------------

#------------------------------------High Caloric Food Consumption (FAVC)-------------------
# Calculate FAVC distribution
favorite_distribution <- table(sample_rows$FAVC)

favorite_percentages <- round(prop.table(favorite_distribution) * 100, 2)

cat("\nHigh Caloric Food Consumption (FAVC):\n")
cat("=====================================\n")

# Create a data frame for the distribution
favorite_table <- data.frame(
  Category = names(favorite_distribution),
  Count = as.numeric(favorite_distribution),
  Percentage = as.numeric(favorite_percentages)
)

# Display the table
print(favorite_table)

# Create pie chart
png("favorite_food_pie_chart_200.png")
pie(favorite_distribution,
    labels = paste(names(favorite_distribution), " (", favorite_percentages, "%)"),
    main = "Figure: High Caloric Food Consumption (FAVC) - 200 Records",
    col = c("#ff7f0e", "#1f77b4"))
dev.off()

# Save the table as CSV
write.csv(favorite_table, "favorite_food_table_200.csv", row.names = FALSE)
cat("\nTable saved as: favorite_food_table_200.csv\n")

# Interpretation
cat("\nInterpretation: The majority of participants (", favorite_percentages["yes"], "%) frequently consume\n")
cat("high-caloric foods, which is a significant modifiable risk factor for obesity and related health issues.\n")

# Save the table as CSV
write.csv(favorite_table, "favorite_food_table_200.csv", row.names = FALSE)

#---------------------------------------------------------------------------------------------------------------------------

#----------------------------------Smoking Status----------------------------------------------------------------------------

# Calculate smoking distribution
smoking_distribution <- table(sample_rows$SMOKE)

smoking_percentages <- round(prop.table(smoking_distribution) * 100, 2)

cat("\nSmoking Status:\n")
cat("===============\n")

# Create table
smoking_table <- data.frame(
  Category = names(smoking_distribution),
  Count = as.numeric(smoking_distribution),
  Percentage = as.numeric(smoking_percentages)
)

# Display table
print(smoking_table)

# Display results
cat("No:", smoking_distribution["no"], "individuals (", smoking_percentages["no"], "%)\n")
cat("Yes:", smoking_distribution["yes"], "individuals (", smoking_percentages["yes"], "%)\n")

# Save pie chart
png("smoking_pie_chart_200.png", width = 800, height = 600)
pie(smoking_distribution,
    labels = paste(names(smoking_distribution), " (", smoking_percentages, "%)"),
    main = "Smoking Status - 200 Records",
    col = c("#1f77b4", "#ff7f0e"))
dev.off()

cat("Pie chart saved as: smoking_pie_chart_200.png\n")

#-----------------------------------------------------------------------------------------------------

#------------------------------------Calorie Monitoring (SCC)----------------------------------------------
# Calculate SCC distribution
calorie_distribution <- table(sample_rows$SCC)
calorie_percentages <- round(prop.table(calorie_distribution) * 100, 2)

cat("\nCalorie Monitoring (SCC):\n")
cat("=========================\n")

# Create table
calorie_table <- data.frame(
  Category = names(calorie_distribution),
  Count = as.numeric(calorie_distribution),
  Percentage = as.numeric(calorie_percentages)
)

# Display table
print(calorie_table)

# Save table as CSV
write.csv(calorie_table, "calorie_monitoring_table_200.csv", row.names = FALSE)
cat("\nTable saved as: calorie_monitoring_table_200.csv\n")

# Save pie chart
png("calorie_monitoring_pie_chart_200.png", width = 800, height = 600)
pie(calorie_distribution,
    labels = paste(names(calorie_distribution), " (", calorie_percentages, "%)"),
    main = "Calorie Monitoring (SCC) - 200 Records",
    col = c("#ff7f0e", "#1f77b4"))
dev.off()
cat("Pie chart saved as: calorie_monitoring_pie_chart_200.png\n")

#--------------------------------------------------------------------------------------------------

#------------------------------------Food Between Meals (CAEC)-------------------
# Calculate CAEC distribution
caec_distribution <- table(sample_rows$CAEC)
caec_percentages <- round(prop.table(caec_distribution) * 100, 2)

cat("\nFood Between Meals (CAEC):\n")
cat("==========================\n")

# Create table
caec_table <- data.frame(
  Category = names(caec_distribution),
  Count = as.numeric(caec_distribution),
  Percentage = as.numeric(caec_percentages)
)

# Display table
print(caec_table)

# Save table as CSV
write.csv(caec_table, "food_between_meals_table_200.csv", row.names = FALSE)
cat("\nTable saved as: food_between_meals_table_200.csv\n")

# Save pie chart
png("food_between_meals_pie_chart_200.png", width = 800, height = 600)
pie(caec_distribution,
    labels = paste(names(caec_distribution), " (", caec_percentages, "%)"),
    main = "Food Between Meals (CAEC) - 200 Records",
    col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))
dev.off()
cat("Pie chart saved as: food_between_meals_pie_chart_200.png\n")

#----------------------------------------------------------------------------------------------

#------------------------------------Transportation Mode (MTRANS)-------------------
# Calculate MTRANS distribution
mtrans_distribution <- table(sample_rows$MTRANS)
mtrans_percentages <- round(prop.table(mtrans_distribution) * 100, 2)

cat("\nTransportation Mode (MTRANS):\n")
cat("=============================\n")

# Create table
mtrans_table <- data.frame(
  Category = names(mtrans_distribution),
  Count = as.numeric(mtrans_distribution),
  Percentage = as.numeric(mtrans_percentages)
)

# Display table
print(mtrans_table)

# Save table as CSV
write.csv(mtrans_table, "transportation_mode_table_200.csv", row.names = FALSE)
cat("\nTable saved as: transportation_mode_table_200.csv\n")

# Save pie chart
png("transportation_mode_pie_chart_200.png", width = 800, height = 600)
pie(mtrans_distribution,
    labels = paste(names(mtrans_distribution), " (", mtrans_percentages, "%)"),
    main = "Transportation Mode (MTRANS) - 200 Records",
    col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"))
dev.off()
cat("Pie chart saved as: transportation_mode_pie_chart_200.png\n")
#----------------------------------------------------------------------------------------------------






#-----------------------------Numerical Attribute----------------------------------------------------------

#------------------------------------Age Analysis - Complete-------------------
# Create numerical values for each age category
age_mapping <- data.frame(
  Age_Category = c("Less than 18", "18-24", "25-30", "Above 30"),
  Numerical_Value = c(17, 21, 28, 31)
)

# Map age categories to numerical values
sample_rows$Age_Numerical <- age_mapping$Numerical_Value[match(sample_rows$Age_Category, age_mapping$Age_Category)]

# 1. CREATE ORDERED ARRAY
ordered_age_array <- sort(sample_rows$Age_Numerical)

cat("Ordered Age Array:\n")
cat("==================\n")
cat(paste(ordered_age_array, collapse = ","))
cat("\n\nTotal values:", length(ordered_age_array), "\n\n")

# Save ordered array
writeLines(paste(ordered_age_array, collapse = ","), "ordered_age_array.txt")
cat("Ordered array saved as: ordered_age_array.txt\n\n")

# 2. CREATE FREQUENCY TABLE
age_freq_table <- table(sample_rows$Age_Category)
age_numerical_table <- data.frame(
  Age_Category = names(age_freq_table),
  Frequency = as.numeric(age_freq_table),
  Value = age_mapping$Numerical_Value[match(names(age_freq_table), age_mapping$Age_Category)]
)

cat("Age Category Table:\n")
cat("===================\n")
print(age_numerical_table)
cat("\n")

# Save frequency table
write.csv(age_numerical_table, "age_numerical_table.csv", row.names = FALSE)
cat("Age table saved as: age_numerical_table.csv\n\n")

# 3. CREATE STATISTICS TABLE
age_stats_table <- data.frame(
  Statistic = c("Mean", "Mode", "Median", "Range", "Standard Deviation", "Variance"),
  Value = c(
    round(mean(sample_rows$Age_Numerical), 2),
    as.numeric(names(sort(-table(sample_rows$Age_Numerical)))[1]),
    median(sample_rows$Age_Numerical),
    max(sample_rows$Age_Numerical) - min(sample_rows$Age_Numerical),
    round(sd(sample_rows$Age_Numerical), 2),
    round(var(sample_rows$Age_Numerical), 2)
  )
)

cat("Age Statistics Table:\n")
cat("=====================\n")
print(age_stats_table)
cat("\n")

# Save statistics table
write.csv(age_stats_table, "age_statistics_table.csv", row.names = FALSE)
cat("Statistics table saved as: age_statistics_table.csv\n\n")

# 4. CREATE HISTOGRAM
png("age_categorical_histogram_200.png", width = 800, height = 600)
hist(sample_rows$Age_Numerical, 
     main = "Age Distribution (Categorical Values) - 200 Records",
     xlab = "Age Categories",
     ylab = "Frequency",
     col = "#2ca02c",
     border = "white",
     breaks = c(16, 19, 24, 29, 32),
     xaxt = "n")
axis(1, at = c(17, 21, 28, 31), labels = c("<18", "18-24", "25-30", ">30"))
dev.off()
cat("Histogram saved as: age_categorical_histogram_200.png\n")

#-------------------------------------------------------------------------------------------------------

#------------------------------------Height Analysis-------------------
# Calculate height statistics
height_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Variance", "Range", "Minimum", "Maximum"),
  Value = c(
    round(mean(sample_rows$Height), 2),
    round(median(sample_rows$Height), 2),
    round(sd(sample_rows$Height), 4),
    round(var(sample_rows$Height), 4),
    paste(round(min(sample_rows$Height), 2), "-", round(max(sample_rows$Height), 2)),
    round(min(sample_rows$Height), 2),
    round(max(sample_rows$Height), 2)
  )
)

cat("\nHeight Analysis:\n")
cat("================\n")
print(height_stats)

# Save table as CSV
write.csv(height_stats, "height_analysis_table_200.csv", row.names = FALSE)
cat("\nTable saved as: height_analysis_table_200.csv\n")

# Create histogram
png("height_histogram_200.png", width = 800, height = 600)
hist(sample_rows$Height, 
     main = "Height Distribution - 200 Records",
     xlab = "Height (meters)",
     ylab = "Frequency",
     col = "#1f77b4",
     border = "white",
     breaks = 15)
dev.off()
cat("Histogram saved as: height_histogram_200.png\n")

# Create boxplot
png("height_boxplot_200.png", width = 800, height = 600)
boxplot(sample_rows$Height,
        main = "Height Boxplot - 200 Records",
        ylab = "Height (meters)",
        col = "#ff7f0e")
dev.off()
cat("Boxplot saved as: height_boxplot_200.png\n") 
#--------------------------------------------------------------------------------------------------

#------------------------------------Weight Analysis-------------------
# Calculate weight statistics
weight_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Variance", "Range", "Minimum", "Maximum"),
  Value = c(
    round(mean(sample_rows$Weight), 2),
    round(median(sample_rows$Weight), 2),
    round(sd(sample_rows$Weight), 2),
    round(var(sample_rows$Weight), 2),
    paste(round(min(sample_rows$Weight), 0), "-", round(max(sample_rows$Weight), 0)),
    round(min(sample_rows$Weight), 2),
    round(max(sample_rows$Weight), 2)
  )
)

cat("\nWeight Analysis:\n")
cat("================\n")
print(weight_stats)

# Save table as CSV
write.csv(weight_stats, "weight_analysis_table_200.csv", row.names = FALSE)
cat("\nTable saved as: weight_analysis_table_200.csv\n")

# Create histogram
png("weight_histogram_200.png", width = 800, height = 600)
hist(sample_rows$Weight, 
     main = "Weight Distribution - 200 Records",
     xlab = "Weight (kg)",
     ylab = "Frequency",
     col = "#1f77b4",
     border = "white",
     breaks = 20)
dev.off()
cat("Histogram saved as: weight_histogram_200.png\n")

# Create boxplot
png("weight_boxplot_200.png", width = 800, height = 600)
boxplot(sample_rows$Weight,
        main = "Weight Boxplot - 200 Records",
        ylab = "Weight (kg)",
        col = "#ff7f0e")
dev.off()
cat("Boxplot saved as: weight_boxplot_200.png\n")

# Create ordered array
ordered_weight_array <- sort(sample_rows$Weight)
cat("\nFirst 10 weights:", paste(round(ordered_weight_array[1:10], 1), collapse = ", "), "\n")
cat("Last 10 weights:", paste(round(ordered_weight_array[(length(ordered_weight_array)-9):length(ordered_weight_array)], 1), collapse = ", "), "\n")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------BMI Calculation and Analysis-------------------
# Calculate BMI
sample_rows$BMI <- sample_rows$Weight / (sample_rows$Height^2)

# Calculate BMI statistics
bmi_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Variance", "Range", "Minimum", "Maximum"),
  Value = c(
    round(mean(sample_rows$BMI), 2),
    round(median(sample_rows$BMI), 2),
    round(sd(sample_rows$BMI), 2),
    round(var(sample_rows$BMI), 2),
    paste(round(min(sample_rows$BMI), 2), "-", round(max(sample_rows$BMI), 2)),
    round(min(sample_rows$BMI), 2),
    round(max(sample_rows$BMI), 2)
  )
)

cat("\nBMI Calculation and Analysis:\n")
cat("==============================\n")
cat("BMI Formula: Weight (kg) / HeightÂ² (mÂ²)\n\n")
print(bmi_stats)

# Save table as CSV
write.csv(bmi_stats, "bmi_analysis_table_200.csv", row.names = FALSE)
cat("\nTable saved as: bmi_analysis_table_200.csv\n")

# Create histogram
png("bmi_histogram_200.png", width = 800, height = 600)
hist(sample_rows$BMI, 
     main = "BMI Distribution - 200 Records",
     xlab = "BMI (kg/mÂ²)",
     ylab = "Frequency",
     col = "#1f77b4",
     border = "white",
     breaks = 20)
dev.off()
cat("Histogram saved as: bmi_histogram_200.png\n")

# Create boxplot
png("bmi_boxplot_200.png", width = 800, height = 600)
boxplot(sample_rows$BMI,
        main = "BMI Boxplot - 200 Records",
        ylab = "BMI (kg/mÂ²)",
        col = "#ff7f0e")
dev.off()
cat("Boxplot saved as: bmi_boxplot_200.png\n")

# Show first few BMI values
cat("\nFirst 10 BMI values:\n")
print(round(head(sample_rows$BMI, 10), 2))

#-------------------------------------------------------------------------------


#------------------------------------BMI Categories Distribution-------------------
# Create BMI categories
sample_rows$BMI_Category <- cut(sample_rows$BMI,
                                breaks = c(0, 18.5, 25, 30, Inf),
                                labels = c("Underweight", "Normal", "Overweight", "Obese"),
                                right = FALSE)

# Calculate BMI category distribution
bmi_category_dist <- table(sample_rows$BMI_Category)
bmi_category_percent <- round(prop.table(bmi_category_dist) * 100, 2)

cat("\nBMI Categories Distribution:\n")
cat("=============================\n")

# Create BMI category table
bmi_category_table <- data.frame(
  Category = names(bmi_category_dist),
  Count = as.numeric(bmi_category_dist),
  Percentage = as.numeric(bmi_category_percent)
)

print(bmi_category_table)

# Save table as CSV
write.csv(bmi_category_table, "bmi_categories_table_200.csv", row.names = FALSE)
cat("\nTable saved as: bmi_categories_table_200.csv\n")

# Create pie chart
png("bmi_categories_pie_chart_200.png", width = 800, height = 600)
pie(bmi_category_dist,
    labels = paste(names(bmi_category_dist), " (", bmi_category_percent, "%)"),
    main = "BMI Categories Distribution - 200 Records",
    col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))
dev.off()
cat("Pie chart saved as: bmi_categories_pie_chart_200.png\n")

# Create bar plot
png("bmi_categories_bar_200.png", width = 800, height = 600)
barplot(bmi_category_dist,
        main = "BMI Categories Distribution - 200 Records",
        xlab = "BMI Categories",
        ylab = "Count",
        col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
        ylim = c(0, max(bmi_category_dist) + 20))
dev.off()
cat("Bar plot saved as: bmi_categories_bar_200.png\n")

#=------------------------------------------------------------------------------------------------


#------------------------------------Physical Activity Frequency (FAF)-------------------
# Calculate FAF statistics
faf_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "Range"),
  Value = c(
    round(mean(sample_rows$FAF), 2),
    round(median(sample_rows$FAF), 2),
    round(sd(sample_rows$FAF), 2),
    round(min(sample_rows$FAF), 2),
    round(max(sample_rows$FAF), 2),
    paste(round(min(sample_rows$FAF), 2), "-", round(max(sample_rows$FAF), 2))
  )
)

cat("\nPhysical Activity Frequency (FAF):\n")
cat("===================================\n")
print(faf_stats)

# Save table as CSV
write.csv(faf_stats, "physical_activity_table_200.csv", row.names = FALSE)
cat("\nTable saved as: physical_activity_table_200.csv\n")

# Create histogram
png("physical_activity_histogram_200.png", width = 800, height = 600)
hist(sample_rows$FAF, 
     main = "Physical Activity Frequency Distribution - 200 Records",
     xlab = "Physical Activity Frequency (0-3 scale)",
     ylab = "Frequency",
     col = "#1f77b4",
     border = "white",
     breaks = 10,
     xlim = c(0, 3))
dev.off()
cat("Histogram saved as: physical_activity_histogram_200.png\n")

# Create boxplot
png("physical_activity_boxplot_200.png", width = 800, height = 600)
boxplot(sample_rows$FAF,
        main = "Physical Activity Frequency - 200 Records",
        ylab = "Physical Activity Frequency (0-3 scale)",
        col = "#ff7f0e")
dev.off()
cat("Boxplot saved as: physical_activity_boxplot_200.png\n")

# Show frequency distribution
cat("\nPhysical Activity Frequency Counts:\n")
faf_counts <- table(sample_rows$FAF)
faf_percentages <- round(prop.table(faf_counts) * 100, 2)
faf_freq_table <- data.frame(
  Activity_Level = names(faf_counts),
  Count = as.numeric(faf_counts),
  Percentage = as.numeric(faf_percentages)
)
print(faf_freq_table)

#-------------------------------------------------------------------------------------------

#------------------------------------Water Consumption (CH2O)-------------------
# Calculate CH2O statistics
water_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "Range"),
  Value = c(
    round(mean(sample_rows$CH2O), 2),
    round(median(sample_rows$CH2O), 2),
    round(sd(sample_rows$CH2O), 2),
    round(min(sample_rows$CH2O), 2),
    round(max(sample_rows$CH2O), 2),
    paste(round(min(sample_rows$CH2O), 2), "-", round(max(sample_rows$CH2O), 2))
  )
)

cat("\nWater Consumption (CH2O):\n")
cat("==========================\n")
print(water_stats)

# Save table as CSV
write.csv(water_stats, "water_consumption_table_200.csv", row.names = FALSE)

cat("\nTable saved as: water_consumption_table_200.csv\n")

# Create histogram
png("water_consumption_histogram_200.png", width = 800, height = 600)
hist(sample_rows$CH2O, 
     main = "Water Consumption Distribution - 200 Records",
     xlab = "Water Consumption (liters/day)",
     ylab = "Frequency",
     col = "#1f77b4",
     border = "white",
     breaks = 15)
dev.off()
cat("Histogram saved as: water_consumption_histogram_200.png\n")

# Create boxplot
png("water_consumption_boxplot_200.png", width = 800, height = 600)
boxplot(sample_rows$CH2O,
        main = "Water Consumption - 200 Records",
        ylab = "Water Consumption (liters/day)",
        col = "#ff7f0e")
dev.off()
cat("Boxplot saved as: water_consumption_boxplot_200.png\n")

# Categorize water consumption
sample_rows$Water_Category <- cut(sample_rows$CH2O,
                                  breaks = c(0, 1.5, 2.5, Inf),
                                  labels = c("Low (<1.5L)", "Moderate (1.5-2.5L)", "High (>2.5L)"),
                                  right = FALSE)

# Show category distribution
cat("\nWater Consumption Categories:\n")
water_cat_dist <- table(sample_rows$Water_Category)
water_cat_percent <- round(prop.table(water_cat_dist) * 100, 2)
water_cat_table <- data.frame(
  Category = names(water_cat_dist),
  Count = as.numeric(water_cat_dist),
  Percentage = as.numeric(water_cat_percent)
)
print(water_cat_table)

#--------------------------------------------------------------------

#------------------------------------Number of Main Meals (NCP)-------------------
# Calculate NCP statistics
ncp_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "Range"),
  Value = c(
    round(mean(sample_rows$NCP), 2),
    round(median(sample_rows$NCP), 2),
    round(sd(sample_rows$NCP), 2),
    round(min(sample_rows$NCP), 2),
    round(max(sample_rows$NCP), 2),
    paste(round(min(sample_rows$NCP), 2), "-", round(max(sample_rows$NCP), 2))
  )
)

cat("\nNumber of Main Meals (NCP):\n")
cat("============================\n")
print(ncp_stats)

# Save table as CSV
write.csv(ncp_stats, "main_meals_table_200.csv", row.names = FALSE)
cat("\nTable saved as: main_meals_table_200.csv\n")

# Create histogram
png("main_meals_histogram_200.png", width = 800, height = 600)
hist(sample_rows$NCP, 
     main = "Number of Main Meals Distribution - 200 Records",
     xlab = "Number of Main Meals per Day",
     ylab = "Frequency",
     col = "#1f77b4",
     border = "white",
     breaks = 10)
dev.off()
cat("Histogram saved as: main_meals_histogram_200.png\n")

# Create boxplot
png("main_meals_boxplot_200.png", width = 800, height = 600)
boxplot(sample_rows$NCP,
        main = "Number of Main Meals - 200 Records",
        ylab = "Number of Main Meals per Day",
        col = "#ff7f0e")
dev.off()
cat("Boxplot saved as: main_meals_boxplot_200.png\n")

# Categorize meal frequency
sample_rows$Meal_Category <- cut(sample_rows$NCP,
                                 breaks = c(0, 2, 3, Inf),
                                 labels = c("Low (1-2 meals)", "Moderate (3 meals)", "High (>3 meals)"),
                                 right = FALSE)

# Show category distribution
cat("\nMeal Frequency Categories:\n")
meal_cat_dist <- table(sample_rows$Meal_Category)
meal_cat_percent <- round(prop.table(meal_cat_dist) * 100, 2)
meal_cat_table <- data.frame(
  Category = names(meal_cat_dist),
  Count = as.numeric(meal_cat_dist),
  Percentage = as.numeric(meal_cat_percent)
)
print(meal_cat_table)

# Show frequency distribution of exact meal counts
cat("\nExact Meal Count Distribution:\n")
ncp_counts <- table(sample_rows$NCP)
ncp_percentages <- round(prop.table(ncp_counts) * 100, 2)
ncp_freq_table <- data.frame(
  Meals = names(ncp_counts),
  Count = as.numeric(ncp_counts),
  Percentage = as.numeric(ncp_percentages)
)
print(ncp_freq_table)

#--------------------------------------------------------------------------------------------------------------------------------

#------------------------------------Female Underweight Test-------------------
# For sample_rows data
sample_rows$BMI <- sample_rows$Weight / (sample_rows$Height^2)
female_data <- subset(sample_rows, Gender == "Female")
resultFemale <- t.test(female_data$BMI, mu = 18.5, alternative = "less")
print(resultFemale)

#------------------------------------Male Underweight Test-------------------
# For sample_rows data
sample_rows$BMI <- sample_rows$Weight / (sample_rows$Height^2)
male_data <- subset(sample_rows, Gender == "Male")
resultMale <- t.test(male_data$BMI, mu = 18.5, alternative = "less")
print(resultMale)

#------------------------------------Female Overweight Test-------------------
# For sample_rows data
sample_rows$BMI <- sample_rows$Weight / (sample_rows$Height^2)
female_data <- subset(sample_rows, Gender == "Female")
resultFemaleOverweight <- t.test(female_data$BMI, mu = 25, alternative = "greater")
print(resultFemaleOverweight)

#------------------------------------Male Overweight Test-------------------
# For sample_rows data
sample_rows$BMI <- sample_rows$Weight / (sample_rows$Height^2)
male_data <- subset(sample_rows, Gender == "Male")
resultMaleOverweight <- t.test(male_data$BMI, mu = 25, alternative = "greater")
print(resultMaleOverweight)

#------------------------------------Gender Comparison Test-------------------------
# For sample_rows data
sample_rows$BMI <- sample_rows$Weight / (sample_rows$Height^2)
male_data <- subset(sample_rows, Gender == "Male")
female_data <- subset(sample_rows, Gender == "Female")
resultGender <- t.test(male_data$BMI, female_data$BMI, alternative = "two.sided")
print(resultGender)

#------------------------------------Gender vs Obesity Category Test-------------------
# For sample_rows data
table_gender_obesity <- table(sample_rows$Gender, sample_rows$NObeyesdad)
resultChiSq <- chisq.test(table_gender_obesity)
print(resultChiSq)




# R Code for High Caloric Food vs Obesity Category-------------------------------------------------

table_food_obesity <- table(sample_rows$FAVC, sample_rows$NObeyesdad)
resultChiSq <- chisq.test(table_food_obesity)
print(resultChiSq)

# R Code for Smoking vs Obesity Category
table_smoking_obesity <- table(sample_rows$SMOKE, sample_rows$NObeyesdad)
resultChiSq <- chisq.test(table_smoking_obesity)
print(resultChiSq)


# --------------------------Gender obesity---------------------------------------

cat("\n=== Test 01: Gender vs Obesity Category ===\n\n")

# Create contingency table
table_gender_obesity <- table(sample_rows$Gender, sample_rows$NObeyesdad)

# Display the table
print("Contingency Table:")
print(table_gender_obesity)

# Add row and column totals
table_gender_obesity_with_totals <- addmargins(table_gender_obesity)
print("\nContingency Table with Totals:")
print(table_gender_obesity_with_totals)

# Convert to data frame for better display
df_gender <- as.data.frame.matrix(table_gender_obesity)
df_gender$Total <- rowSums(df_gender)
df_gender <- rbind(df_gender, Total = colSums(rbind(df_gender)))
print("\nFormatted Table:")
print(df_gender)

# Perform chi-square test
resultChiSq <- chisq.test(table_gender_obesity)
print("\nChi-square Test Results:")
print(resultChiSq)



# -------------------------------Test 02: Family History vs Obesity Category

cat("\n=== Test 02: Family History vs Obesity Category ===\n\n")

# Create contingency table
table_family_obesity <- table(sample_rows$family_history_with_overweight, sample_rows$NObeyesdad)

# Create formatted data frame
df_family <- as.data.frame.matrix(table_family_obesity)

# Add Total column
df_family$Total <- rowSums(df_family)

# Add Total row
df_family <- rbind(df_family, Total = colSums(df_family))

# Rename columns and rows
colnames(df_family) <- c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", 
                         "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", 
                         "Obesity_Type_III", "Total")
rownames(df_family) <- c("No", "Yes", "Total")

# Display the formatted table with table name
cat("Table 2: Family History vs Obesity Category Contingency Table\n")
cat("=============================================================\n")
print(df_family)

# Perform chi-square test
resultFamilyChiSq <- chisq.test(table_family_obesity)
cat("\nChi-square Test Results:\n")
cat("========================\n")
print(resultFamilyChiSq)


# -------------------------------Test 03: High Caloric Food vs Obesity Category

cat("\n=== Test 03: High Caloric Food vs Obesity Category ===\n\n")

# Create contingency table
table_food_obesity <- table(sample_rows$FAVC, sample_rows$NObeyesdad)

# Create formatted data frame
df_food <- as.data.frame.matrix(table_food_obesity)

# Add Total column
df_food$Total <- rowSums(df_food)

# Add Total row
df_food <- rbind(df_food, Total = colSums(df_food))

# Rename columns and rows
colnames(df_food) <- c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", 
                       "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", 
                       "Obesity_Type_III", "Total")
rownames(df_food) <- c("No", "Yes", "Total")

# Display the formatted table with table name
cat("Table 3: High Caloric Food vs Obesity Category Contingency Table\n")
cat("===============================================================\n")
print(df_food)

# Perform chi-square test
resultFoodChiSq <- chisq.test(table_food_obesity)
cat("\nChi-square Test Results:\n")
cat("========================\n")
print(resultFoodChiSq)

#------------------------test 4: smoking vs Obesity Category -----------------------------------

cat("\n\n=== Test 05: Smoking vs Obesity Category ===\n\n")

# Create contingency table
table_smoking_obesity <- table(sample_rows$SMOKE, sample_rows$NObeyesdad)

# Create formatted data frame
df_smoking <- as.data.frame.matrix(table_smoking_obesity)

# Add Total column
df_smoking$Total <- rowSums(df_smoking)

# Add Total row
df_smoking <- rbind(df_smoking, Total = colSums(df_smoking))

# Rename columns and rows
colnames(df_smoking) <- c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", 
                          "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", 
                          "Obesity_Type_III", "Total")
rownames(df_smoking) <- c("No", "Yes", "Total")

# Display the formatted table with table name
cat("Table 5: Smoking vs Obesity Category Contingency Table\n")
cat("======================================================\n")
print(df_smoking)

# Perform chi-square test
resultSmokingChiSq <- chisq.test(table_smoking_obesity)
cat("\nChi-square Test Results:\n")
cat("========================\n")
print(resultSmokingChiSq)