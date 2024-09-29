# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)



####################################
#Section 1: Date Cleanup
####################################

# Load the datasets
demographics <- read_csv("Documents/GitHub/apprentice-data-analysis/demographics.csv")
grades <- read_csv("Documents/GitHub/apprentice-data-analysis/grades.csv")
transactions <- read_csv("Documents/GitHub/apprentice-data-analysis/transactions.csv")

# Clean up column names to avoid issues (remove spaces, standardize)
colnames(demographics) <- gsub(" ", "_", colnames(demographics))
#View(demographics)
colnames(grades) <- gsub(" ", "_", colnames(grades))
#View(grades)
colnames(transactions) <- gsub(" ", "_", colnames(transactions))
#View(transactions)

# Rename columns to ensure a common key across datasets
#demographics <- demographics %>% rename('LTI ID' = 'LTI_ID')
#grades <- grades %>% rename('LTI ID' = 'LTI_ID')
transactions <- transactions %>% rename('LTI_ID' = 'lti_user_id')
#View(transactions)

# Remove duplicate rows in demographics
demographics <- demographics %>% distinct()

# Remove duplicate rows in grades
grades <- grades %>% distinct()

# Remove duplicate rows in transactions
transactions <- transactions %>% distinct()

# Remove System_Availability, Student Type, Residency Code, High School, Attempt Date, Attempt Status
grades <- grades %>%
  select(-Student_Type, -Residency_Code, -Residency_Code, -High_School, -Attempt_Date, -Attempt_Status, -System_Availability, -Availability, -Type )


# Filter grades to only show rows with "Display_Column_Name" = "Course Average"
filtered_data <- grades %>%
  filter(Display_Column_Name == 'Course Average')

# Filter out rows where LTI_ID is NA in both datasets
demographics <- demographics %>%
  filter(!is.na(LTI_ID) & LTI_ID != "null" & LTI_ID != "")

grades <- grades %>%
  filter(!is.na(LTI_ID) & LTI_ID != "null" & LTI_ID != "")

transactions <- transactions %>%
  filter(!is.na(LTI_ID) & LTI_ID != "null" & LTI_ID != "")

#View(transactions)

# Calculate the number of occurrences for each unique LTI_ID in the transactions dataset
transaction_counts <- transactions %>%
  group_by(LTI_ID) %>%
  summarise(Transaction_Count = n())

# Make sure filtered data is distinct
filtered_data <- filtered_data %>% distinct()

# Join the datasets
merged_data <- demographics %>%
  inner_join(filtered_data, by = "LTI_ID", relationship = "many-to-many")

#View(merged_data)

# Perform the join between merged_data and transaction_counts
merged_data_with_transactions <- merged_data %>%
  left_join(transaction_counts, by = "LTI_ID")

# View the merged dataset with transaction counts
#View(merged_data_with_transactions)

# Remove duplicate rows in grades
merged_data_with_transactions <- merged_data_with_transactions %>% distinct()

# Save the merged dataset to a CSV file
write_csv(merged_data_with_transactions, "Documents/GitHub/apprentice-data-analysis/merged_data_with_transactions.csv")


# filter outliers
#merged_data_with_transactions <- merged_data_with_transactions[merged_data_with_transactions$Transaction_Count<1500,]

####################################
#Section 3: Demographics Data
####################################

# Filter and categorize users as tutor and non-tutor based on Transaction_Count
#demographic_dataset <- merged_data_with_transactions %>%
#  mutate(Tutor_Usage = ifelse(is.na(Transaction_Count), "Non-Tutor Users", "Tutor Users"))

#View(demographics)

# Categorize Gender with a catch-all for "Others"
demographic_dataset$Gender <- ifelse(demographic_dataset$Gender %in% c("M", "F"), 
                                     demographic_dataset$Gender, "Others")

# Calculate the count of users by Tutor Usage and Gender
gender_tutor_data <- demographic_dataset %>%
  group_by(Tutor_Usage, Gender) %>%
  summarise(User_Count = n(), .groups = 'drop')

# Calculate the percentage of users within each Tutor Usage group by Gender
gender_tutor_data <- gender_tutor_data %>%
  group_by(Tutor_Usage) %>%
  mutate(Percentage = (User_Count / sum(User_Count)) * 100)

# View the calculated percentages
print(gender_tutor_data)

# Create a multi-bar plot with percentages
ggplot(gender_tutor_data, aes(x = Tutor_Usage, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Gender Distribution Among Tutor Users vs Non-Tutor Users", 
       x = "Tutor Usage", 
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +  # Adjust colors as needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4)



# Categorize Race with a catch-all for "Others"
demographic_dataset$Race <- ifelse(demographic_dataset$Race %in% c("Asian", "White", "Black or African American", "Hispanic/Latino", "Two or more races"),
                                   demographic_dataset$Race, "Others")

# Calculate the count of users in each group (by Race and Tutor Usage)
race_tutor_data <- demographic_dataset %>%
  group_by(Race, Tutor_Usage) %>%
  summarise(User_Count = n(), .groups = 'drop')

# Calculate the percentage of users within each Tutor_Usage group by Race
race_tutor_data <- race_tutor_data %>%
  group_by(Tutor_Usage) %>%
  mutate(Percentage = User_Count / sum(User_Count) * 100)

# View the updated data with percentages
print(race_tutor_data)

# Create a multi-bar plot with percentages
ggplot(race_tutor_data, aes(x = Race, y = Percentage, fill = Tutor_Usage)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Tutor Usage Distribution by Race", 
       x = "Race", 
       y = "Percentage (%) within Tutor Usage") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +  # Adjust colors as needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4)




#AGE DATA
# Calculate mean age for each Tutor_Usage group
age_data <- demographic_dataset %>%
  group_by(Tutor_Usage) %>%
  summarise(Mean_Age = mean(Term_Age, na.rm = TRUE))

# View the calculated mean ages
print(age_data)

# Create a bar plot with mean ages
ggplot(age_data, aes(x = Tutor_Usage, y = Mean_Age, fill = Tutor_Usage)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Mean Age of Tutor Users vs Non-Tutor Users", x = "Group", y = "Mean Age") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  geom_text(aes(label = round(Mean_Age, 1)), vjust = -0.5, size = 5)  



####################################
#Section 3: Graphs
####################################

##################
#Histogram of transaction count
##################
hist(merged_data_with_transactions$Transaction_Count)

##################
#Overall Grade vs. Tutor Transaction
##################
# Replace NA in Transaction_Count with 0 for plotting purposes
merged_data_with_transactions <- merged_data_with_transactions %>%
  mutate(Transaction_Count = replace_na(Transaction_Count, 0),
         Percentage = as.numeric(gsub("%", "", Percentage)))  # Convert Percentage to numeric

# Filter data where Transaction_Count is between 1 and 100
filtered_data <- merged_data_with_transactions %>%
  filter(Transaction_Count > 0 & Transaction_Count < 100)

# Create the scatter plot with a line of best fit
ggplot(filtered_data, aes(x = Transaction_Count, y = Percentage)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add line of best fit (linear model)
  labs(
    title = "Overall Grade vs Tutor Transaction Count (Filtered with Best Fit Line)",
    x = "Tutor Transaction Count",
    y = "Overall Grade (%)"
  ) +
  theme_minimal()

##################
#Correlation between tutor and grades
##################
# Perform linear regression
model <- lm(Percentage ~ Transaction_Count, data = merged_data_with_transactions)

# View the summary of the linear regression model
summary(model)


##################
#Histogram of transaction count by tutor
##################

################# Assuming your dataset is named transactions
transactions_summary <- transactions %>%
  group_by(tutor) %>%
  summarise(transactions_count = n())

# Create a bar chart of transaction counts by tutor
ggplot(transactions_summary, aes(x = tutor, y = transactions_count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Number of Transactions by Tutor",
       x = "Tutor",
       y = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(transactions_summary$tutor)

################# Grouping data into main tutors

# Define the categories and the mapping
category_mapping <- function(tutor) {
  case_when(
    tutor %in% c("logarithms", "htn_logarithms") ~ "logarithms",
    
    tutor %in% c("exponents", "htn_exponents") ~ "exponents",
    
    tutor %in% c("htn_exponential_equations", "adaptive_scaffolded_exponential_equations", "exponential_equations") ~ "exponential equations",
    
    tutor %in% c("rational_equation", "htn_rational_equation") ~ "rational_equations",
    
    tutor %in% c("quadratic_equations", "htn_quadratic_equations") ~ "quadratic_equations",
    
    tutor %in% c("quadratic_functions") ~ "quadratic_functions",
    
    tutor %in% c("logarithmic_equations", "htn_logarithmic_equations") ~ "logarithmic_equations",
    
    tutor %in% c("factoring_polynomials", "factoring", "htn_factoring_polynomials") ~ "factoring_polynomials",
    
    tutor %in% c("radicals", "htn_radicals") ~ "radicals"
  )
}

# Apply the mapping to the dataset
transactions_with_categories <- transactions %>%
  mutate(category = category_mapping(tutor))

# Group the transactions by category and count the number of transactions
category_summary <- transactions_with_categories %>%
  group_by(category) %>%
  summarise(transactions_count = n())

ggplot(category_summary, aes(x = category, y = transactions_count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = transactions_count), vjust = -0.5) +  # Add labels for each bar
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Number of Transactions by Category",
       x = "Category",
       y = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################ Group into tutor usage by unit

# Define the units and the tutor-to-unit mapping
unit_mapping <- function(tutor) {
  case_when(
    tutor %in% c("exponents", "htn_exponents", "radicals", "htn_radicals", 
                 "factoring_polynomials", "factoring", "htn_factoring_polynomials", 
                 "rational_equation", "htn_rational_equation") ~ "Unit 1",
    
    tutor %in% c("quadratic_equations", "htn_quadratic_equations") ~ "Unit 2",
    
    tutor %in% c("quadratic_functions") ~ "Unit 3",
    
    tutor %in% c("htn_exponential_equations", "adaptive_scaffolded_exponential_equations", 
                 "exponential_equations", "logarithms", "htn_logarithms", 
                 "logarithmic_equations", "htn_logarithmic_equations") ~ "Unit 4"
  )
}

# Apply the mapping to the dataset
transactions_with_units <- transactions %>%
  mutate(unit = unit_mapping(tutor))

# Group the transactions by unit and count the number of transactions
unit_summary <- transactions_with_units %>%
  group_by(unit) %>%
  summarise(transactions_count = n())

# Create a bar chart for the units
ggplot(unit_summary, aes(x = unit, y = transactions_count)) +
  geom_text(aes(label = transactions_count), vjust = -0.5) +  # Add labels for each bar
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  scale_y_continuous(trans = "log10") +  # Apply logarithmic scale
  labs(title = "Number of Transactions by Unit",
       x = "Unit",
       y = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########################################################################################
##########################################################################################
#Unit Grades vs. Unit Tutor Usage Relationship
##################
##########################################################################################
##########################################################################################
#Goal: Bucket the transactions / user based on unit and see how it effected their grade in that unit. 
#Compare this to non tutor user grades in those unites
# Process: Clean up transactions data, and bucket based on unit 
# Overlap based on units for comparison


# Filter the grades for the Unit tests using Display_Column_Name
grades_with_tutors <- grades %>%
  filter(Display_Column_Name %in% c("Unit 1 Test", "Unit 2 Test", "Unit 3 Test", "Unit 4 Test")) %>%
  left_join(transactions, by = "LTI_ID", relationship = "many-to-many") 

#Join with tutor transactions / unit
# Step 1: Create new columns for each unit based on the tutor types
transactions <- transactions %>%
  mutate(
    Unit_1_Usage = ifelse(tutor %in% c("exponents", "htn_exponents", "radicals", "htn_radicals", 
                                       "factoring_polynomials", "factoring", "htn_factoring_polynomials", 
                                       "rational_equation", "htn_rational_equation"), 1, 0),
    Unit_2_Usage = ifelse(tutor %in% c("quadratic_equations", "htn_quadratic_equations"), 1, 0),
    Unit_3_Usage = ifelse(tutor %in% c("quadratic_functions"), 1, 0),
    Unit_4_Usage = ifelse(tutor %in% c("htn_exponential_equations", "adaptive_scaffolded_exponential_equations", 
                                       "exponential_equations", "logarithms", "htn_logarithms", 
                                       "logarithmic_equations", "htn_logarithmic_equations"), 1, 0)
  )

# Step 2: Summarize the number of transactions per LTI ID for each unit
unit_usage_summary <- transactions %>%
  group_by(LTI_ID) %>%
  summarise(
    Unit_1_Transactions = sum(Unit_1_Usage, na.rm = TRUE),
    Unit_2_Transactions = sum(Unit_2_Usage, na.rm = TRUE),
    Unit_3_Transactions = sum(Unit_3_Usage, na.rm = TRUE),
    Unit_4_Transactions = sum(Unit_4_Usage, na.rm = TRUE)
  )

# Step 3: Replace any missing transaction counts with 0
unit_usage_summary <- unit_usage_summary %>%
  mutate(across(starts_with("Unit_"), ~replace_na(., 0)))

# View the summary of transactions per LTI ID
#View(unit_usage_summary)


final_dataset <- grades_with_tutors %>%
  left_join(unit_usage_summary, by = "LTI_ID")

# First, clean the 'Percentage' column to ensure it's numeric
final_dataset <- final_dataset %>%
  mutate(Percentage = as.numeric(gsub("%", "", Percentage)))

View(final_dataset)

write_csv(final_dataset, "Documents/GitHub/apprentice-data-analysis/unitgrades_transactions_dataset.csv")


######################################### 
#UNIT 1,2,3,4 TUTOR USERS
#########################################
# Define a function to calculate mean grades for tutor users and non-tutor users across all units
library(dplyr)
library(ggplot2)

calculate_overall_mean_grade <- function(final_dataset) {
  # Create a helper function to process each unit
  process_unit <- function(unit_number, transactions_col) {
    filtered_data <- final_dataset %>%
      select(-LTI_IDs) %>%
      filter(Display_Column_Name == paste0("Unit ", unit_number, " Test")) %>%
      distinct(LTI_ID, .keep_all = TRUE)
    
    # Filter students who used tutors
    students_used_tutors <- filtered_data %>%
      filter(!is.na(.data[[transactions_col]]) & .data[[transactions_col]] > 0)
    
    # Calculate the mean and standard error for tutor users
    mean_grade_used_tutors <- students_used_tutors %>%
      summarise(mean_grade = mean(Percentage, na.rm = TRUE),
                se = sd(Percentage, na.rm = TRUE) / sqrt(n()))
    
    # Filter students who did not use tutors
    students_not_used_tutors <- filtered_data %>%
      filter(is.na(.data[[transactions_col]]) | .data[[transactions_col]] == 0)
    
    # Calculate the mean and standard error for non-tutor users
    mean_grade_not_used_tutors <- students_not_used_tutors %>%
      summarise(mean_grade = mean(Percentage, na.rm = TRUE),
                se = sd(Percentage, na.rm = TRUE) / sqrt(n()))
    
    return(list(
      used_tutors = mean_grade_used_tutors$mean_grade, 
      not_used_tutors = mean_grade_not_used_tutors$mean_grade,
      se_used = mean_grade_used_tutors$se,
      se_not_used = mean_grade_not_used_tutors$se
    ))
  }
  
  # Process all units (1 through 4)
  unit_1 <- process_unit(1, "Unit_1_Transactions")
  unit_2 <- process_unit(2, "Unit_2_Transactions")
  unit_3 <- process_unit(3, "Unit_3_Transactions")
  unit_4 <- process_unit(4, "Unit_4_Transactions")
  
  # Combine the mean grades and standard errors across all units
  mean_grades_used_tutors <- c(unit_1$used_tutors, unit_2$used_tutors, unit_3$used_tutors, unit_4$used_tutors)
  mean_grades_not_used_tutors <- c(unit_1$not_used_tutors, unit_2$not_used_tutors, unit_3$not_used_tutors, unit_4$not_used_tutors)
  
  overall_mean_grade_used_tutors <- mean(mean_grades_used_tutors, na.rm = TRUE)
  overall_mean_grade_not_used_tutors <- mean(mean_grades_not_used_tutors, na.rm = TRUE)
  
  # Calculate overall standard errors for both groups
  overall_se_used_tutors <- sd(mean_grades_used_tutors, na.rm = TRUE) / sqrt(length(mean_grades_used_tutors))
  overall_se_not_used_tutors <- sd(mean_grades_not_used_tutors, na.rm = TRUE) / sqrt(length(mean_grades_not_used_tutors))
  
  # Create a data frame with the mean grades and their standard errors
  data <- data.frame(
    Group = c("Used Tutors", "Not Used Tutors"),
    Mean_Grade = c(overall_mean_grade_used_tutors, overall_mean_grade_not_used_tutors),
    SE = c(overall_se_used_tutors, overall_se_not_used_tutors)
  )
  
  # Create the bar plot with error bars
  ggplot(data, aes(x = Group, y = Mean_Grade, fill = Group)) +
   #stat_summary(fun.data = Mean_Grade, geom = "errorbar", width=0.025, alpha=0.7)
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = Mean_Grade - SE, ymax = Mean_Grade + SE), width = 0.2) +
    labs(title = "Overall Mean Grades: Tutor Users vs Non-Tutor Users",
         x = "Group", y = "Mean Grade (%)") +
    theme_minimal() +
    scale_fill_manual(values = c("lightblue", "lightgreen")) +
    geom_text(aes(label = round(Mean_Grade, 1)), vjust = -0.5, size = 5)
}


# Call the function with your dataset
overall_mean_grades <- calculate_overall_mean_grade(final_dataset)

print(overall_mean_grades)

######################################### 
#UNIT 1 TUTOR USERS
#########################################
Unit1_filtered_data <- final_dataset %>%
  select(-Unit_2_Transactions, -Unit_3_Transactions, -Unit_4_Transactions, -LTI_IDs) %>%
  filter(Display_Column_Name == "Unit 1 Test") %>%  
  distinct(LTI_ID, .keep_all = TRUE)

# View the filtered data
# View(Unit1_filtered_data)

write_csv(final_dataset, "Documents/GitHub/apprentice-data-analysis/Unit1_filtered_data.csv")


# Filter students who used tutors (Unit_1_Transactions is not NA)
unit1_students_used_tutors <- Unit1_filtered_data %>%
  filter(!is.na(Unit_1_Transactions) & Unit_1_Transactions > 0)

# Calculate the mean grade for these students
unit1_mean_grade_used_tutors <- unit1_students_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit1_mean_grade_used_tutors

##### UNIT 1 NON TUTOR USERS
# Filter students who did not use tutors (Unit_1_Transactions is NA or 0)
unit1_students_not_used_tutors <- final_dataset %>%
  filter(is.na(Unit_1_Transactions) | Unit_1_Transactions == 0)

# Calculate the mean grade for these students
unit1_mean_grade_not_used_tutors <- unit1_students_not_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit1_mean_grade_not_used_tutors

# Extract the mean_grade values as numeric
unit1_mean_grade_used_tutors_value <- unit1_mean_grade_used_tutors$mean_grade[1]
unit1_mean_grade_not_used_tutors_value <- unit1_mean_grade_not_used_tutors$mean_grade[1]

# Create a data frame with the mean values (since stat_summary will calculate the error bars)
data <- data.frame(
  Group = c("Used Tutors", "Not Used Tutors"),
  Mean_Grade = c(unit1_mean_grade_used_tutors$mean_grade, unit1_mean_grade_not_used_tutors$mean_grade)
)

# Save the plot to an object
my_plot <- ggplot(data, aes(x = Group, y = Mean_Grade, fill = Group)) +
  geom_bar(stat = "identity", width = 0.5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, alpha = 0.7) +
  labs(title = "Unit 1 Mean Grades: Tutor Users vs Non-Tutor Users",
       x = "Group", y = "Mean Grade (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  geom_text(aes(label = round(Mean_Grade, 1)), vjust = -0.5, size = 5) +
  ylim(0, 100)

# Print the plot
print(my_plot)

# Add labels on top of the bars showing the mean grades
text(x = c(1, 2), y = mean_grades, labels = round(mean_grades, 1), pos = 3)

# Boxplot to inspect variance in the groups
boxplot(unit1_students_used_tutors$Percentage, unit1_students_not_used_tutors$Percentage,
        names = c("Used Tutors", "Not Used Tutors"),
        main = "Unit 1 Mean Grades: Tutor Users vs Non-Tutor Users",
        ylab = "Mean Grade (%)")

# Add labels to the boxplot
text(x = 1, y = unit1_mean_grade_used_tutors, labels = round(unit1_mean_grade_used_tutors, 1), pos = 3, col = "blue")
text(x = 2, y = unit1_mean_grade_not_used_tutors, labels = round(unit1_mean_grade_not_used_tutors, 1), pos = 3, col = "blue")

# Scatter plot for Unit 1 tutor users
ggplot(unit1_students_used_tutors, aes(x = Unit_1_Transactions, y = Percentage)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Line of best fit with standard error band
  labs(title = "Unit 1: Scatter Plot of Grades vs Tutor Transactions for Tutor Users",
       x = "Number of Tutor Transactions",
       y = "Grade Percentage") +
  theme_minimal()

# Fit the linear model
regression_model <- lm(Percentage ~ Unit_1_Transactions, data = unit1_students_used_tutors)

# Summarize the model to get p-values and other statistics
summary(regression_model)

######################################### 
#UNIT 1 TUTOR USERS
#########################################



######################################### 
#UNIT 2 TUTOR USERS
#########################################
Unit2_filtered_data <- final_dataset %>%
  select(-Unit_1_Transactions, -Unit_3_Transactions, -Unit_4_Transactions, -LTI_IDs) %>%
  filter(Display_Column_Name == "Unit 2 Test") %>%  
  distinct(LTI_ID, .keep_all = TRUE)

# View the filtered data
# View(Unit2_filtered_data)

write_csv(final_dataset, "Documents/GitHub/apprentice-data-analysis/Unit2_filtered_data.csv")


# Filter students who used tutors (Unit_1_Transactions is not NA)
unit2_students_used_tutors <- Unit2_filtered_data %>%
  filter(!is.na(Unit_2_Transactions) & Unit_2_Transactions > 0)

# Calculate the mean grade for these students
unit2_mean_grade_used_tutors <- unit2_students_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit2_mean_grade_used_tutors

##### UNIT 2 NON TUTOR USERS
# Filter students who did not use tutors (Unit_1_Transactions is NA or 0)
unit2_students_not_used_tutors <- final_dataset %>%
  filter(is.na(Unit_2_Transactions) | Unit_2_Transactions == 0)

# Calculate the mean grade for these students
unit2_mean_grade_not_used_tutors <- unit2_students_not_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit2_mean_grade_not_used_tutors

# Create a data frame with the values
data <- data.frame(
  Group = c("Used Tutors", "Not Used Tutors"),
  Mean_Grade = c(unit2_mean_grade_used_tutors, unit2_mean_grade_not_used_tutors)
)

# Extract the mean grades for the two groups
mean_grades <- c(data$Mean_Grade.mean_grade[1], data$Mean_Grade.mean_grade.1[1])

# Group names for the plot
groups <- c("Used Tutors", "Not Used Tutors")

# Create the bar plot
barplot(
  height = mean_grades, 
  names.arg = groups,
  col = c("lightblue", "lightgreen"),
  main = "Unit 2 Mean Grades: Tutor Users vs Non-Tutor Users",
  ylab = "Mean Grade (%)",
  ylim = c(0, 100)  # Adjust as necessary to fit the mean grade range
)

# Add labels on top of the bars showing the mean grades
text(x = c(1, 2), y = mean_grades, labels = round(mean_grades, 1), pos = 3)

# Boxplot to inspect variance in the groups
boxplot(unit2_students_used_tutors$Percentage, unit2_students_not_used_tutors$Percentage,
        names = c("Used Tutors", "Not Used Tutors"),
        main = "Unit 2 Mean Grades: Tutor Users vs Non-Tutor Users",
        ylab = "Mean Grade (%)")

# Add labels to the boxplot
text(x = 1, y = unit2_mean_grade_used_tutors, labels = round(unit2_mean_grade_used_tutors, 1), pos = 3, col = "blue")
text(x = 2, y = unit2_mean_grade_not_used_tutors, labels = round(unit2_mean_grade_not_used_tutors, 1), pos = 3, col = "blue")

# Scatter plot for Unit 3 tutor users
ggplot(unit2_students_used_tutors, aes(x = Unit_2_Transactions, y = Percentage)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Line of best fit with standard error band
  labs(title = "Unit 2: Scatter Plot of Grades vs Tutor Transactions for Tutor Users",
       x = "Number of Tutor Transactions",
       y = "Grade Percentage") +
  theme_minimal()

# Fit the linear model
regression_model <- lm(Percentage ~ Unit_2_Transactions, data = unit2_students_used_tutors)

# Summarize the model to get p-values and other statistics
summary(regression_model)
######################################### 
#UNIT 2 TUTOR USERS
#########################################



######################################### 
#UNIT 3 TUTOR USERS
#########################################
Unit3_filtered_data <- final_dataset %>%
  select(-Unit_1_Transactions, -Unit_2_Transactions, -Unit_4_Transactions, -LTI_IDs) %>%
  filter(Display_Column_Name == "Unit 3 Test") %>%  
  distinct(LTI_ID, .keep_all = TRUE)

# View the filtered data
# View(Unit3_filtered_data)

write_csv(final_dataset, "Documents/GitHub/apprentice-data-analysis/Unit3_filtered_data.csv")


# Filter students who used tutors (Unit_1_Transactions is not NA)
unit3_students_used_tutors <- Unit3_filtered_data %>%
  filter(!is.na(Unit_3_Transactions) & Unit_3_Transactions > 0)

# Calculate the mean grade for these students
unit3_mean_grade_used_tutors <- unit3_students_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit3_mean_grade_used_tutors

##### UNIT 2 NON TUTOR USERS
# Filter students who did not use tutors (Unit_1_Transactions is NA or 0)
unit3_students_not_used_tutors <- final_dataset %>%
  filter(is.na(Unit_3_Transactions) | Unit_3_Transactions == 0)

# Calculate the mean grade for these students
unit3_mean_grade_not_used_tutors <- unit3_students_not_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit3_mean_grade_not_used_tutors

# Create a data frame with the values
data <- data.frame(
  Group = c("Used Tutors", "Not Used Tutors"),
  Mean_Grade = c(unit3_mean_grade_used_tutors, unit3_mean_grade_not_used_tutors)
)

# Extract the mean grades for the two groups
mean_grades <- c(data$Mean_Grade.mean_grade[1], data$Mean_Grade.mean_grade.1[1])

# Group names for the plot
groups <- c("Used Tutors", "Not Used Tutors")

# Create the bar plot
barplot(
  height = mean_grades, 
  names.arg = groups,
  col = c("lightblue", "lightgreen"),
  main = "Unit 3 Mean Grades: Tutor Users vs Non-Tutor Users",
  ylab = "Mean Grade (%)",
  ylim = c(0, 100)  # Adjust as necessary to fit the mean grade range
)

# Add labels on top of the bars showing the mean grades
text(x = c(1, 2), y = mean_grades, labels = round(mean_grades, 1), pos = 3)

# Boxplot to inspect variance in the groups
boxplot(unit3_students_used_tutors$Percentage, unit3_students_not_used_tutors$Percentage,
        names = c("Used Tutors", "Not Used Tutors"),
        main = "Unit 3 Mean Grades: Tutor Users vs Non-Tutor Users",
        ylab = "Mean Grade (%)")

# Add labels to the boxplot
text(x = 1, y = unit3_mean_grade_used_tutors, labels = round(unit3_mean_grade_used_tutors, 1), pos = 3, col = "blue")
text(x = 2, y = unit3_mean_grade_not_used_tutors, labels = round(unit3_mean_grade_not_used_tutors, 1), pos = 3, col = "blue")

# Scatter plot for Unit 3 tutor users
ggplot(unit3_students_used_tutors, aes(x = Unit_3_Transactions, y = Percentage)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Line of best fit with standard error band
  labs(title = "Unit 3: Scatter Plot of Grades vs Tutor Transactions for Tutor Users",
       x = "Number of Tutor Transactions",
       y = "Grade Percentage") +
  theme_minimal()

# Fit the linear model
regression_model <- lm(Percentage ~ Unit_3_Transactions, data = unit3_students_used_tutors)

# Summarize the model to get p-values and other statistics
summary(regression_model)
######################################### 
#UNIT 3 TUTOR USERS
#########################################



######################################### 
#UNIT 4 TUTOR USERS
#########################################
Unit4_filtered_data <- final_dataset %>%
  select(-Unit_1_Transactions, -Unit_2_Transactions, -Unit_3_Transactions, -LTI_IDs) %>%
  filter(Display_Column_Name == "Unit 4 Test") %>%  
  distinct(LTI_ID, .keep_all = TRUE)

# View the filtered data
# View(Unit4_filtered_data)

write_csv(final_dataset, "Documents/GitHub/apprentice-data-analysis/Unit4_filtered_data.csv")


# Filter students who used tutors (Unit_1_Transactions is not NA)
unit4_students_used_tutors <- Unit4_filtered_data %>%
  filter(!is.na(Unit_4_Transactions) & Unit_4_Transactions > 0)

# Calculate the mean grade for these students
unit4_mean_grade_used_tutors <- unit4_students_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit4_mean_grade_used_tutors

##### UNIT 2 NON TUTOR USERS
# Filter students who did not use tutors (Unit_1_Transactions is NA or 0)
unit4_students_not_used_tutors <- final_dataset %>%
  filter(is.na(Unit_4_Transactions) | Unit_4_Transactions == 0)

# Calculate the mean grade for these students
unit4_mean_grade_not_used_tutors <- unit4_students_not_used_tutors %>%
  summarise(mean_grade = mean(Percentage, na.rm = TRUE))

# View the result
unit4_mean_grade_not_used_tutors

# Create a data frame with the values
data <- data.frame(
  Group = c("Used Tutors", "Not Used Tutors"),
  Mean_Grade = c(unit4_mean_grade_used_tutors, unit4_mean_grade_not_used_tutors)
)

# Extract the mean grades for the two groups
mean_grades <- c(data$Mean_Grade.mean_grade[1], data$Mean_Grade.mean_grade.1[1])

# Group names for the plot
groups <- c("Used Tutors", "Not Used Tutors")

# Create the bar plot
barplot(
  height = mean_grades, 
  names.arg = groups,
  col = c("lightblue", "lightgreen"),
  main = "Unit 4 Mean Grades: Tutor Users vs Non-Tutor Users",
  ylab = "Mean Grade (%)",
  ylim = c(0, 100)  # Adjust as necessary to fit the mean grade range
)

# Add labels on top of the bars showing the mean grades
text(x = c(1, 2), y = mean_grades, labels = round(mean_grades, 1), pos = 3)

# Boxplot to inspect variance in the groups
boxplot(unit4_students_used_tutors$Percentage, unit4_students_not_used_tutors$Percentage,
        names = c("Used Tutors", "Not Used Tutors"),
        main = "Unit 4 Mean Grades: Tutor Users vs Non-Tutor Users",
        ylab = "Mean Grade (%)")

# Add labels to the boxplot
text(x = 1, y = unit4_mean_grade_used_tutors, labels = round(unit4_mean_grade_used_tutors, 1), pos = 3, col = "blue")
text(x = 2, y = unit4_mean_grade_not_used_tutors, labels = round(unit4_mean_grade_not_used_tutors, 1), pos = 3, col = "blue")

# Scatter plot for Unit 4 tutor users
ggplot(unit4_students_used_tutors, aes(x = Unit_4_Transactions, y = Percentage)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Line of best fit with standard error band
  labs(title = "Unit 4: Scatter Plot of Grades vs Tutor Transactions for Tutor Users",
       x = "Number of Tutor Transactions",
       y = "Grade Percentage") +
  theme_minimal()

# Fit the linear model
regression_model <- lm(Percentage ~ Unit_4_Transactions, data = unit4_students_used_tutors)

# Summarize the model to get p-values and other statistics
summary(regression_model)
######################################### 
#UNIT 4 TUTOR USERS
#########################################

######################################### 
#UNIT 1 Linear Regression
#########################################








######################################### 
#Tutor Usage Time
#########################################

# View(transactions)

# Extract the day of the week and the hour
transactions <- transactions %>%
  mutate(day_of_week = wday(time, label = TRUE, abbr = TRUE),  # Day of the week
         hour_of_day = hour(time))  # Hour of the day

# Define time ranges (e.g., Morning, Afternoon, Evening, Night)
transactions <- transactions %>%
  mutate(time_range = case_when(
    hour_of_day >= 6 & hour_of_day < 12 ~ "Morning (6a-12p)",
    hour_of_day >= 12 & hour_of_day < 17 ~ "Afternoon (12p-5p)",
    hour_of_day >= 17 & hour_of_day < 24 ~ "Evening (5p-12a)",
    TRUE ~ "Late Night (12a-6a)"
  ))

# Plot tutor usage by day of the week with labels and log scale
ggplot(transactions, aes(x = day_of_week)) +
  geom_bar(fill = "lightblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  # Add labels
  labs(title = "Tutor Usage by Day of the Week", x = "Day", y = "Number of Tutor Transactions (log scale)") +
  theme_minimal() 
  #scale_y_log10()  # Apply log scale to y-axis

# Plot tutor usage by time range with labels and log scale
ggplot(transactions, aes(x = time_range)) +
  geom_bar(fill = "lightgreen") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  # Add labels
  labs(title = "Tutor Usage by Time Range", x = "Time Range", y = "Number of Tutor Transactions (log scale)") +
  theme_minimal() 
  #scale_y_log10()  # Apply log scale to y-axis


# Combine day of the week and time range with labels and log scale, and rotate labels
ggplot(transactions, aes(x = day_of_week, fill = time_range)) +
  geom_bar(position = position_dodge(width = 0.9)) +  # Adjust bar dodge width
  geom_text(stat = 'count', aes(label = ..count..), vjust = 0.5, position = position_dodge(width = 0.9), 
            angle = 45, hjust = -0.2, size = 3) +  # Adjust label dodge width, angle, and size
  labs(title = "Tutor Usage by Day and Time Range", x = "Day", y = "Number of Tutor Transactions (log scale)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 
  #scale_y_log10()  # Apply log scale to y-axis


######################################### 
#Unique Sessions Per LTI ID and Retention
#########################################

# Sort by user and time, calculate time differences, and identify new sessions
transactions_with_sessions <- transactions %>%
  arrange(LTI_ID, time) %>%
  group_by(LTI_ID) %>%
  mutate(time_diff = difftime(time, lag(time), units = "mins"),
         new_session = ifelse(is.na(time_diff) | time_diff >= 10, 1, 0)) %>%
  mutate(session_id = cumsum(new_session))  # Cumulative sum of new sessions

# Summarize the number of sessions per user
unique_sessions <- transactions_with_sessions %>%
  summarise(unique_session_count = n_distinct(session_id))

# View the results
# View(unique_sessions)


################################################################################## 
#How Unit 1 Test Takers who did NOT use tutors in the future 
# Finding: Users who used the tutors in unit 1 and not in other units performed the best in unit 1
################################################################################## 

#Find the dataset of students who had transactions in Unit 1 but not in Unit 2/3/4. 
#This representation should show how the grades were with trasactions in unit 1 

# Filter the dataset based on the given conditions
unit1_filtered_data <- final_dataset[final_dataset$Unit_1_Transactions > 0 & 
                                       final_dataset$Unit_2_Transactions < 1 & 
                                       final_dataset$Unit_3_Transactions < 1 & 
                                       final_dataset$Unit_4_Transactions < 1 &
                                       !is.na(final_dataset$Unit_1_Transactions) ,]

# View the filtered data
View(unit1_filtered_data)

# Calculate the mean percentage for each unit test
mean_percentage_by_unit <- unit1_filtered_data %>%
  group_by(Display_Column_Name) %>%
  summarise(mean_percentage = mean(Percentage, na.rm = TRUE))

# Create a ggplot bar chart
ggplot(mean_percentage_by_unit, aes(x = Display_Column_Name, y = mean_percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(mean_percentage, 1)), vjust = -0.5) +
  labs(title = "Mean Percentage by Unit Test",
       x = "Unit Test", y = "Mean Percentage") +
  theme_minimal()


##########################################################################################################  
#Which instructors had the most tutor usage via transactions, and did that correlate to higher grades?
########################################################################################################## 




##########################################################################################################  
#Heatmaps for Tutor Usage vs. Performance: How much tutor usage is optimal for grade improvement? 
########################################################################################################## 



##########################################################################################################  
#Correlation between unique tutor sessions and final grade outcome 
#Heatmaps for Tutor Usage vs. Performance: How much tutor usage is optimal for grade improvement? 
########################################################################################################## 



##################
#how many unique LTI ID's did we log in database vs in demo/grades data?
#whats the distribution of gender?
#what's the distribution of race?
#what's the distribution of age?
#How many classes are there? (based on class ID)
#How many students are there in segments of usage (low, medium, high) --> usage segments
#What is the distribution of grade by usage segments
#What is the distribution of grade by age
#What is the distribution of grade by Race
#What is the distribution of grade by Gender
#How did students who used the tutor do in the overall class compared to non-users?
#Run T Test, Linear Regression
#https://docs.google.com/presentation/d/1vzKI-MyiHMByvPYM2kXsSjqkxuNElqytpYIkQ0B2aUI/edit#slide=id.p
##################

