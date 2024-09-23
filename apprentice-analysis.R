# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)


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
# View(merged_data_with_transactions)

# Remove duplicate rows in grades
merged_data_with_transactions <- merged_data_with_transactions %>% distinct()

# Save the merged dataset to a CSV file
write_csv(merged_data, "Documents/GitHub/apprentice-data-analysis/merged_data_with_transactions.csv")


# filter outliers
#merged_data_with_transactions <- merged_data_with_transactions[merged_data_with_transactions$Transaction_Count<1500,]



####################################
#Section 2: Graphs
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
  filter(Transaction_Count > 1 & Transaction_Count < 100)

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

