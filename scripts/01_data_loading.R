# Customer Churn Analysis and Retention Strategy
# Step 1: Data Loading

library(tidyverse)
library(janitor)
library(corrplot)
library(plotly)
library(readxl)


# Load the dataset
churn_data <- read_csv("data/Telco-Customer-Churn.csv")

# Check dimensions
dim(churn_data)

# View first 6 rows
head(churn_data)

# Check column names
names(churn_data)


# Check structure
str(churn_data)


# Check missing values
colSums(is.na(churn_data))

# Check duplicate customer IDs
sum(duplicated(churn_data$customerID))

# Check churn distribution
table(churn_data$Churn)

# Churn percentage
prop.table(table(churn_data$Churn)) * 100

# Check unique values of important categorical variables

unique(churn_data$gender)

unique(churn_data$Partner)

unique(churn_data$Contract)

unique(churn_data$InternetService)

unique(churn_data$PaymentMethod)

unique(churn_data$Churn)


#checks whether a value is missing
colSums(is.na(churn_data))

#checks duplicate customers
sum(duplicated(churn_data$customerID))

#counts how many customers belong to each category.
table(churn_data$Churn)

# Investigate records with missing TotalCharges

missing_rows <- churn_data[missing_total, ]
missing_rows_base <- as.data.frame(missing_rows)
print(missing_rows_base[, c(
  "customerID",
  "tenure",
  "MonthlyCharges",
  "TotalCharges",
  "Contract",
  "Churn"
  
)])

