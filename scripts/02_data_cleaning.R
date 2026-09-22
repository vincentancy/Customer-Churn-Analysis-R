# Customer Churn Analysis and Retention Strategy
# Step 2: Data Cleaning

library(tidyverse)
setwd("C:/Users/ACER1/Documents/Customer_Churn_Analysis")

getwd()
list.files("data")

# Load the original dataset
churn_data <- read_csv("data/Telco-Customer-Churn.csv")

# Check missing TotalCharges
sum(is.na(churn_data$TotalCharges))

# Replace missing TotalCharges with 0 for customers with tenure = 0
churn_data$TotalCharges[
  is.na(churn_data$TotalCharges) & churn_data$tenure == 0
] <- 0
# saving the cleaned dataset
write_csv(churn_data, "data/Telco-Customer-Churn-Cleaned.csv")

# Check missing values again
colSums(is.na(churn_data))

# Check for duplicate customer IDs
sum(duplicated(churn_data$customerID))

# Check data types and structure
str(churn_data)

# Summary statistics
summary(churn_data)

# Check the number of customers in each churn category
table(churn_data$Churn)

# Calculate overall churn rate
churn_rate <- mean(churn_data$Churn == "Yes") * 100

# Display churn rate
churn_rate

# -----------------------------------
# Outlier Detection using IQR Method
# -----------------------------------

# Function to calculate number of outliers
count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  sum(x < lower_bound | x > upper_bound)
}

# Count outliers
count_outliers(churn_data$tenure)
count_outliers(churn_data$MonthlyCharges)
count_outliers(churn_data$TotalCharges)


# -----------------------------------
# Correct Normalization
# -----------------------------------

# Create a fresh copy of the cleaned dataset
churn_normalized <- churn_data

# Normalize tenure
churn_normalized$tenure <-
  (churn_data$tenure - min(churn_data$tenure)) /
  (max(churn_data$tenure) - min(churn_data$tenure))

# Normalize MonthlyCharges
churn_normalized$MonthlyCharges <-
  (churn_data$MonthlyCharges - min(churn_data$MonthlyCharges)) /
  (max(churn_data$MonthlyCharges) - min(churn_data$MonthlyCharges))

# Normalize TotalCharges
churn_normalized$TotalCharges <-
  (churn_data$TotalCharges - min(churn_data$TotalCharges)) /
  (max(churn_data$TotalCharges) - min(churn_data$TotalCharges))

# Check the normalized variables
summary(churn_normalized[, c(
  "tenure",
  "MonthlyCharges",
  "TotalCharges"
)])

summary(churn_normalized[, c(
  "tenure",
  "MonthlyCharges",
  "TotalCharges"
)])


# -----------------------------------
# Categorical Variable Encoding
# -----------------------------------

# Create a copy of the cleaned dataset
churn_encoded <- churn_data

# Convert categorical variables to factors
categorical_columns <- c(
  "gender",
  "Partner",
  "Dependents",
  "PhoneService",
  "MultipleLines",
  "InternetService",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  "Churn"
)

churn_encoded[categorical_columns] <- 
  lapply(churn_encoded[categorical_columns], factor)

# Check the structure after encoding
str(churn_encoded)

# -----------------------------------
# Exploratory Data Analysis
# Overall Churn Distribution
# -----------------------------------

ggplot(churn_data, aes(x = Churn)) +
  geom_bar() +
  labs(
    title = "Overall Customer Churn Distribution",
    x = "Churn Status",
    y = "Number of Customers"
  ) +
  theme_minimal()

# -----------------------------------
# Churn by Contract Type
# -----------------------------------

ggplot(churn_data, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Customer Churn by Contract Type",
    x = "Contract Type",
    y = "Number of Customers",
    fill = "Churn Status"
  ) +
  theme_minimal()

# -----------------------------------
# Churn by Tenure Group
# -----------------------------------
# Create tenure groups

churn_data$TenureGroup <- cut(
  churn_data$tenure,
  breaks = c(-1, 12, 24, 48, 72),
  labels = c(
    "0-12 Months",
    "13-24 Months",
    "25-48 Months",
    "49-72 Months"
  )
)
table(churn_data$TenureGroup)
ggplot(churn_data, aes(x = TenureGroup, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Customer Churn by Tenure Group",
    x = "Tenure Group",
    y = "Number of Customers",
    fill = "Churn Status"
  ) +
  theme_minimal()

# -----------------------------------
# Monthly Charges by Churn Status
# -----------------------------------

ggplot(churn_data, aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
  geom_boxplot() +
  labs(
    title = "Monthly Charges by Churn Status",
    x = "Churn Status",
    y = "Monthly Charges"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# -----------------------------------
# Churn by Internet Service
# -----------------------------------

ggplot(churn_data, aes(x = InternetService, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Customer Churn by Internet Service",
    x = "Internet Service",
    y = "Number of Customers",
    fill = "Churn Status"
  ) +
  theme_minimal()

# -----------------------------------
# Churn Rate by Internet Service
# -----------------------------------

internet_churn_rate <- prop.table(
  table(churn_data$InternetService, churn_data$Churn),
  margin = 1
) * 100

internet_churn_rate


# -----------------------------------
# Churn by Payment Method
# -----------------------------------

ggplot(churn_data, aes(x = PaymentMethod, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Customer Churn by Payment Method",
    x = "Payment Method",
    y = "Number of Customers",
    fill = "Churn Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )


# -----------------------------------
# Churn Rate by Payment Method
# -----------------------------------

payment_churn_rate <- prop.table(
  table(churn_data$PaymentMethod, churn_data$Churn),
  margin = 1
) * 100

payment_churn_rate


# -----------------------------------
# Correlation Analysis
# -----------------------------------

numeric_data <- churn_data[
  c("SeniorCitizen", "tenure", "MonthlyCharges", "TotalCharges")
]

correlation_matrix <- cor(numeric_data)

correlation_matrix

# -----------------------------------
# Correlation Plot
# -----------------------------------

library(corrplot)

# Create a larger correlation plot

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 1.2,
  tl.cex = 1.1,
  title = "Correlation Matrix of Numerical Variables",
  mar = c(0, 0, 2, 0)
)


# -----------------------------------
# Churn Rate by Gender
# -----------------------------------

gender_churn_rate <- prop.table(
  table(churn_data$gender, churn_data$Churn),
  margin = 1
) * 100

gender_churn_rate

# -----------------------------------
# Churn Rate by Gender
# -----------------------------------

gender_churn_rate <- prop.table(
  table(churn_data$gender, churn_data$Churn),
  margin = 1
) * 100

gender_plot_data <- data.frame(
  Gender = c("Female", "Male"),
  ChurnRate = c(
    gender_churn_rate["Female", "Yes"],
    gender_churn_rate["Male", "Yes"]
  )
)

ggplot(gender_plot_data, aes(x = Gender, y = ChurnRate, fill = Gender)) +
  geom_col() +
  labs(
    title = "Customer Churn Rate by Gender",
    x = "Gender",
    y = "Churn Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# -----------------------------------
# Churn Rate by Senior Citizen Status
# -----------------------------------

senior_churn_rate <- prop.table(
  table(churn_data$SeniorCitizen, churn_data$Churn),
  margin = 1
) * 100

senior_churn_rate

# -----------------------------------
# Senior Citizen Churn Rate Graph
# -----------------------------------

senior_plot_data <- data.frame(
  SeniorCitizen = c("Not Senior Citizen", "Senior Citizen"),
  ChurnRate = c(
    senior_churn_rate["0", "Yes"],
    senior_churn_rate["1", "Yes"]
  )
)

ggplot(
  senior_plot_data,
  aes(x = SeniorCitizen, y = ChurnRate, fill = SeniorCitizen)
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(ChurnRate, 2), "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Churn Rate by Senior Citizen Status",
    x = "Customer Group",
    y = "Churn Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# -----------------------------------
# Descriptive Statistics
# -----------------------------------

mean(churn_data$tenure)
median(churn_data$tenure)
sd(churn_data$tenure)

mean(churn_data$MonthlyCharges)
median(churn_data$MonthlyCharges)
sd(churn_data$MonthlyCharges)

mean(churn_data$TotalCharges)
median(churn_data$TotalCharges)
sd(churn_data$TotalCharges)

# -----------------------------------
# Churn Rate by Contract Type
# -----------------------------------

contract_churn_rate <- prop.table(
  table(churn_data$Contract, churn_data$Churn),
  margin = 1
) * 100

contract_churn_rate

churn_data %>%
  group_by(Contract) %>%
  summarise(
    Churn_Rate = mean(Churn == "Yes") * 100
  )
table(churn_data$Churn)

prop.table(table(churn_data$Churn)) * 100

churn_data %>%
  group_by(PaymentMethod) %>%
  summarise(
    Churn_Rate = mean(Churn == "Yes") * 100
  )

churn_data %>%
  group_by(InternetService) %>%
  summarise(
    Churn_Rate = mean(Churn == "Yes") * 100
  )

churn_data %>%
  group_by(SeniorCitizen) %>%
  summarise(
    Churn_Rate = mean(Churn == "Yes") * 100
  )
churn_data %>%
  group_by(gender) %>%
  summarise(
    Churn_Rate = mean(Churn == "Yes") * 100
  )
cor(
  churn_data$tenure,
  churn_data$TotalCharges,
  use = "complete.obs"
)
corrplot(cor(
  churn_data[, c("tenure", "MonthlyCharges", "TotalCharges")],
  use = "complete.obs"
))

# saving the cleaned dataset
write_csv(churn_data, "data/Telco-Customer-Churn-Cleaned2.csv")


library(ggplot2)

ggplot(churn_data, aes(x = Churn)) +
  geom_bar() +
  labs(
    title = "Overall Customer Churn Distribution",
    x = "Churn Status",
    y = "Number of Customers"
  ) +
  theme_minimal()
