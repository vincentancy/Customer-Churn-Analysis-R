# Customer Churn Analysis and Retention Strategy Using R

## 📌 Project Overview

Customer churn is an important business problem for subscription-based
companies because losing existing customers can reduce recurring revenue
and increase customer acquisition costs.

This project performs an end-to-end analysis of customer churn using R.
The objective is to identify the major factors associated with customer
churn, build predictive models, and develop data-driven customer
retention strategies.

The project includes data cleaning, exploratory data analysis (EDA),
visualization, statistical analysis, hypothesis testing, predictive
modeling, model evaluation, and classification-threshold optimization.

---

## 📊 Dataset

The project uses the Telco Customer Churn dataset.

- Total customers: 7,043
- Total variables: 21
- Target variable: `Churn`
- Target classes: `Yes` and `No`

The dataset contains information about:

- Customer demographics
- Internet and telephone services
- Contract type
- Customer tenure
- Monthly charges
- Total charges
- Payment methods
- Customer churn status

---

## 🛠️ Technologies and Tools

- R
- RStudio
- tidyverse
- ggplot2
- caret
- pROC
- corrplot
- car
- Random Forest
- Gradient Boosting

---

## 🔄 Project Workflow

### 1. Data Loading and Understanding

The dataset was imported into R and examined to understand its
dimensions, variables, data types, and customer characteristics.

### 2. Data Cleaning and Preprocessing

The preprocessing stage included:

- Missing-value detection and treatment
- Duplicate customer checks
- Data-type validation
- Outlier assessment
- Categorical-variable preparation
- Preparation of cleaned data for further analysis

### 3. Exploratory Data Analysis

EDA was performed to understand customer behavior and identify patterns
associated with churn.

Important factors investigated included:

- Contract type
- Customer tenure
- Monthly charges
- Total charges
- Internet service
- Payment method
- Senior-citizen status

### 4. Data Visualization

Multiple visualizations were created using R to communicate churn
patterns, including:

- Bar charts
- Histograms
- Boxplots
- Scatter plots
- Line charts
- Correlation heatmaps

### 5. Statistical Analysis

Statistical analysis was performed to determine whether observed
customer characteristics were significantly associated with churn.

The analysis included:

- Descriptive statistics
- Distribution analysis
- Hypothesis testing
- Correlation analysis
- Multicollinearity assessment

### 6. Predictive Modeling

Customer churn was treated as a binary classification problem.

The following models were evaluated:

- Logistic Regression
- Decision Tree
- Random Forest
- Gradient Boosting

An 80/20 stratified train-test split and cross-validation were used for
model development and evaluation.

### 7. Model Evaluation

Models were evaluated using multiple performance metrics:

- Accuracy
- Precision
- Recall
- Specificity
- F1-score
- Kappa
- ROC-AUC

Classification-threshold optimization was also performed to improve the
identification of potential churners.

---

## 📈 Key Findings

- 5,174 customers remained with the company.
- 1,869 customers churned.
- The overall churn rate was approximately **26.54%**.
- Month-to-month customers showed substantially higher churn than
  customers with longer-term contracts.
- Customers who churned generally had shorter tenure.
- Churned customers had higher average monthly charges.
- Contract, tenure, service characteristics, and billing behavior were
  important factors in understanding customer churn.

---

## 💼 Business Recommendations

Based on the analysis, organizations can consider:

- Prioritizing high-risk customers for retention campaigns.
- Improving engagement during the early stages of the customer
  relationship.
- Providing suitable incentives for eligible month-to-month customers
  to move toward longer-term contracts.
- Reviewing pricing and service value for customers with high monthly
  charges.
- Using churn probability and predictive models to support proactive
  customer-retention decisions.

---

## 📁 Repository Structure

```text
Customer-Churn-Analysis-R/
│
├── data/
│   ├── Telco-Customer-Churn.csv
│   └── Telco-Customer-Churn-Cleaned.csv
│
├── scripts/
│   ├── 01_data_loading.R
│   ├── 02_data_cleaning.R
│   ├── 03_data_visualization.R
│   ├── 04_statistical_analysis_modeling.R
│   └── 05_threshold_optimization.R
│
├── reports/
│   ├── all_model_threshold_comparison.csv
│   ├── best_threshold_by_model.csv
│   └── final_model_comparison.csv
│
└── README.md
