list.files("data")

library(tidyverse)
library(ggplot2)
library(corrplot)

churn_data <- read_csv("data/Telco-Customer-Churn-Cleaned.csv")
dim(churn_data)

colSums(is.na(churn_data))
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

# ==========================================
# VISUALIZATION 1: OVERALL CHURN DISTRIBUTION
# ==========================================

churn_plot <- ggplot(churn_data, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  labs(
    title = "Overall Customer Churn Distribution",
    x = "Customer Churn",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

churn_plot

# Calculate churn percentages
churn_summary <- churn_data %>%
  count(Churn) %>%
  mutate(
    Percentage = n / sum(n) * 100
  )

churn_summary
ggsave(
  "plots/01_overall_churn_distribution.png",
  plot = churn_plot,
  width = 8,
  height = 5,
  dpi = 300
)
churn_plot
churn_summary

# ==========================================
# VISUALIZATION 2: CHURN RATE BY CONTRACT
# ==========================================

contract_churn <- churn_data %>%
  group_by(Contract) %>%
  summarise(
    Total_Customers = n(),
    Churned_Customers = sum(Churn == "Yes"),
    Churn_Rate = mean(Churn == "Yes") * 100
  )

contract_churn
print(contract_churn, width = Inf)



contract_plot <- ggplot(
  contract_churn,
  aes(x = Contract, y = Churn_Rate, fill = Contract)
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(Churn_Rate, 1), "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Churn Rate by Contract Type",
    x = "Contract Type",
    y = "Churn Rate (%)"
  ) +
  ylim(0, 50) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

contract_plot

ggsave(
  "plots/02_churn_rate_by_contract.png",
  plot = contract_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# ==========================================
# VISUALIZATION 3: MONTHLY CHARGES DISTRIBUTION
# ==========================================

monthly_charges_plot <- ggplot(
  churn_data,
  aes(x = MonthlyCharges)
) +
  geom_histogram(
    binwidth = 10,
    fill =  "orange",
    color = "white"
  ) +
  labs(
    title = "Distribution of Monthly Charges",
    x = "Monthly Charges",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

monthly_charges_plot

ggsave(
  "plots/03_monthly_charges_distribution.png",
  plot = monthly_charges_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ==========================================
# VISUALIZATION 4: MONTHLY CHARGES BY CHURN (Create the boxplot)
# ==========================================

monthly_churn_plot <- ggplot(
  churn_data,
  aes(x = Churn, y = MonthlyCharges, fill = Churn)
) +
  geom_boxplot() +
  labs(
    title = "Monthly Charges by Customer Churn Status",
    x = "Customer Churn",
    y = "Monthly Charges"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

monthly_churn_plot
churn_data %>%
  group_by(Churn) %>%
  summarise(
    Count = n(),
    Mean_MonthlyCharges = mean(MonthlyCharges),
    Median_MonthlyCharges = median(MonthlyCharges),
    Min_MonthlyCharges = min(MonthlyCharges),
    Max_MonthlyCharges = max(MonthlyCharges)
  )

churn_data %>%
  group_by(Churn) %>%
  summarise(
    Mean_MonthlyCharges = mean(MonthlyCharges)
  )
churn_data %>%
  group_by(Churn) %>%
  summarise(
    Count = n(),
    Mean_MonthlyCharges = mean(MonthlyCharges),
    Median_MonthlyCharges = median(MonthlyCharges),
    Min_MonthlyCharges = min(MonthlyCharges),
    Max_MonthlyCharges = max(MonthlyCharges)
  ) %>%
  print(width = Inf)
ggsave(
  "plots/04_monthly_charges_by_churn.png",
  plot = monthly_churn_plot,
  width = 8,
  height = 5,
  dpi = 300
)

churn_data %>%
  group_by(Churn) %>%
  summarise(
    Count = n(),
    Mean_MonthlyCharges = mean(MonthlyCharges),
    Median_MonthlyCharges = median(MonthlyCharges),
    Min_MonthlyCharges = min(MonthlyCharges),
    Max_MonthlyCharges = max(MonthlyCharges)
  )


# ==========================================
# VISUALIZATION 5: TENURE VS TOTAL CHARGES
# ==========================================

tenure_total_plot <- ggplot(
  churn_data,
  aes(x = tenure, y = TotalCharges)
) +
  geom_point(
    alpha = 0.4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Tenure and Total Charges",
    x = "Tenure (Months)",
    y = "Total Charges"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

tenure_total_plot
cor(
  churn_data$tenure,
  churn_data$TotalCharges
)

ggsave(
  "plots/05_tenure_vs_total_charges.png",
  plot = tenure_total_plot,
  width = 8,
  height = 5,
  dpi = 300
)
tenure_total_plot
cor(
  churn_data$tenure,
  churn_data$TotalCharges
)


# ==========================================
# VISUALIZATION 6: CHURN RATE BY INTERNET SERVICE
# ==========================================

internet_churn <- churn_data %>%
  group_by(InternetService) %>%
  summarise(
    Total_Customers = n(),
    Churned_Customers = sum(Churn == "Yes"),
    Churn_Rate = mean(Churn == "Yes") * 100
  )

print(internet_churn, width = Inf)


internet_churn_plot <- ggplot(
  internet_churn,
  aes(
    x = InternetService,
    y = Churn_Rate,
    fill = InternetService
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(Churn_Rate, 1), "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Churn Rate by Internet Service",
    x = "Internet Service",
    y = "Churn Rate (%)"
  ) +
  ylim(0, 50) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

internet_churn_plot
ggsave(
  "plots/06_churn_rate_by_internet_service.png",
  plot = internet_churn_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ==========================================
# VISUALIZATION 7: CHURN RATE BY PAYMENT METHOD
# ==========================================

payment_churn <- churn_data %>%
  group_by(PaymentMethod) %>%
  summarise(
    Total_Customers = n(),
    Churned_Customers = sum(Churn == "Yes"),
    Churn_Rate = mean(Churn == "Yes") * 100
  )

print(payment_churn, width = Inf)

payment_churn_plot <- ggplot(
  payment_churn,
  aes(
    x = PaymentMethod,
    y = Churn_Rate,
    fill = PaymentMethod
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(Churn_Rate, 1), "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Churn Rate by Payment Method",
    x = "Payment Method",
    y = "Churn Rate (%)"
  ) +
  ylim(0, 50) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

payment_churn_plot

ggsave(
  "plots/07_churn_rate_by_payment_method.png",
  plot = payment_churn_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# ==========================================
# VISUALIZATION 8: CHURN RATE ACROSS TENURE GROUPS
# ==========================================

tenure_churn <- churn_data %>%
  group_by(TenureGroup) %>%
  summarise(
    Total_Customers = n(),
    Churned_Customers = sum(Churn == "Yes"),
    Churn_Rate = mean(Churn == "Yes") * 100
  )

print(tenure_churn, width = Inf)

tenure_churn

tenure_churn_plot <- ggplot(
  tenure_churn,
  aes(
    x = TenureGroup,
    y = Churn_Rate,
    group = 1
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0(round(Churn_Rate, 1), "%")),
    vjust = -0.8
  ) +
  labs(
    title = "Customer Churn Rate Across Tenure Groups",
    x = "Customer Tenure",
    y = "Churn Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

tenure_churn_plot

ggsave(
  "plots/08_churn_rate_across_tenure_groups.png",
  plot = tenure_churn_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# ==========================================
# VISUALIZATION 9: CORRELATION HEATMAP
# ==========================================

numerical_data <- churn_data %>%
  select(
    SeniorCitizen,
    tenure,
    MonthlyCharges,
    TotalCharges
  )

correlation_matrix <- cor(numerical_data)

correlation_matrix

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Heatmap of Numerical Variables",
  mar = c(0, 0, 2, 0)
)

png(
  "plots/09_correlation_heatmap.png",
  width = 1200,
  height = 900,
  res = 150
)

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Heatmap of Numerical Variables",
  mar = c(0, 0, 2, 0)
)

dev.off()

# ==========================================
# VISUALIZATION 10: CHURN RATE BY SENIOR CITIZEN STATUS
# ==========================================

senior_churn <- churn_data %>%
  group_by(SeniorCitizen) %>%
  summarise(
    Total_Customers = n(),
    Churned_Customers = sum(Churn == "Yes"),
    Churn_Rate = mean(Churn == "Yes") * 100
  ) %>%
  mutate(
    Customer_Group = ifelse(
      SeniorCitizen == 1,
      "Senior Citizen",
      "Non-Senior Citizen"
    )
  )

print(senior_churn, width = Inf)
senior_churn


senior_churn_plot <- ggplot(
  senior_churn,
  aes(
    x = Customer_Group,
    y = Churn_Rate,
    fill = Customer_Group
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(Churn_Rate, 1), "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Churn Rate by Senior Citizen Status",
    x = "Customer Group",
    y = "Churn Rate (%)"
  ) +
  ylim(0, 50) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

senior_churn_plot

ggsave(
  "plots/10_churn_rate_by_senior_citizen.png",
  plot = senior_churn_plot,
  width = 8,
  height = 5,
  dpi = 300
)
