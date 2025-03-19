if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

if (!require("fixest")) install.packages("fixest")
library(fixest)

final.data = readRDS('data/output/TaxBurden_Data.rds')

# Question 1
final.data.q1 <- final.data %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = if_else(is.na(lag(tax_state)) | tax_state == lag(tax_state), FALSE, TRUE)) %>%
  ungroup() %>%
  filter(Year >= 1970 & Year <= 1985) %>%
  group_by(Year) %>%
  summarize(proportion = mean(tax_change, na.rm = TRUE)) %>%
  ungroup()

tax.change.plot <- ggplot(final.data.q1, aes(x = Year, y = proportion)) +
  geom_col(fill = "steelblue") +
  labs(title = "Proportion of States with a Change in Cigarette Tax (1970-1985)",
       x = "Year",
       y = "Proportion of States") +
  theme_minimal()

# Question 2
# Extract the CPI value for 2012 to use as the base for adjustments
# Adjust tax to 2012 dollars and assume price needs to be adjusted similarly
final.data <- final.data %>%
filter(Year >= 1970 & Year <= 2018) %>%
  mutate(
    tax_cpi = tax_dollar * (229.594 / index),  
    price_cpi = cost_per_pack * (229.594 / index)  
  )

# Calculate average tax and price per year
yearly_averages <- final.data %>%
  group_by(Year) %>%
  summarize(
    average_tax = mean(tax_cpi, na.rm = TRUE),
    average_price = mean(price_cpi, na.rm = TRUE)
  )

# Plotting the average tax and price in 2012 dollars
avtgtax.price.plot <- ggplot(yearly_averages, aes(x = Year)) +
  geom_line(aes(y = average_tax, colour = "Average Tax"), size = 1) +
  geom_line(aes(y = average_price, colour = "Average Price"), size = 1) +
  labs(
    title = "Average Tax and Price of Cigarettes (1970-2018, in 2012 Dollars)",
    x = "Year",
    y = "Amount in 2012 Dollars",
    colour = "Measure"
  ) +
  scale_colour_manual(
    values = c("Average Tax" = "blue", "Average Price" = "red")
  ) +
  theme_minimal()

# Question 3
final.data <- final.data %>%
  mutate(Year = as.integer(Year))

final.data.q3 <- final.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  group_by(state) %>%
  summarize(
    price_1970 = min(price_cpi[Year == 1970], na.rm = TRUE),
    price_2018 = max(price_cpi[Year == 2018], na.rm = TRUE),
    price_increase = price_2018 - price_1970,
    .groups = "drop"
  ) %>%
  arrange(desc(price_increase)) %>%
  slice(1:5)

# Extract the top 5 states
top_states <- final.data.q3$state

# Compute the average number of packs sold per capita for the top 5 states
top_states_data <- final.data %>%
  filter(state %in% top_states) %>%
  group_by(Year, state) %>%
  summarize(avg_packs_sold = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

# Generate the plot
top.states.plot <- ggplot(top_states_data, aes(x = Year, y = avg_packs_sold, color = state)) +
  geom_line(size = 1) +
  geom_point(size = 1, alpha = 0.6) +
  labs(
    title = "Average Packs Sold Per Capita (Top 5 States with Highest Price Increase)",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Question 4
final.data.q4 <- final.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  group_by(state) %>%
  summarize(
    price_1970 = min(price_cpi[Year == 1970], na.rm = TRUE),
    price_2018 = max(price_cpi[Year == 2018], na.rm = TRUE),
    price_increase = price_2018 - price_1970,
    .groups = "drop"
  ) %>%
  arrange(price_increase) %>%  # Arrange in ascending order
  slice(1:5)  # Select the 5 states with the lowest price increases

# Extract the bottom 5 states
bottom_states <- final.data.q4$state

# Compute the average number of packs sold per capita for the bottom 5 states
bottom_states_data <- final.data %>%
  filter(state %in% bottom_states) %>%
  group_by(Year, state) %>%
  summarize(avg_packs_sold = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

# Generate the plot
bottom.states.plot <- ggplot(bottom_states_data, aes(x = Year, y = avg_packs_sold, color = state)) +
  geom_line(size = 1) +
  geom_point(size = 1, alpha = 0.6) +
  labs(
    title = "Average Packs Sold Per Capita (Bottom 5 States with Lowest Price Increase)",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Question 5
# Calculate average packs sold per capita for the top 5 states
top_states_avg <- final.data %>%
  filter(state %in% top_states) %>%
  group_by(Year) %>%
  summarize(avg_packs_top = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

# Calculate average packs sold per capita for the bottom 5 states
bottom_states_avg <- final.data %>%
  filter(state %in% bottom_states) %>%
  group_by(Year) %>%
  summarize(avg_packs_bottom = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

# Merge the two datasets by Year
final.data.q5 <- inner_join(top_states_avg, bottom_states_avg, by = "Year")

# Plot the comparison
merged.states.plot <- ggplot(final.data.q5, aes(x = Year)) + 
  geom_line(aes(y = avg_packs_top, color = "Top 5 Price Increase"), size = 1.2) +
  geom_line(aes(y = avg_packs_bottom, color = "Bottom 5 Price Increase"), size = 1.2) +
  labs(
    title = "Cigarette Sales in States with High vs. Low Price Increases (1970-2018)",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Question 6
library(fixest)

# Filter data for the period 1970-1990 
final.data.q6 <- final.data %>%
  filter(Year >= 1970 & Year <= 1990) %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(price_cpi)
  ) %>%
  drop_na(log_sales, log_price)  # Ensure no missing values in log variables

# Estimate the price elasticity of demand
elasticity_model <- feols(log_sales ~ log_price, data = final.data.q6)

# Print results
summary(elasticity_model)

# Question 7
# Filter data for 1970-1990 and compute log variables
final.data.q7 <- final.data %>%
  filter(Year >= 1970 & Year <= 1990) %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(price_cpi),
    log_tax = log(tax_cpi)
  ) %>%
  drop_na(log_sales, log_price, log_tax)

# IV Regression: Log sales on Log price using Log tax as an instrument
iv_model <- feols(log_sales ~ 1 | log_price ~ log_tax, data = final.data.q7)

# Print results
summary(iv_model)

# Question 8
# First-stage regression: Log price on Log tax
first_stage <- feols(log_price ~ log_tax, data = final.data.q7)

# Print first-stage results
summary(first_stage)

# Reduced-form regression: Log sales on Log tax
reduced_form <- feols(log_sales ~ log_tax, data = final.data.q7)

# Print reduced-form results
summary(reduced_form)

# Question 9

# Filter data for 1991-2015 and compute log variables
final.data.q6b <- final.data %>%
  filter(Year >= 1991 & Year <= 2015) %>%
  mutate(
    log_sales_b = log(sales_per_capita),
    log_price_b = log(price_cpi)
  ) %>%
  drop_na(log_sales_b, log_price_b)

# Estimate OLS model for price elasticity of demand
elasticity_model_b <- feols(log_sales_b ~ log_price_b, data = final.data.q6b)
summary(elasticity_model_b)

# Compute log of total tax
final.data.q7b <- final.data.q6b %>%
  mutate(log_tax_b = log(tax_cpi)) %>%
  drop_na(log_tax_b)
# IV Regression: Log sales on log price using log tax as an instrument
iv_model_b <- feols(log_sales_b ~ 1 | log_price_b ~ log_tax_b, data = final.data.q7b)
summary(iv_model_b)

# First-stage regression: Log price on Log tax
first_stage_b <- feols(log_price_b ~ log_tax_b, data = final.data.q7b)
summary(first_stage_b)

# Reduced-form regression: Log sales on Log tax
reduced_form_b <- feols(log_sales_b ~ log_tax_b, data = final.data.q7b)
summary(reduced_form_b)

# Question 10
# Extract OLS estimates
ols_1970_1990 <- coefficients(elasticity_model)["log_price"]
ols_1991_2015 <- coefficients(elasticity_model_b)["log_price_b"]

# Extract IV estimates
iv_1970_1990 <- coefficients(iv_model)["log_price"]
iv_1991_2015 <- coefficients(iv_model_b)["log_price_b"]

# Create a summary table
elasticity_comparison <- tibble(
  Period = c("1970-1990", "1991-2015"),
  OLS_Elasticity = c(ols_1970_1990, ols_1991_2015),
  IV_Elasticity = c(iv_1970_1990, iv_1991_2015)
)

# Print the table
print(elasticity_comparison)

library(modelsummary)
library(kableExtra)

# Function to format large numbers with commas
f <- function(x) formatC(x, digits = 0, big.mark = ",", format = "f")

# Generate the regression table with compact formatting
modelsummary(list("Estimates"=list("OLS"=elasticity_model, "IV"=iv_model, "OLS"=elasticity_model_b, "IV"=iv_model_b),
                  "Reduced Form"=list("IV"=reduced_form, "IV"=reduced_form_b),
                  "First Stage"=list("IV"=first_stage, "IV"=first_stage_b)),
            shape="rbind",
            coef_map=c('log_price'="Log Price", 'log_price_b'="Log Price", 'fit_log_price'="Log Price (IV)", 
                       'fit_log_price_b'="Log Price (IV)", 'log_tax'="Log Tax", 'log_tax_b'="Log Tax"),
            gof_map=list(list("raw"="nobs", "clean"="N", "fmt"=f), list("raw"="r.squared", "clean"="R2", "fmt"=3)),
            output="kableExtra") %>%
    add_header_above(c(" " = 1, "1970-1990" = 2, "1991-2015" = 2)) %>%
    kable_styling(latex_options = "hold_position")


rm(list=c("final.data", "final.data.q1", "yearly_averages", "final.data.q3", "top_states", "top_states_data"))
save.image("submission2/Hwk3_workspace.RData")

