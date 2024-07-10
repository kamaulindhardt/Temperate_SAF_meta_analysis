```{r}
## Example data preparation
# Assume `agroforestry_data` is your dataset with columns: 'study_id', 'treatment_mean', 'control_mean',
# 'treatment_sd', 'control_sd', 'treatment_n', 'control_n'

# Clean column names
agroforestry_data <- agroforestry_data %>%
  janitor::clean_names()

## Calculate Effect Sizes and Variances
# Compute log-transformed response ratios (lnRR) and their variances
agroforestry_data <- agroforestry_data %>%
  mutate(
    lnRR = log(treatment_mean / control_mean),
    var_lnRR = (treatment_sd^2 / (treatment_n * treatment_mean^2)) + (control_sd^2 / (control_n * control_mean^2))
  )

## Prepare data for meta-analysis
# Aggregate data if necessary
meta_data <- agroforestry_data %>%
  group_by(study_id) %>%
  summarise(
    lnRR = mean(lnRR),
    var_lnRR = mean(var_lnRR),
    n = sum(treatment_n + control_n)
  ) %>%
  ungroup()

## Fit Random-Effects Model
# Random-effects model to account for variability within and between studies
res <- rma(yi = lnRR, vi = var_lnRR, data = meta_data, method = "REML", weights = meta_data$n)

## Print Summary of the Meta-Analysis
summary(res)

## Forest Plot
forest(res, xlab = "Log Response Ratio (lnRR)", slab = meta_data$study_id, cex = 0.8, cex.lab = 1.2)

## Funnel Plot to Check for Publication Bias
funnel(res)

## Sensitivity Analysis
# Influence diagnostics
inf <- influence(res)
plot(inf)

## Interpretation
# Print the estimated overall effect size and confidence interval
cat("Overall Effect Size (lnRR):", res$b, "\n")
cat("95% Confidence Interval:", confint(res)$ci.lb, "to", confint(res)$ci.ub, "\n")

# Check for heterogeneity
cat("Heterogeneity (Q):", res$QE, "\n")
cat("I^2:", res$I2, "%\n")

```



```{r}
## Meta-Analysis for Each Response Variable
results <- list()  # List to store results for each response variable
response_vars <- unique(meta_data$response_variable)  # Get unique response variables

# Loop through each response variable
for (response in response_vars) {
  # Filter data for the current response variable
  data_response <- filter(meta_data, response_variable == response)
  
  # Fit random-effects model using log-transformed response ratios and variances
  res <- rma(yi = lnRR, vi = var_lnRR, data = data_response, method = "REML")
  
  # Store the results in the list
  results[[response]] <- res
  
  # Print summary of the meta-analysis
  cat("\nResponse Variable:", response, "\n")
  print(summary(res))
  
  # Print predicted pooled effect size and corresponding CI/PI
  print(predict(res, transf = exp, digits = 2))
  
  # Generate forest plot
  forest(res, xlab = "Log Response Ratio (lnRR)", slab = data_response$slab, main = response, cex = 0.8, cex.lab = 1.2)
  
  # Generate funnel plot to check for publication bias
  funnel(res)
}
```

```{r}
## Sensitivity Analysis for Each Response Variable
# Loop through each response variable to perform influence diagnostics
for (response in response_vars) {
  res <- results[[response]]  # Get results for the current response variable
  
  # Perform influence diagnostics
  inf <- influence(res)
  
  # Plot influence diagnostics
  plot(inf, main = paste("Influence Diagnostics for", response))
  
  # Print summary statistics for interpretation
  cat("\nResponse Variable:", response, "\n")
  cat("Overall Effect Size (lnRR):", res$b, "\n")
  cat("95% Confidence Interval:", confint(res)$ci.lb, "to", confint(res)$ci.ub, "\n")
  cat("Heterogeneity (Q):", res$QE, "\n")
  cat("I^2:", res$I2, "%\n")
}
```