
```{r}
# Convert moderators to factors
preprocess_data <- function(data, moderators) {
  data <- data %>%
    mutate(across(all_of(moderators), as.factor)) %>%
    as.data.frame()
  return(data)
}

# Validate and adjust V_matrix
validate_v_matrix <- function(data, V_matrix) {
  # Check if rownames match between data and V_matrix
  if (!identical(rownames(V_matrix), rownames(data))) {
    stop("Row names of V_matrix and data do not match!")
  }
  
  # Ensure positive definiteness
  is_positive_definite <- function(mat) {
    if (!isSymmetric(mat)) {
      return(FALSE)
    }
    eigenvalues <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
    return(all(eigenvalues > 0))
  }
  
  if (!is_positive_definite(V_matrix)) {
    cat("Matrix is not positive definite. Adding small diagonal correction...\n")
    diag(V_matrix) <- diag(V_matrix) + 1e-6
  }
  
  return(V_matrix)
}
```


```{r}
# Create subsets for all datasets
test_non_imp <- create_test_subset(non_imp_dataset, V_matrix_non_imp, sample_size = 150)
test_imp <- create_test_subset(imp_dataset, V_matrix_imp, sample_size = 150)
test_non_imp_imputed <- create_test_subset(non_imp_dataset_imputed, V_matrix_non_imp_imputed, sample_size = 150)
test_imp_imputed <- create_test_subset(imp_dataset_imputed, V_matrix_imp_imputed, sample_size = 150)
```
```{r}
identical(rownames(non_imp_dataset), rownames(V_matrix_non_imp))
```

```{r}
identical(rownames(test_non_imp$data), rownames(test_non_imp$V_matrix))
```
```{r}
dim(V_matrix_non_imp) # Should show a square matrix
identical(rownames(V_matrix_non_imp), colnames(V_matrix_non_imp)) # Should be TRUE
```


```{r}
# Define the output directory
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to calculate I² statistic for meta-regression models
# Define calculate_I2 function
calculate_I2 <- function(model, data) {
  # Extract variance components
  tau2 <- sum(model$sigma2)  # Total heterogeneity
  sigma2 <- mean(data$vi, na.rm = TRUE)  # Average within-study variance
  
  # Calculate I² (heterogeneity percentage)
  if (!is.na(tau2) && !is.na(sigma2) && (tau2 + sigma2) > 0) {
    I2 <- (tau2 / (tau2 + sigma2)) * 100
  } else {
    I2 <- NA
  }
  
  return(I2)
}


run_loo_sensitivity <- function(data, V_matrix, moderator_formula) {
  cat("\nRunning LOO sensitivity analysis...\n")
  
  n <- nrow(data)
  loo_results <- vector("list", n)
  
  for (i in seq_len(n)) {
    cat("Removing study:", i, "/", n, "\n")
    
    # Subset the data excluding the i-th study
    loo_data <- data[-i, ]
    V_matrix_loo <- V_matrix[-i, -i]
    
    # Fit the full model on the LOO subset
    model <- tryCatch({
      rma.mv(
        yi = yi,
        V = V_matrix_loo,
        mods = moderator_formula,
        random = list(
          ~ 1 | id_article,
          ~ 1 | id_article/response_variable,
          ~ 1 | exp_id
        ),
        data = loo_data,
        method = "ML",
        control = list(
          optimizer = "optim",
          optim.method = "BFGS",
          iter.max = 1000,
          rel.tol = 1e-8
        )
      )
    }, error = function(e) {
      cat("Error fitting model:", e$message, "\n")
      return(NULL)
    })
    
    # Store results if model fitting was successful
    if (!is.null(model)) {
      loo_results[[i]] <- list(
        study_removed = loo_data$id_article[i],
        estimate = coef(model)[1],
        se = model$se[1],
        I2 = ifelse(length(model$sigma2) > 0, 100 * sum(model$sigma2) / (sum(model$sigma2) + mean(data$vi)), NA),
        AIC = AIC(model),
        BIC = BIC(model),
        LogLik = logLik(model)
      )
    }
  }
  
  return(loo_results)
}
```




**OBS! This is a subset because it takes long with the full datasets**
  ```{r, eval=FALSE}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

##########################################################################
# Run LOO sensitivity analysis on test subsets

results <- list(
  non_imp = process_and_fit_with_loo(test_non_imp, "Non-Imputed Test Subset", moderators),
  imp = process_and_fit_with_loo(test_imp, "Imputed Test Subset", moderators),
  non_imp_imputed = process_and_fit_with_loo(test_non_imp_imputed, "Non-Imputed Imputed Test Subset", moderators),
  imp_imputed = process_and_fit_with_loo(test_imp_imputed, "Imputed Test Subset", moderators)
)

# Save results
saveRDS(
  results,
  file = file.path(output_dir, "meta_model_results_with_loo_test_subsets.rds")
)

cat("Results saved to:", file.path(output_dir, "meta_model_results_with_loo_test_subsets.rds"), "\n")
##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (21/11-24)
# Time difference of 17.70906 mins
# Time difference of 12.06283 mins
```

Diagnostics of the LOO model fitting

```{r}
# Visualize fixed effects
ggplot(all_fixed_effects, aes(x = estimate, y = term, color = dataset)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.3, position = position_dodge(width = 0.5)) +
  facet_wrap(~ response_variable, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparison of Fixed Effects Across Models by Response Variable",
    subtitle = "Estimates with 95% Confidence Intervals",
    x = "Estimate",
    y = "Fixed Effect Term",
    color = "Dataset"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )
```

```{r}
# Check if a matrix is symmetric positive definite
is_positive_definite <- function(mat) {
  if (!isSymmetric(mat)) {
    return(FALSE)
  }
  eigenvalues <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  return(all(eigenvalues > 0))
}

# Adjust the V_matrix to make it symmetric positive definite
make_positive_definite <- function(V_matrix) {
  if (!is_positive_definite(V_matrix)) {
    diag(V_matrix) <- diag(V_matrix) + 1e-6
  }
  return(V_matrix)
}

# Validate and align the dataset and its V_matrix
validate_data_and_v_matrix <- function(data, V_matrix) {
  # Symmetrize V_matrix if needed
  if (!isSymmetric(V_matrix)) {
    V_matrix <- (V_matrix + t(V_matrix)) / 2
    cat("V_matrix has been symmetrized.\n")
  }
  
  # Ensure positive definiteness
  if (!is_positive_definite(V_matrix)) {
    diag(V_matrix) <- diag(V_matrix) + 1e-6
    cat("V_matrix adjusted for positive definiteness.\n")
  }
  
  # Validate row alignment between data and V_matrix
  if (!identical(rownames(V_matrix), rownames(data))) {
    stop("Row names of V_matrix and data do not match!")
  }
  
  return(V_matrix)
}

# Function to create a test subset of data and its associated V_matrix
create_test_subset <- function(data, V_matrix, sample_size = 150) {
  if (nrow(data) < sample_size) {
    stop("Sample size is larger than the dataset.")
  }
  
  # Ensure rownames exist
  if (is.null(rownames(data))) {
    rownames(data) <- seq_len(nrow(data))
  }
  if (is.null(rownames(V_matrix))) {
    rownames(V_matrix) <- colnames(V_matrix) <- seq_len(nrow(V_matrix))
  }
  
  # Sample row indices
  sampled_indices <- sample(rownames(data), size = sample_size, replace = FALSE)
  
  # Subset data and V_matrix
  data_subset <- data[sampled_indices, , drop = FALSE]
  V_matrix_subset <- V_matrix[sampled_indices, sampled_indices, drop = FALSE]
  
  # Validate alignment
  if (!identical(rownames(data_subset), rownames(V_matrix_subset))) {
    stop("Row names of V_matrix and data do not match!")
  }
  
  # Return validated subsets
  list(data = data_subset, V_matrix = V_matrix_subset)
}
```





























```{r}
# Apply the function to all datasets
non_imp_results <- process_loo_diagnostics(diagnostics_non_imp, "Non-Imputed Data")
imp_results <- process_loo_diagnostics(diagnostics_imp, "Imputed Data")
non_imp_imputed_results <- process_loo_diagnostics(diagnostics_non_imp_imputed, "Non-Imputed Imputed Data")
imp_imputed_results <- process_loo_diagnostics(diagnostics_imp_imputed, "Imputed Imputed Data")

non_imp_results
imp_results
non_imp_imputed_results
imp_imputed_results
```

```{r}
# Debugging a single LOO iteration
loo_data <- test_non_imp$data[-1, ]
V_matrix_loo <- test_non_imp$V_matrix[-1, -1]

model <- rma.mv(
  yi = yi,
  V = V_matrix_loo,
  mods = as.formula("~ tree_type + crop_type + age_system + season + soil_texture"),
  random = list(
    ~ 1 | id_article,
    ~ 1 | id_article/response_variable,
    ~ 1 | exp_id
  ),
  data = loo_data,
  method = "ML",
  control = list(
    optimizer = "optim",
    optim.method = "BFGS",
    iter.max = 1000,
    rel.tol = 1e-8
  )
)

# Check diagnostics
AIC(model)
BIC(model)
logLik(model)

```

```{r}
# Load the LOO results
loo_results <- readRDS(file.path(output_dir, "meta_model_results_with_loo_test_subsets.rds"))

# Inspect the structure
# str(loo_results)

loo_results
```

Assessing the model-datasets fexed effects

```{r}
str(loo_results$non_imp$loo_results[[1]])

# loo_results$non_imp$model
# loo_results$imp$model
# loo_results$non_imp_imputed$model
# str(loo_results$imp_imputed$model)
```

```{r}
extract_fixed_effects <- function(model) {
  # Check if the model and required components exist
  if (!is.null(model)) {
    # Extract the term names from `b` (or fallback to sequential labels)
    term_names <- rownames(model$b)
    if (is.null(term_names)) {
      term_names <- paste0("term_", seq_along(model$beta))
    }
    
    # Ensure all components are properly aligned and handle missing ones
    num_terms <- length(model$beta)
    estimate <- model$beta
    se <- model$se
    zval <- model$zval
    pval <- model$pval
    ci.lb <- model$ci.lb
    ci.ub <- model$ci.ub
    
    # Handle cases where any component is missing
    if (is.null(se) || length(se) != num_terms) se <- rep(NA, num_terms)
    if (is.null(zval) || length(zval) != num_terms) zval <- rep(NA, num_terms)
    if (is.null(pval) || length(pval) != num_terms) pval <- rep(NA, num_terms)
    if (is.null(ci.lb) || length(ci.lb) != num_terms) ci.lb <- rep(NA, num_terms)
    if (is.null(ci.ub) || length(ci.ub) != num_terms) ci.ub <- rep(NA, num_terms)
    
    # Create a data frame
    fixed_effects <- data.frame(
      term = term_names,
      estimate = estimate,
      se = se,
      zval = zval,
      pval = pval,
      ci.lb = ci.lb,
      ci.ub = ci.ub,
      stringsAsFactors = FALSE
    )
    return(fixed_effects)
  }
  
  # Return an empty data frame if the model is invalid
  return(data.frame(
    term = character(0),
    estimate = numeric(0),
    se = numeric(0),
    zval = numeric(0),
    pval = numeric(0),
    ci.lb = numeric(0),
    ci.ub = numeric(0),
    stringsAsFactors = FALSE
  ))
}
```

```{r}
fixed_effects_non_imp <- extract_fixed_effects(loo_results$non_imp$model)
fixed_effects_imp <- extract_fixed_effects(loo_results$imp$model)
fixed_effects_non_imp_imputed <- extract_fixed_effects(loo_results$non_imp_imputed$model)
fixed_effects_imp_imputed <- extract_fixed_effects(loo_results$imp_imputed$model)

# Add dataset identifiers
fixed_effects_non_imp$dataset <- "Non-Imputed Dataset"
fixed_effects_imp$dataset <- "Imputed Dataset"
fixed_effects_non_imp_imputed$dataset <- "Non-Imputed Imputed Dataset"
fixed_effects_imp_imputed$dataset <- "Imputed Imputed Dataset"

# Combine all datasets
all_fixed_effects <- bind_rows(
  fixed_effects_non_imp,
  fixed_effects_imp,
  fixed_effects_non_imp_imputed,
  fixed_effects_imp_imputed
)

# View the combined fixed effects
head(all_fixed_effects)
```
Visually assessing

```{r}
# Prepare the data
all_fixed_effects <- all_fixed_effects %>%
  mutate(
    # Categorize significance levels
    signif = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*",
      pval < 0.1 ~ ".",
      TRUE ~ ""
    )
  )

# Create a forest plot for fixed effects
all_fixed_effects |> 
  ggplot(aes(x = estimate, y = term, color = dataset)) +
  geom_point(size = 3, position = position_dodge(width = 0.8)) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = signif), vjust = -1, hjust = -0.3, position = position_dodge(width = 0.8), size = 3.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparison of Fixed Effects Across Models and Datasets",
    subtitle = "Estimates with 95% Confidence Intervals",
    x = "Estimate",
    y = "Fixed Effect Term",
    color = "Dataset"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~dataset, scales = "free", ncol = 2)
```
```{r}
# Assuming the data used in the model is stored in `loo_results$<model>$model$data`
reference_levels <- sapply(
  loo_results$imp_imputed$model$data %>% select(where(is.factor)),
  levels
)
# View the reference group levels
print(sapply(reference_levels, function(x) x[1]))

```

```{r}
all_fixed_effects |> 
  ggplot(aes(x = estimate, y = term, color = dataset)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.3, position = position_dodge(width = 0.5)) +
  facet_grid(term ~ response_variable, scales = "free", space = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparison of Fixed Effects Across Models and Response Variables",
    subtitle = "Estimates with 95% Confidence Intervals",
    x = "Estimate",
    y = "Fixed Effect Term",
    color = "Dataset"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

```


Assessing I2, AIC, BIC, LogLik, k.all

```{r}
# Function to extract LOO diagnostics including I2, AIC, BIC, LogLik, and k.all
extract_loo_diagnostics <- function(loo_results) {
  # Loop through LOO results and extract diagnostics
  diagnostics_list <- lapply(seq_along(loo_results), function(i) {
    res <- loo_results[[i]]
    if (!is.null(res)) {
      data.frame(
        study_removed = res$study_removed,
        estimate = res$estimate, 
        se = res$se,
        I2 = ifelse(!is.null(res$I2), res$I2[1], NA),  # Extract I2 if available
        AIC = ifelse(!is.null(res$AIC), res$AIC, NA),  # Extract AIC
        BIC = ifelse(!is.null(res$BIC), res$BIC, NA),  # Extract BIC
        LogLik = ifelse(!is.null(res$LogLik), res$LogLik, NA),  # Extract LogLik
        k_all = ifelse(!is.null(res$k_all), res$k_all, NA)  # Extract k.all if available
      )
    } else {
      # Return NA for all fields if the result is NULL
      data.frame(
        study_removed = i,
        estimate = NA,
        se = NA,
        I2 = NA,
        AIC = NA,
        BIC = NA,
        LogLik = NA,
        k_all = NA
      )
    }
  })
  
  # Combine into a single data frame
  do.call(rbind, diagnostics_list)
}
```

```{r}
# Extract diagnostics for each test result
diagnostics_non_imp <- extract_loo_diagnostics(loo_results$non_imp$loo_results)
diagnostics_imp <- extract_loo_diagnostics(loo_results$imp$loo_results)
diagnostics_non_imp_imputed <- extract_loo_diagnostics(loo_results$non_imp_imputed$loo_results)
diagnostics_imp_imputed <- extract_loo_diagnostics(loo_results$imp_imputed$loo_results)
```
```{r}
# Add a dataset column to each data frame
diagnostics_non_imp$dataset <- "non_imp"
diagnostics_imp$dataset <- "imp"
diagnostics_non_imp_imputed$dataset <- "non_imp_imputed"
diagnostics_imp_imputed$dataset <- "imp_imputed"

# Combine all diagnostics into a single data frame
all_diagnostics_df <- bind_rows(
  diagnostics_non_imp,
  diagnostics_imp,
  diagnostics_non_imp_imputed,
  diagnostics_imp_imputed
)

# Preview the combined diagnostics
head(all_diagnostics_df)

```


```{r}
process_loo_diagnostics <- function(diagnostics, dataset_name) {
  if (nrow(diagnostics) == 0) {
    cat("The diagnostics dataset is empty for:", dataset_name, "\n")
    return(NULL)
  }
  
  # Filter rows with complete cases for estimate and se only
  diagnostics_filtered <- diagnostics %>%
    select(study_removed, estimate, se) %>%
    filter(!is.na(estimate) & !is.na(se))
  
  if (nrow(diagnostics_filtered) == 0) {
    cat("No valid Estimate and SE values for diagnostics in:", dataset_name, "\n")
    return(NULL)
  }
  
  # Reshape the data for plotting
  diagnostics_long <- diagnostics_filtered %>%
    pivot_longer(cols = c(estimate, se), names_to = "Metric", values_to = "Value")
  
  # Plot Estimate and SE
  estimate_se_plot <- ggplot(diagnostics_long, aes(x = factor(study_removed), y = Value, color = Metric)) +
    geom_point() +
    geom_line(aes(group = Metric)) +
    labs(
      title = paste("LOO Sensitivity Analysis: Estimate and SE (", dataset_name, ")", sep = ""),
      x = "Study Removed", y = "Value"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(list(estimate_se_plot = estimate_se_plot, filtered_diagnostics = diagnostics_filtered))
}
```





####################################################################################################################################################













































Moderator Analysis (Subgroup and Meta-Regression)

```{r}
# Function for subgroup analysis and meta-regression
run_moderator_analysis <- function(data, V_matrix, moderator) {
  cat("\nAnalyzing moderator:", moderator, "\n")
  
  # Check if the moderator has enough levels for analysis
  if (length(unique(data[[moderator]])) < 2) {
    cat("Insufficient levels for moderator:", moderator, "\n")
    return(NULL)
  }
  
  # Meta-regression model with the moderator
  tryCatch({
    model <- rma.mv(
      yi = yi,
      V = V_matrix,
      mods = as.formula(paste("~", moderator)),
      random = list(~ 1 | id_article, 
                    ~ 1 | id_article/response_variable, 
                    ~ 1 | exp_id),
      data = data,
      method = "REML"
    )
    return(summary(model))
  }, error = function(e) {
    cat("Error in meta-regression for moderator:", moderator, "-", e$message, "\n")
    return(NULL)
  })
}
```





```{r}
# Function to check if a matrix is positive definite
is_positive_definite <- function(V_matrix) {
  eigen_values <- eigen(V_matrix, symmetric = TRUE)$values
  all(eigen_values > 0)
}

# Function to regularize the V_matrix if it's not positive definite
regularize_v_matrix <- function(V_matrix) {
  cat("Regularizing V_matrix to ensure positive definiteness...\n")
  epsilon <- 1e-6  # Small value to add to diagonal
  diag(V_matrix) <- diag(V_matrix) + epsilon
  return(V_matrix)
}
```

```{r}
# Updated helper function to subset the V_matrix and check positive definiteness
subset_v_matrix <- function(V_matrix, data_subset) {
  # Update the rownames of the data subset to character type
  filtered_ids <- as.character(data_subset$id_article)
  
  # Intersect the filtered ids with the rownames of V_matrix
  matching_ids <- intersect(filtered_ids, rownames(V_matrix))
  
  # Check if there are any matching rownames
  if (length(matching_ids) == 0) {
    cat("Warning: No matching rownames found between filtered data and V_matrix. Returning NULL.\n")
    return(NULL)
  }
  
  # Subset the V_matrix using the matching rownames
  V_matrix_subset <- V_matrix[matching_ids, matching_ids, drop = FALSE]
  
  # Check if the V_matrix_subset is empty
  if (nrow(V_matrix_subset) == 0 || ncol(V_matrix_subset) == 0) {
    cat("Warning: Subset V_matrix is empty. Skipping analysis.\n")
    return(NULL)
  }
  
  # Check positive definiteness
  if (!is_positive_definite(V_matrix_subset)) {
    cat("Warning: V_matrix is not positive definite. Regularizing...\n")
    V_matrix_subset <- regularize_v_matrix(V_matrix_subset)
  }
  
  return(V_matrix_subset)
}
```

```{r}
# Moderator analysis for each response subset
moderator_results <- lapply(response_variables, function(response_var) {
  cat("\nAnalyzing response variable:", response_var, "\n")
  
  # Step 1: Get the data subset
  data_subset <- response_splits$non_imp[[response_var]]
  V_matrix <- v_matrices$v_matrix_non_imp[[response_var]]
  
  # Step 2: Filter the data for valid moderators and handle single-level factors
  filtered_output <- filter_data_for_moderator_analysis(data_subset, moderators)
  filtered_data <- filtered_output$data
  valid_moderators <- filtered_output$valid_moderators
  
  # Step 3: Subset the V_matrix based on filtered data
  V_matrix_subset <- subset_v_matrix(V_matrix, filtered_data)
  if (is.null(V_matrix_subset)) {
    cat("Skipping analysis due to issues with V_matrix.\n")
    return(NULL)
  }
  
  # Step 4: Run moderator analysis only if there are valid moderators and enough data
  if (nrow(filtered_data) > 1 && length(valid_moderators) > 0) {
    lapply(valid_moderators, run_moderator_analysis, data = filtered_data, V_matrix = V_matrix_subset)
  } else {
    cat("Insufficient data or no valid moderators after filtering. Skipping analysis for this subset.\n")
    NULL
  }
})

# Name the list of results by response variables
names(moderator_results) <- response_variables
```

```{r}
# Summary of moderator analysis results
summary_results <- lapply(moderator_results, function(response_result) {
  if (is.null(response_result)) {
    return("Skipped")
  } else {
    sapply(response_result, function(mod_result) {
      if (is.null(mod_result)) {
        return("Failed")
      } else {
        return("Success")
      }
    })
  }
})

print(summary_results)
```

```{r}
# Extract successful results
successful_results <- lapply(moderator_results, function(response_result) {
  if (!is.null(response_result)) {
    Filter(Negate(is.null), response_result)
  } else {
    return(NULL)
  }
})

# Display summaries of successful models
for (response_var in names(successful_results)) {
  cat("\nResults for response variable:", response_var, "\n")
  if (length(successful_results[[response_var]]) > 0) {
    lapply(successful_results[[response_var]], summary)
  } else {
    cat("No successful models found for this response variable.\n")
  }
}

```




```{r}
# Save plots if available
# if (!is.null(non_imp_results)) {
#   ggsave(non_imp_results$estimate_se_plot, filename = file.path(output_dir, "estimate_se_non_imp.png"), width = 8, height = 6)
# }
# 
# if (!is.null(imp_results)) {
#   ggsave(imp_results$estimate_se_plot, filename = file.path(output_dir, "estimate_se_imp.png"), width = 8, height = 6)
# }
# 
# if (!is.null(non_imp_imputed_results)) {
#   ggsave(non_imp_imputed_results$estimate_se_plot, filename = file.path(output_dir, "estimate_se_non_imp_imputed.png"), width = 8, height = 6)
# }
# 
# if (!is.null(imp_imputed_results)) {
#   ggsave(imp_imputed_results$estimate_se_plot, filename = file.path(output_dir, "estimate_se_imp_imputed.png"), width = 8, height = 6)
# }
```


```{r}
# Helper function to extract LOO metrics into a tidy format
extract_loo_metrics <- function(loo_results) {
  # Loop through results and extract key metrics
  metrics_list <- lapply(seq_along(loo_results), function(i) {
    res <- loo_results[[i]]  # Access the LOO result for the i-th study removed
    if (!is.null(res)) {
      # Extract relevant fields
      data.frame(
        study_removed = res$study_removed,
        estimate = ifelse(!is.null(res$estimate), res$estimate, NA),
        se = ifelse(!is.null(res$se), res$se, NA),
        zval = ifelse(!is.null(res$zval), res$zval, NA),
        pval = ifelse(!is.null(res$pval), res$pval, NA),
        ci.lb = ifelse(!is.null(res$ci.lb), res$ci.lb, NA),
        ci.ub = ifelse(!is.null(res$ci.ub), res$ci.ub, NA),
        signif = ifelse(!is.null(res$pval) && res$pval < 0.05, "**", "")
      )
    } else {
      # Return NA if the result is NULL
      data.frame(
        study_removed = i,
        estimate = NA,
        se = NA,
        zval = NA,
        pval = NA,
        ci.lb = NA,
        ci.ub = NA,
        signif = NA
      )
    }
  })
  
  # Combine into a single data frame
  metrics_df <- do.call(rbind, metrics_list)
  
  # Unnest estimates if they're named vectors (e.g., intrcpt/moderators)
  if (!is.null(metrics_df$estimate[[1]]) && length(metrics_df$estimate[[1]]) > 1) {
    metrics_df <- tidyr::unnest(metrics_df, c(estimate, se, zval, pval, ci.lb, ci.ub, signif))
  }
  
  return(metrics_df)
}
```

```{r}
# Extract metrics for each dataset
diagnostics_non_imp <- extract_loo_metrics(loo_results$non_imp$loo_results)
diagnostics_imp <- extract_loo_metrics(loo_results$imp$loo_results)
diagnostics_non_imp_imputed <- extract_loo_metrics(loo_results$non_imp_imputed$loo_results)
diagnostics_imp_imputed <- extract_loo_metrics(loo_results$imp_imputed$loo_results)

# Combine all diagnostics into a single list
all_diagnostics <- list(
  non_imp = diagnostics_non_imp,
  imp = diagnostics_imp,
  non_imp_imputed = diagnostics_non_imp_imputed,
  imp_imputed = diagnostics_imp_imputed
)

# Add dataset names and combine into one dataframe
all_diagnostics_df <- purrr::imap_dfr(all_diagnostics, ~ mutate(.x, dataset = .y))

# Preview the combined diagnostics
head(all_diagnostics_df)
```


```{r}
# Plot estimates with CIs for each dataset
all_diagnostics_df |> 
  ggplot(aes(x = study_removed, y = estimate, color = dataset)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  facet_wrap(~ dataset) +
  theme_minimal() +
  labs(
    title = "Leave-One-Out Sensitivity Diagnostics",
    x = "Study Removed",
    y = "Estimate (with 95% CI)"
  )
```




Create test sub-set of data (because it takes too long to run the code)

```{r}
# Helper functions for processing - (validate, and subset creation of data and v_matrix)
is_positive_definite <- function(mat) {
  if (!isSymmetric(mat)) {
    return(FALSE)
  }
  eigenvalues <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  return(all(eigenvalues > 0))
}

make_positive_definite <- function(V_matrix) {
  if (!is_positive_definite(V_matrix)) {
    diag(V_matrix) <- diag(V_matrix) + 1e-6
  }
  return(V_matrix)
}

validate_data_and_v_matrix <- function(data, V_matrix) {
  if (!isSymmetric(V_matrix)) {
    V_matrix <- (V_matrix + t(V_matrix)) / 2
    cat("V_matrix has been symmetrized.\n")
  }
  
  if (!is_positive_definite(V_matrix)) {
    diag(V_matrix) <- diag(V_matrix) + 1e-6
    cat("V_matrix adjusted for positive definiteness.\n")
  }
  
  if (!identical(rownames(V_matrix), rownames(data))) {
    stop("Row names of V_matrix and data do not match!")
  }
  
  return(V_matrix)
}

# Subset creation helper
# Updated function for creating a test subset
create_test_subset <- function(data, V_matrix, sample_size = 150) {
  if (nrow(data) < sample_size) {
    stop("Sample size is larger than the dataset.")
  }
  
  # Ensure both data and V_matrix have row names
  if (is.null(rownames(data))) {
    rownames(data) <- seq_len(nrow(data))
  }
  if (is.null(rownames(V_matrix))) {
    rownames(V_matrix) <- colnames(V_matrix) <- seq_len(nrow(V_matrix))
  }
  
  # Sample row indices (ensure the sampled indices are consistent)
  sampled_indices <- sample(rownames(data), size = sample_size, replace = FALSE)
  
  # Subset data and V_matrix using the sampled indices
  data_subset <- data[sampled_indices, , drop = FALSE]
  V_matrix_subset <- V_matrix[sampled_indices, sampled_indices, drop = FALSE]
  
  # Validate alignment
  if (!identical(rownames(data_subset), rownames(V_matrix_subset))) {
    stop("Row names of V_matrix and data do not match!")
  }
  
  # Return subsets
  list(data = data_subset, V_matrix = V_matrix_subset)
}
```


```{r,  eval=FALSE}
extract_diagnostics <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }
  list(
    AIC = model$aic,
    BIC = model$bic,
    LogLik = model$loglik,
    I2 = 100 * model$tau2 / (model$tau2 + model$sigma2),
    QM = model$QM,
    QM_pval = model$pval
  )
}

# Use the function
full_model_diagnostics <- extract_diagnostics(full_model)
print(full_model_diagnostics)

```



**OBS! This code takes extremely long to run ~ 2 days!**
  
  ```{r, eval=FALSE}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

####################################################################################################
# Run LOO sensitivity analysis on all datasets
loo_non_imp <- run_loo_sensitivity(non_imp_dataset, V_matrix_non_imp, moderator_formula)
loo_imp <- run_loo_sensitivity(imp_dataset, V_matrix_imp, moderator_formula)
loo_non_imp_imputed <- run_loo_sensitivity(non_imp_dataset_imputed, V_matrix_non_imp_imputed, moderator_formula)
loo_imp_imputed <- run_loo_sensitivity(imp_dataset_imputed, V_matrix_imp_imputed, moderator_formula)

####################################################################################################
# Define the output directory
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

# Ensure the directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the LOO results
saveRDS(
  list(
    loo_non_imp = loo_non_imp,
    loo_imp = loo_imp,
    loo_non_imp_imputed = loo_non_imp_imputed,
    loo_imp_imputed = loo_imp_imputed
  ),
  file = file.path(output_dir, "loo_sensitivity_results_full_dataset.rds")
)

cat("LOO sensitivity results saved to:", file.path(output_dir, "loo_sensitivity_results_full_dataset.rds"), "\n")

##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (18/11-24)
# 
```




```{r}
split_by_response_variable <- function(test_data, selected_response_variables = NULL) {
  data <- test_data$data
  V_matrix <- test_data$V_matrix
  
  # Optionally filter for selected response variables
  if (!is.null(selected_response_variables)) {
    data <- data[data$response_variable %in% selected_response_variables, ]
    row_indices <- rownames(data)
    V_matrix <- V_matrix[row_indices, row_indices, drop = FALSE]
  }
  
  # Split data by response variable
  response_splits <- split(data, data$response_variable)
  
  # Align each subset's V_matrix
  splits <- lapply(response_splits, function(sub_data) {
    indices <- rownames(sub_data)
    V_matrix_subset <- V_matrix[indices, indices, drop = FALSE]
    
    # Validate alignment
    validate_data_and_v_matrix(sub_data, V_matrix_subset)
    list(data = sub_data, V_matrix = V_matrix_subset)
  })
  
  return(splits)
}
```

```{r}
# Define the selected response variables
selected_responses <- c("Crop yield", "Soil quality", "Biodiversity")


####################################################################################################

# Create splits for the non-imputed dataset
split_non_imp <- split_by_response_variable(test_non_imp, 
                                            selected_response_variables = selected_responses)

# Create splits for the imputed dataset
split_imp <- split_by_response_variable(test_imp, 
                                        selected_response_variables = selected_responses)

# Create splits for the non-imputed, imputed dataset
split_non_imp_imputed <- split_by_response_variable(test_non_imp_imputed, 
                                                    selected_response_variables = selected_responses)

# Create splits for the imputed, imputed dataset
split_imp_imputed <- split_by_response_variable(test_imp_imputed, 
                                                selected_response_variables = selected_responses)

# View sneak-peak
split_non_imp |> glimpse()
```

```{r}
split_non_imp$Biodiversity$data |> glimpse()

split_non_imp$Biodiversity$V_matrix |> glimpse()
```
```{r}
validate_data_and_v_matrix <- function(data, V_matrix) {
  if (!identical(rownames(data), rownames(V_matrix))) {
    stop("Row names of data and V_matrix do not match!")
  }
}

# Validate the split
split_non_imp$Biodiversity$data |> glimpse()
split_non_imp$Biodiversity$V_matrix |> glimpse()
```
```{r}
# Define model formulas
# as.formula("yi ~ 0 + tree_type + crop_type + age_system + season + soil_texture")
# Define the moderator formula
moderator_formula <- as.formula("~ tree_type + soil_texture")

# Define the random effects formula
random_effects_formula <- as.formula("~ 1 | id_article/exp_id")


# random_effects_formula <- list(~ 1 | id_article,
#                                ~ 1 | id_article/response_variable,
#                                ~ 1 | exp_id)
```


**OBS! This is a subset because it takes long with the full datasets**
  ```{r, eval=FALSE}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

# Define a directory to save results
output_model_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
if (!dir.exists(output_model_dir)) {
  dir.create(output_model_dir, recursive = TRUE)
}

##########################################################################
# Run LOO sensitivity analysis on test subsets and each response variable split with integrated saving and collect results

# Run LOO sensitivity analysis
results <- list(
  non_imp = run_loo_sensitivity_for_splits(
    splits = split_non_imp,
    moderator_formula = moderator_formula,
    random_effects = random_effects_formula,
    output_dir = file.path(output_model_dir, "Non_Imputed")
  ),
  imp = run_loo_sensitivity_for_splits(
    splits = split_imp,
    moderator_formula = moderator_formula,
    random_effects = random_effects_formula,
    output_dir = file.path(output_model_dir, "Imputed")
  ),
  non_imp_imputed = run_loo_sensitivity_for_splits(
    splits = split_non_imp_imputed,
    moderator_formula = moderator_formula,
    random_effects = random_effects_formula,
    output_dir = file.path(output_model_dir, "Non_Imputed_Imputed")
  ),
  imp_imputed = run_loo_sensitivity_for_splits(
    splits = split_imp_imputed,
    moderator_formula = moderator_formula,
    random_effects = random_effects_formula,
    output_dir = file.path(output_model_dir, "Imputed_Imputed")
  )
)

# Save all results
results_path <- file.path(output_model_dir, "meta_model_results_with_loo_test_subsets.rds")
saveRDS(results, file = results_path)
cat("Results saved to:", results_path, "\n")


# str(loo_non_imp)



##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (21/11-24)
# Time difference of 17.70906 mins
# Time difference of 12.06283 mins
# Time difference of 1.435304 mins

```

Diagnostics of LOO sensitivity analysis

```{r}
# Load results from saved RDS file
results_path <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R", "meta_model_results_with_loo_test_subsets.rds")
if (file.exists(results_path)) {
  results <- readRDS(results_path)
  cat("Results loaded from:", results_path, "\n")
} else {
  stop("Results file not found! Please run the model fitting and saving step first.")
}
```

```{r}
#str(results)  

# Example for Non-Imputed Dataset
str(results$non_imp)

results$non_imp[[1]] |> 
  glimpse()

# Confirm the presence of key components
names(results$non_imp[[1]])
# Check 'model' and 'data' structures
glimpse(results$non_imp[[1]]$model)
glimpse(results$non_imp[[1]]$data)

```

```{r}
valid_model_objects <- purrr::keep(results$non_imp, ~ is.list(.x) && !is.null(.x$model))
valid_model_objects
```


```{r}
extract_forestplot_data <- function(model_object, moderator) {
  # Extract relevant components
  beta <- model_object$model$beta[, 1]
  se <- model_object$model$se
  ci.lb <- model_object$model$ci.lb
  ci.ub <- model_object$model$ci.ub
  pval <- model_object$model$pval
  term_names <- rownames(model_object$model$beta)
  
  # Match terms for the specified moderator
  matching_terms <- grep(moderator, term_names, ignore.case = TRUE, value = TRUE)
  if (length(matching_terms) == 0) {
    return(tibble())  # Return empty tibble if moderator not found
  }
  
  # Construct tibble for matching terms
  forestplot_data <- tibble(
    term = term_names,
    estimate = beta,
    std_error = se,
    ci_lower = ci.lb,
    ci_upper = ci.ub,
    p_value = pval
  ) %>%
    filter(term %in% matching_terms) %>%
    mutate(
      moderator_level = gsub(".*_", "", term)  # Extract only the level
    )
  
  return(forestplot_data)
}
```

```{r}
forest_plot_data <- purrr::map_dfr(valid_model_objects, ~ {
  purrr::map_dfr(selected_responses, function(response_variable) {
    tryCatch(
      {
        data <- extract_forestplot_data(
          .x,
          moderator = selected_moderator
        )
        data <- data %>% mutate(response_variable = response_variable)
        data
      },
      error = function(e) {
        message("Error in data extraction: ", e$message)
        tibble()
      }
    )
  })
})

# Inspect final data
forest_plot_data |> glimpse() 
```

```{r}
forest_plot_data_agg <- forest_plot_data %>%
  group_by(response_variable, moderator_level) %>%
  summarize(
    yi = mean(estimate, na.rm = TRUE),       # Mean effect size
    lower_ci = mean(ci_lower, na.rm = TRUE), # Mean lower CI
    upper_ci = mean(ci_upper, na.rm = TRUE), # Mean upper CI
    .groups = "drop"                         # Drop grouping
  )

forest_plot_data_agg |> glimpse()
```

```{r}
forest_plot <- forest_plot_data_agg %>%
  ggplot(aes(x = yi, y = moderator_level, color = response_variable)) +
  geom_point(size = 3) +  # Effect size
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # Confidence intervals
  facet_wrap(~response_variable, scales = "free_x", ncol = 1) +  # Facet by response variable
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
  labs(
    title = glue("Forest Plot: {selected_moderator}"),
    x = "Effect Size (Estimate)",
    y = "Moderator Levels"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )

forest_plot
```

```{r}
str(results$Biodiversity$model)
```
```{r}
if (is.null(results[[response_var]]$model)) {
  stop(paste("Model is missing for response variable:", response_var))
}
```
```{r}
print(paste("Processing response variable:", response_var))
print("Model structure:")
print(str(results[[response_var]]$model))

```
```{r}
overall_summary <- tryCatch({
  data.frame(
    id_obs = NA,  # Placeholder for meta-analysis
    estimate = model$b[1, 1],
    ci_lb = model$ci.lb[1],
    ci_ub = model$ci.ub[1],
    study = "Meta-analysis",
    response_var = response_var
  )
}, error = function(e) {
  message(paste("Error in overall_summary for", response_var, ":", e$message))
  data.frame()  # Return an empty data frame instead of NULL
})

```

```{r}
if (nrow(overall_summary) == 0) {
  message(paste("Skipping response variable due to empty overall_summary:", response_var))
  return(NULL)
}

```

```{r}
print("Overall summary created:")
print(overall_summary)

```
```{r}
prepare_forest_data <- function(results, response_var, meta_data) {
  # Extract meta-analysis model and LOO results
  model <- results[[response_var]]$model
  loo_results <- results[[response_var]]$loo_results
  
  # Check if the model or LOO results are NULL
  if (is.null(model) || is.null(loo_results)) {
    message(paste("Skipping", response_var, "due to missing model or LOO results"))
    return(NULL)
  }
  
  # Prepare overall meta-analysis summary
  overall_summary <- tryCatch({
    data.frame(
      id_obs = NA,  # Placeholder for meta-analysis
      estimate = model$b[1, 1],
      ci_lb = model$ci.lb[1],
      ci_ub = model$ci.ub[1],
      study = "Meta-analysis",
      response_var = response_var
    )
  }, error = function(e) {
    message(paste("Error creating overall_summary for", response_var, ":", e$message))
    data.frame()
  })
  
  # Check if overall_summary was successfully created
  if (nrow(overall_summary) == 0) {
    message(paste("Skipping response variable due to empty overall_summary:", response_var))
    return(NULL)
  }
  
  # Safely extract LOO results
  loo_data <- tryCatch({
    data.frame(
      id_obs = seq_along(loo_results),
      estimate = sapply(loo_results, function(res) if (!is.null(res["intrcpt"])) res["intrcpt"] else NA),
      ci_lb = sapply(loo_results, function(res) if (!is.null(res["intrcpt"])) res["intrcpt"] - 1.96 * model$se[1] else NA),
      ci_ub = sapply(loo_results, function(res) if (!is.null(res["intrcpt"])) res["intrcpt"] + 1.96 * model$se[1] else NA),
      study = "LOO Observations",
      response_var = response_var
    )
  }, error = function(e) {
    message(paste("Error creating LOO data for", response_var, ":", e$message))
    return(NULL)
  })
  
  # Handle cases where LOO data creation fails
  if (is.null(loo_data) || nrow(loo_data) == 0) {
    message(paste("No valid LOO data for", response_var))
    return(overall_summary)
  }
  
  # Merge LOO data with metadata
  merged_data <- tryCatch({
    merge(loo_data, meta_data, by.x = "id_obs", by.y = "id_obs", all.x = TRUE)
  }, error = function(e) {
    message(paste("Error merging LOO data with metadata for", response_var, ":", e$message))
    return(NULL)
  })
  
  # Handle cases where merging fails
  if (is.null(merged_data)) {
    return(overall_summary)
  }
  
  # Use base::union to resolve conflicts
  required_columns <- base::union(names(overall_summary), names(merged_data))
  
  # Add missing columns to overall_summary and merged_data
  overall_summary[setdiff(required_columns, names(overall_summary))] <- NA
  merged_data[setdiff(required_columns, names(merged_data))] <- NA
  
  # Reorder columns to match
  overall_summary <- overall_summary[, required_columns]
  merged_data <- merged_data[, required_columns]
  
  # Combine the data
  combined_data <- rbind(overall_summary, merged_data)
  
  return(combined_data)
}
```

```{r}
forest_data <- do.call(
  rbind, 
  lapply(
    names(results),
    function(response_var) prepare_forest_data(
      results = results,
      response_var = response_var,
      meta_data = splits[[response_var]]$data
    )
  )
)

forest_data |> glimpse()
```
```{r}
# Load necessary library
# Filter out rows with missing `estimate`, `ci_lb`, or `ci_ub`
forest_data_filtered <- forest_data |>
  dplyr::filter(!is.na(estimate), !is.na(ci_lb), !is.na(ci_ub))

# Create the forest plot with response variables in separate panes and moderators on the y-axis
forest_plot <- ggplot(forest_data_filtered, aes(x = estimate, y = study, color = response_var)) +
  geom_point(size = 3) +  # Points for effect sizes
  geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0.2) +  # Error bars for confidence intervals
  facet_grid(rows = vars(study), cols = vars(response_var), scales = "free_y") +  # Separate panes for response variables
  labs(
    title = "Forest Plot of Meta-Analysis Results",
    x = "Effect Size (Estimate)",
    y = "Moderators",
    color = "Response Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

# Print the forest plot
print(forest_plot)

```





```{r}
extract_model_diagnostics <- function(model) {
  list(
    tau2 = if (!is.null(model$tau2)) model$tau2 else NA,  # Between-study variance
    I2 = if (!is.null(model$tau2) && !is.null(model$sigma2)) {
      100 * model$tau2 / (model$tau2 + sum(model$sigma2))
    } else {
      NA
    },  # Proportion of heterogeneity
    AIC = tryCatch(AIC(model), error = function(e) NA),  # Akaike Information Criterion
    BIC = tryCatch(BIC(model), error = function(e) NA),  # Bayesian Information Criterion
    logLik = tryCatch(logLik(model), error = function(e) NA),  # Log-Likelihood
    QM = if (!is.null(model$QM)) model$QM else NA,  # Omnibus test statistic for moderators
    pval_QM = if (!is.null(model$pval.QM)) model$pval.QM else NA  # p-value for the omnibus test
  )
}

# Extract diagnostics for each response variable
diagnostics <- lapply(results, function(res) {
  if (!is.null(res$model)) {
    extract_model_diagnostics(res$model)
  } else {
    NULL
  }
})
```

```{r}
summarize_loo <- function(loo_results) {
  if (is.list(loo_results) && length(loo_results) > 0) {
    # Extract each observation's estimates
    data.frame(
      observation = seq_along(loo_results),
      estimate = sapply(loo_results, function(res) if (!is.null(res[1])) res[1] else NA),
      ci_lb = sapply(loo_results, function(res) if (length(res) >= 2 && !is.null(res[2])) res[2] else NA),
      ci_ub = sapply(loo_results, function(res) if (length(res) >= 3 && !is.null(res[3])) res[3] else NA)
    )
  } else {
    stop("Unexpected structure in loo_results. Check its structure using str().")
  }
}

# Summarize LOO results for each response variable
loo_summaries <- lapply(results, function(res) {
  if (!is.null(res$loo_results)) {
    tryCatch(
      summarize_loo(res$loo_results),
      error = function(e) {
        cat("Error in summarizing LOO results for a response variable:", conditionMessage(e), "\n")
        NULL
      }
    )
  } else {
    NULL
  }
})
```

```{r}
# Print diagnostics for inspection
str(diagnostics)

# Print one LOO summary to verify correctness
if (length(loo_summaries) > 0) {
  str(loo_summaries[[1]])
}
```

Visualize Leave-One-Out (LOO) influence:
  
  ```{r}
loo_data <- loo_data %>%
  mutate(
    se = abs((ci_ub - ci_lb) / (2 * 1.96)),
    ci_lb_new = estimate - 1.96 * se,
    ci_ub_new = estimate + 1.96 * se
  )

loo_data <- loo_data %>%
  filter(ci_lb_new <= estimate, ci_ub_new >= estimate) |> 
  arrange(-estimate)
```
```{r}
plot_loo_influence <- function(loo_data, response_var, top_n = 10) {
  # Select top influential observations
  top_influential <- head(loo_data, n = top_n)
  
  # Add observation labels
  top_influential <- top_influential %>%
    mutate(label = paste("Obs", observation))
  
  # Create the plot
  ggplot(top_influential, aes(x = estimate, y = reorder(label, estimate))) +
    geom_point(size = 3, color = "blue") +
    geom_errorbarh(aes(xmin = ci_lb_new, xmax = ci_ub_new), height = 0.2, color = "blue") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Top", top_n, "Influential Observations: LOO Influence"),
      subtitle = paste("Response Variable:", response_var),
      x = "Effect Size Estimate",
      y = "Observation (Sorted by Estimate)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10, hjust = 1),
      axis.text.x = element_text(size = 10)
    )
}

# Example Usage
plot_loo_influence(loo_data, response_var = "Biodiversity")
```























































































Summarize and Visualize LOO Results:
  
  Create plots of the change in effect size estimates, I², and model fit metrics (AIC, BIC).
Highlight any influential studies that cause large changes when removed.
Integrate Findings:
  
  Compare the LOO results from the full model with those from the individual meta-regression models.
Discuss any discrepancies or notable findings with your collaborators.
























































Key Problems Identified:
  Optimizer Did Not Achieve Convergence: The nlminb optimizer is failing to converge. This is often caused by poor model fit, extreme variance ratios, or insufficient data.
Warnings About Variance Ratios: The warning "Ratio of largest to smallest sampling variance extremely large" suggests that your data may have high variability, which could make the models unstable.
Single-Level Factors in Random Effects: The error "Single-level factor(s) found in 'random' argument" indicates that some random effects have only one level, which causes issues in the meta-regression model.
Insufficient Levels for Moderators: For some moderators, there are not enough unique levels, leading to errors during analysis.
Null Model Outputs: Many of your moderator models are returning NULL, indicating that the model fitting process is failing.


##########################################################################################################################################
ASSESSING THE SPLITS
##########################################################################################################################################



















```{r}
if (nrow(test_subset_splits$Biodiversity$data) != nrow(test_subset_splits$Biodiversity$V_matrix)) {
  stop(sprintf(
    "Mismatch in data and V_matrix dimensions: Data rows = %d, V_matrix rows = %d for id_article '%s'",
    nrow(test_subset_splits$Biodiversity$data), nrow(test_subset_splits$Biodiversity$V_matrix), id_article
  ))
}
```
```{r}
valid_mods <- create_moderator_formula(test_subset_splits$Biodiversity$data, all_moderators)
if (identical(valid_mods, ~1)) {
  cat("Only one level left for moderators after excluding id_article:", id_article, "\n")
}
```
```{r}
if (anyNA(test_subset_splits$Biodiversity$data) || anyNA(test_subset_splits$Biodiversity$V_matrix)) {
  cat("NA values found after excluding id_article:", id_article, "\n")
  return(list(id_article = id_article, estimate = NA, ci.lb = NA, ci.ub = NA))
}
```
```{r}
# Function to check input data for issues in LOO analysis
check_subset_loo_input_data <- function(data, V_matrix, id_article_col, moderators) {
  unique_articles <- unique(data[[id_article_col]])  # Get unique id_article values
  issues <- list()  # To store problematic articles and their issues
  
  # Iterate through each `id_article`
  for (article_id in unique_articles) {
    # Exclude current `id_article`
    reduced_data <- data[data[[id_article_col]] != article_id, ]
    indices <- which(data[[id_article_col]] != article_id)
    reduced_V_matrix <- V_matrix[indices, indices, drop = FALSE]
    
    issue_list <- list()  # To store issues for the current article
    
    # Check 1: Dimension mismatch
    if (nrow(reduced_data) != nrow(reduced_V_matrix)) {
      issue_list$dimension_mismatch <- TRUE
    }
    
    # Check 2: Single-level factors
    single_level_factors <- names(Filter(function(col) length(unique(col)) < 2, reduced_data[moderators]))
    if (length(single_level_factors) > 0) {
      issue_list$single_level_factors <- single_level_factors
    }
    
    # Check 3: Insufficient data
    if (nrow(reduced_data) < 2) {
      issue_list$insufficient_data <- TRUE
    }
    
    # Check 4: Variance-covariance validity
    if (!is.matrix(reduced_V_matrix) || !isSymmetric(reduced_V_matrix) || nrow(reduced_V_matrix) != ncol(reduced_V_matrix)) {
      issue_list$v_matrix_invalid <- TRUE
    }
    
    # Check 5: Redundant predictors
    valid_mods <- create_moderator_formula(reduced_data, moderators)
    if (identical(valid_mods, ~1)) {
      issue_list$redundant_predictors <- TRUE
    }
    
    # Store issues for this article if any
    if (length(issue_list) > 0) {
      issues[[as.character(article_id)]] <- issue_list
    }
  }
  
  return(issues)  # Return the list of issues for all `id_article`
}
```

```{r}
# Test the function with the subset causing issues
subset <- test_subset_splits[["Biodiversity"]]  # Replace with the response variable name
data_subset <- subset$data
V_matrix_subset <- subset$V_matrix

# Check for issues
issues <- check_subset_loo_input_data(
  data = data_subset,
  V_matrix = V_matrix_subset,
  id_article_col = "id_article",
  moderators = all_moderators
)

# Print issues
if (length(issues) > 0) {
  cat("Issues found for the following articles:\n")
  print(issues)
} else {
  cat("No issues found.\n")
}
```




















##########################################################################################################################################
REMOVED FROM 4_SENSITIVITY ANALYSIS
##########################################################################################################################################




```{r}
##########################################################################
# Define the moderator formula
moderator_formula <- as.formula("~ tree_type + crop_type + age_system + season + soil_texture")

##########################################################################
```


```{r}
# Split data and V_matrix by response variable with pre selection
split_by_response_variable <- function(test_data, selected_response_variables = NULL) {
  data <- test_data$data
  V_matrix <- test_data$V_matrix
  
  # If selected_response_variables is provided, filter the data
  if (!is.null(selected_response_variables)) {
    data <- data[data$response_variable %in% selected_response_variables, , drop = FALSE]
  }
  
  # Check if any data remains after filtering
  if (nrow(data) == 0) {
    stop("No data remains after filtering for selected response variables.")
  }
  
  # Split data by response variable
  response_splits <- split(data, data$response_variable)
  
  # Align each subset's V_matrix
  splits <- lapply(response_splits, function(sub_data) {
    indices <- rownames(sub_data)
    V_matrix_subset <- V_matrix[indices, indices, drop = FALSE]
    
    # Validate alignment and return
    validate_data_and_v_matrix(sub_data, V_matrix_subset)
    list(data = sub_data, V_matrix = V_matrix_subset)
  })
  
  return(splits)
}
```



```{r}
# Custom transformation function
apply_transf <- function(results, transf = NULL) {
  if (!is.null(transf) && is.function(transf)) {
    results$estimate <- transf(results$estimate)
    results$ci.lb <- transf(results$ci.lb)
    results$ci.ub <- transf(results$ci.ub)
  }
  return(results)
}


manual_leave1out <- function(model, data, V_matrix, transf = NULL) {
  results <- lapply(seq_len(nrow(data)), function(i) {
    tryCatch({
      # Subset data and V_matrix
      reduced_data <- data[-i, , drop = FALSE]
      reduced_V_matrix <- V_matrix[-i, -i, drop = FALSE]
      
      # Refit the model
      reduced_model <- rma.mv(
        yi = yi,
        V = reduced_V_matrix,
        mods = model$mods,
        random = model$random,
        data = reduced_data,
        method = "REML"
      )
      
      # Extract relevant diagnostics
      res <- list(
        estimate = coef(reduced_model),
        ci.lb = confint(reduced_model)$random["lower"],
        ci.ub = confint(reduced_model)$random["upper"]
      )
      
      # Apply transformation if specified
      apply_transf(res, transf = transf)
    }, error = function(e) {
      cat("Error in iteration", i, ":", conditionMessage(e), "\n")
      NULL
    })
  })
  
  # Return results as a data frame
  do.call(rbind, results)
}
```


```{r}
# Define a directory to save results
# output_model_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
# if (!dir.exists(output_model_dir)) {
#   dir.create(output_model_dir, recursive = TRUE)
# }
```

```{r}
run_loo_sensitivity <- function(data, 
                                V_matrix, 
                                moderator_formula, 
                                random_effects = NULL, 
                                output_dir = NULL, 
                                transf = NULL) {
  
  # Ensure the V_matrix is positive definite
  V_matrix <- make_positive_definite(V_matrix)
  
  # Fit the model
  model <- tryCatch(
    rma.mv(
      yi = yi,
      V = V_matrix,
      mods = moderator_formula,
      random = random_effects,
      data = data,
      method = "ML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    ),
    error = function(e) {
      cat("Error in model fitting:", conditionMessage(e), "\n")
      NULL
    }
  )
  
  # Run leave-one-out diagnostics manually
  loo_results <- if (!is.null(model)) {
    tryCatch(
      manual_leave1out(model = model, data = data, V_matrix = V_matrix, transf = transf),
      error = function(e) {
        cat("Error in manual leave-one-out diagnostics:", conditionMessage(e), "\n")
        NULL
      }
    )
  } else {
    NULL
  }
  
  # Save the model and diagnostics
  if (!is.null(output_dir) && !is.null(model)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    save_path_model <- file.path(output_dir, paste0("model_", make.names(deparse(moderator_formula)), ".rds"))
    save_path_loo <- file.path(output_dir, paste0("loo_results_", make.names(deparse(moderator_formula)), ".rds"))
    tryCatch({
      saveRDS(model, file = save_path_model)
      if (!is.null(loo_results)) {
        saveRDS(loo_results, file = save_path_loo)
      }
    }, error = function(e) {
      cat("Error saving results:", conditionMessage(e), "\n")
    })
  }
  
  # Return the model and diagnostics
  list(model = model, loo_results = loo_results)
}
```

```{r}
run_loo_sensitivity_for_splits <- function(splits, 
                                           moderator_formula, 
                                           random_effects = NULL, 
                                           output_dir = NULL) {
  
  results <- lapply(names(splits), function(response_var) {
    split <- splits[[response_var]]
    
    # Fit model and run diagnostics
    loo_result <- run_loo_sensitivity(
      data = split$data,
      V_matrix = split$V_matrix,
      moderator_formula = moderator_formula,
      random_effects = random_effects
    )
    
    # Add response variable metadata
    loo_result$response_variable <- response_var
    
    # Save each result in a structured way
    response_dir <- file.path(output_dir, response_var)
    if (!dir.exists(response_dir)) {
      dir.create(response_dir, recursive = TRUE)
    }
    
    # Save model and diagnostics separately
    if (!is.null(loo_result$model)) {
      saveRDS(loo_result$model, file = file.path(response_dir, "model.rds"))
    }
    if (!is.null(loo_result$loo_results)) {
      saveRDS(loo_result$loo_results, file = file.path(response_dir, "loo_results.rds"))
    }
    
    # Return the results in memory
    loo_result
  })
  
  # Combine results into a single list
  return(results)
}
```

```{r}
# Set row names for data
rownames(non_imp_dataset) <- as.character(non_imp_dataset$id_obs)

# Ensure V_matrix is a matrix and assign dimension names
V_matrix_non_imp <- as.matrix(V_matrix_non_imp)
dimnames(V_matrix_non_imp) <- list(rownames(non_imp_dataset), rownames(non_imp_dataset))

# Prepare test_data
test_data <- list(data = non_imp_dataset, V_matrix = V_matrix_non_imp)

```

```{r}
# Define the selected response variables
selected_responses <- c("Crop yield", "Soil quality", "Biodiversity")
```

```{r}
# Split the data with selected response variables
splits <- split_by_response_variable(test_data, selected_response_variables = selected_responses)
```

```{r}
splits$Biodiversity$data |> glimpse()
splits$Biodiversity$V_matrix |> glimpse()
```

```{r}
# Function to create a moderator formula excluding variables with only one level
create_moderator_formula <- function(data, moderators) {
  # Identify moderators with more than one level
  valid_moderators <- moderators[sapply(data[, moderators, drop = FALSE], function(x) length(unique(x)) > 1)]
  if (length(valid_moderators) == 0) {
    # If no moderators vary, return an intercept-only model
    return(~ 1)
  }
  # Create the formula
  formula_str <- paste("~", paste(valid_moderators, collapse = " + "))
  as.formula(formula_str)
}
```

```{r}
# Define random effects structure
random_effects <- ~ 1 | id_article/id_obs

# Define all potential moderators
all_moderators <- c("tree_type", "crop_type", "season", "soil_texture")  # Update with actual moderator column names

```

```{r}
# Function to apply a transformation to model results (optional)
apply_transf <- function(results, transf = NULL) {
  # Check if a transformation function is provided and valid
  if (!is.null(transf) && is.function(transf)) {
    # Apply the transformation to the estimate and confidence intervals
    results$estimate <- transf(results$estimate)
    results$ci.lb <- transf(results$ci.lb)
    results$ci.ub <- transf(results$ci.ub)
  }
  return(results)  # Return the transformed (or original) results
}






# Function to perform manual Leave-One-Out (LOO) sensitivity analysis by `id_article`
manual_leave1out <- function(data, V_matrix, id_article_column = "id_article") {
  unique_articles <- unique(data[[id_article_column]])
  
  results <- lapply(unique_articles, function(article_id) {
    reduced_data <- data[data[[id_article_column]] != article_id, , drop = FALSE]
    indices <- which(data[[id_article_column]] != article_id)
    reduced_V_matrix <- V_matrix[indices, indices, drop = FALSE]
    
    if (nrow(reduced_data) == 0 || nrow(reduced_V_matrix) != nrow(reduced_data)) {
      cat("Skipping id_article", article_id, ": Dimension mismatch!\n")
      return(NULL)
    }
    
    tryCatch({
      model <- rma.mv(
        yi = yi,
        V = reduced_V_matrix,
        random = ~1 | id_article,
        data = reduced_data,
        method = "REML"
      )
      list(
        id_article = article_id,
        estimate = coef(model),
        ci.lb = confint(model)$random["lower"],
        ci.ub = confint(model)$random["upper"]
      )
    }, error = function(e) {
      cat("Error excluding id_article", article_id, ":", conditionMessage(e), "\n")
      NULL
    })
  })
  
  do.call(rbind, lapply(results, as.data.frame))
}
```

```{r}
# Length of dataset/v_matrix
n_no <- 100

# Generic function to create test subsets
create_test_subset <- function(dataset, n = n_no) {
  # Extract the data and V_matrix
  data <- dataset$data
  V_matrix <- dataset$V_matrix
  
  # Ensure the data and V_matrix are properly aligned
  if (nrow(data) != nrow(V_matrix)) {
    stop("Data and V_matrix row counts do not match.")
  }
  
  # Randomly sample n rows for the test subset
  sampled_indices <- sample(seq_len(nrow(data)), size = n_no)
  
  # Subset the data and V_matrix
  test_data <- data[sampled_indices, , drop = FALSE]
  test_V_matrix <- V_matrix[sampled_indices, sampled_indices, drop = FALSE]
  
  # Return the test subset
  list(data = test_data, V_matrix = test_V_matrix)
}

####################################################################################################
# Apply the function to all subsets in 'splits'
subset_splits <- splits  # Ensure we are working on the original splits structure
test_subset_splits <- lapply(subset_splits, create_test_subset, n = n_no)

####################################################################################################
# Save the test subsets for future reference
test_output_dir <- here::here("DATA", "OUTPUT_FROM_R", "TEST_SUBSETS")
if (!dir.exists(test_output_dir)) {
  dir.create(test_output_dir, recursive = TRUE)
}

# Save the test subsets
saveRDS(test_subset_splits, file = file.path(test_output_dir, "test_subset_splits.rds"))
cat("Test subsets saved to:", file.path(test_output_dir, "test_subset_splits.rds"), "\n")

test_subset_splits |> str()
test_subset_splits$Biodiversity$data |> glimpse()
test_subset_splits$Biodiversity$data |> str()
```


```{r}
if (nrow(test_subset_splits$Biodiversity$data) != nrow(test_subset_splits$Biodiversity$V_matrix)) {
  cat("Skipping id_article", article_id, ": Dimension mismatch!\n")
  cat("Data rows:", nrow(reduced_data), "V_matrix rows:", nrow(reduced_V_matrix), "\n")
  return(NULL)
}
```








```{r}
# Load the saved results
results_path <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R", "meta_model_results_with_loo_test_subsets.rds")
results <- readRDS(results_path)
results |> glimpse() |> head()
```





























































**OBS! This takes long, as its running on the full datasets**
  ```{r, eval=FALSE}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################
# Run Leave-One-Out (LOO) Sensitivity Analysis on dataset subsets for each response variable [FULL DATASETS]

# Initialize a list to store the results
results <- list()

# Enable progress reporting
with_progress({
  # Create a progressor object
  p <- progressr::progressor(along = names(splits))
  
  # Using the full splits
  for (response_var in names(splits)) {
    # Update the progress bar
    p(message = sprintf("Processing %s", response_var))
    
    # Extract the test subset and its corresponding variance-covariance matrix
    subset <- splits[[response_var]]
    data_subset <- subset$data
    V_matrix_subset <- subset$V_matrix
    
    cat("Processing response variable:", response_var, "\n")
    
    # Dynamically create a moderator formula for the current test subset
    moderator_formula <- create_moderator_formula(data_subset, all_moderators)
    cat("Using moderator formula:", deparse(moderator_formula), "\n")
    
    # Fit the random-effects meta-analysis model_full
    model_full <- tryCatch(
      rma.mv(
        yi = yi,  # Effect sizes
        V = V_matrix_subset,  # Variance-covariance matrix
        mods = if (identical(moderator_formula, ~1)) NULL else moderator_formula,
        random = random_effects,  # Random-effects structure
        data = data_subset,  # Test subset data
        method = "REML"  # Restricted maximum likelihood estimation
      ),
      error = function(e) {
        # Log an error message if the model_full fitting fails
        cat("Error in model_full fitting for", response_var, ":", conditionMessage(e), "\n")
        NULL
      }
    )
    
    # If the model_full fitting was successful, perform LOO sensitivity analysis
    if (!is.null(model_full)) {
      loo_results <- manual_leave1out(
        model_full = model_full,
        data = data_subset,
        V_matrix = V_matrix_subset,
        mods = moderator_formula,
        random = random_effects,
        transf = NULL  # Replace with a transformation function if needed
      )
      
      # Store the model_full and LOO results
      results[[response_var]] <- list(
        model_full = model_full,
        loo_results = loo_results
      )
    }
  }
})


# Save the results to an RDS file
output_model_full_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
if (!dir.exists(output_model_full_dir)) {
  dir.create(output_model_full_dir, recursive = TRUE)
}
results_path <- file.path(output_model_full_dir, "meta_model_full_results_with_loo.rds")
saveRDS(results, file = results_path)

# Confirm that the results have been saved
cat("Results saved to:", results_path, "\n")
##################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("Total time taken:", time.taken, "\n")
##############################################################
# Last go: (21/11-24)
```


Prepare and Process LOO Data for Plotting
```{r}
prepare_loo_data <- function(loo_data) {
  loo_data %>%
    mutate(
      se = abs((ci_ub - ci_lb) / (2 * 1.96)),  # Calculate standard error
      ci_lb_new = estimate - 1.96 * se,       # Recompute lower confidence bound
      ci_ub_new = estimate + 1.96 * se        # Recompute upper confidence bound
    ) %>%
    filter(ci_lb_new <= estimate, ci_ub_new >= estimate) %>%  # Filter valid intervals
    arrange(-estimate)  # Sort by descending estimate
}
```

Visualize LOO Influence
```{r}
plot_loo_influence <- function(loo_data, response_var, top_n = 10) {
  top_influential <- head(loo_data, n = top_n) %>%
    mutate(label = paste("Obs", observation))  # Add labels
  
  ggplot(top_influential, aes(x = estimate, y = reorder(label, estimate))) +
    geom_point(size = 3, color = "blue") +
    geom_errorbarh(aes(xmin = ci_lb_new, xmax = ci_ub_new), height = 0.2, color = "blue") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Top", top_n, "Influential Observations: LOO Influence"),
      subtitle = paste("Response Variable:", response_var),
      x = "Effect Size Estimate",
      y = "Observation (Sorted by Estimate)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10, hjust = 1),
      axis.text.x = element_text(size = 10)
    )
}
```

Apply Viz Workflow to All Response Variables
```{r}
# Loop through all response variables
for (i in seq_along(loo_summaries)) {
  loo_data <- loo_summaries[[i]]
  
  if (!is.null(loo_data)) {
    loo_data <- prepare_loo_data(loo_data)  # Process LOO data
    response_var <- names(loo_summaries)[i]  # Get response variable name
    
    # Plot and display
    print(plot_loo_influence(loo_data, response_var = response_var))
  }
}

loo_data
```
```{r}
# Add confidence intervals to the data
all_loo_results_for_plotting <- combined_loo_res %>%
  mutate(
    lower_ci = estimate - 1.96 * se,
    upper_ci = estimate + 1.96 * se
  )

# Create the forest plot with study_id on the y-axis
all_loo_results_for_plotting %>%
  ggplot(aes(y = as.factor(id_article), x = estimate, color = response_variable)) +
  geom_point(size = 2) +  # Effect size points
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, alpha = 0.7) +  # Horizontal CI bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line at 0
  facet_grid(dataset ~ response_variable, scales = "free_y") +  # Split by dataset and response variable
  labs(
    y = "Excluded Study (ID)",
    x = "Estimated Effect Size",
    title = "Leave-One-Out Sensitivity Analysis",
    subtitle = "Point estimates with 95% confidence intervals",
    color = "Response Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    strip.text = element_text(size = 12)  # Larger facet labels
  )
```



Forest Plots
```{r}

```

Heterogeneity Metrics Across Response Variables

```{r}
# str(results[["Biodiversity"]])
test_subset_splits
# Calculate Heterogeneity Metrics
heterogeneity_metrics <- lapply(names(results), function(response_var) {
  # Extract the relevant data
  if (!is.null(results[[response_var]]$data)) {
    res <- results[[response_var]]$data
    
    # Check for `yi` and `vi`
    if (!all(c("yi", "vi") %in% names(res))) {
      stop(paste("Missing `yi` or `vi` in response variable:", response_var))
    }
    
    # Fit the meta-analysis model
    model <- rma.uni(yi = res$yi, vi = res$vi, method = "REML")
    
    # Return heterogeneity metrics
    tibble(
      response_var = response_var,
      I2 = model$I2,
      Q = model$QE,
      tau2 = model$tau2,
      Q_pval = model$QEp
    )
  } else {
    warning(paste("No data available for response variable:", response_var))
    return(NULL)
  }
}) |> bind_rows()

# Display the heterogeneity metrics
heterogeneity_metrics

```
```{r}
# Check structure of `data` for each response variable
lapply(names(results), function(response_var) {
  list(
    response_var = response_var,
    structure = if (!is.null(results[[response_var]]$data)) {
      str(results[[response_var]]$data, max.level = 1)
    } else {
      "No data"
    }
  )
})
```

Funnel Plot
```{r}
# str(test_subset_splits)

# Example for one response variable
response_var <- "Biodiversity"  # Replace with your target response variable
data <- test_subset_splits[[response_var]]$data

# Ensure the data contains yi and vi
if (!is.null(data) && all(c("yi", "vi") %in% names(data))) {
  funnel_data <- data %>%
    mutate(se = sqrt(vi))
} else {
  stop("Data is missing `yi` or `vi` for the selected response variable.")
}
```

```{r}
funnel_plot <- ggplot(funnel_data, aes(x = yi, y = 1 / se)) +
  geom_point(alpha = 0.6, size = 3) +  # Points for studies
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Reference line
  labs(
    title = paste("Funnel Plot for", response_var),
    x = "Effect Size (yi)",
    y = "Precision (1/SE)"
  ) +
  theme_minimal()

funnel_plot <- funnel_plot +
  geom_abline(slope = c(-1.96, 1.96), intercept = 0, linetype = "dotted", color = "red")


funnel_plot
```


```{r}
funnel_plots <- lapply(names(test_subset_splits), function(response_var) {
  data <- test_subset_splits[[response_var]]$data
  
  if (!is.null(data) && all(c("yi", "vi") %in% names(data))) {
    funnel_data <- data %>%
      mutate(se = sqrt(vi))
    
    ggplot(funnel_data, aes(x = yi, y = 1 / se)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = paste("Funnel Plot for", response_var),
        x = "Effect Size (yi)",
        y = "Precision (1/SE)"
      ) +
      theme_minimal()
  } else {
    NULL  # Skip if data is missing
  }
})

# Print all funnel plots
lapply(funnel_plots, print)

```
```{r}
# Example for "Biodiversity" response variable
response_var <- "Biodiversity"
data <- test_subset_splits[[response_var]]$data

if (!is.null(data) && all(c("yi", "vi") %in% names(data))) {
  res <- rma(yi, vi, data = data, measure = "GEN", method = "EE")
} else {
  stop("Data is missing `yi` or `vi` for the selected response variable.")
}
```

```{r}
# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Standard Error on the y-axis
funnel(res, main = "Standard Error")

# Sampling Variance on the y-axis
funnel(res, yaxis = "vi", main = "Sampling Variance")

# Inverse Standard Error on the y-axis
funnel(res, yaxis = "seinv", main = "Inverse Standard Error")

# Inverse Sampling Variance on the y-axis
funnel(res, yaxis = "vinv", main = "Inverse Sampling Variance")

```

```{r}
results <- lapply(names(test_subset_splits), function(response_var) {
  data <- test_subset_splits[[response_var]]$data
  
  if (!is.null(data) && all(c("yi", "vi") %in% names(data))) {
    res <- rma(yi, vi, data = data, measure = "GEN", method = "EE")
    
    # Set up 2x2 plotting area for each response variable
    par(mfrow = c(2, 2))
    funnel(res, main = paste("Standard Error -", response_var))
    funnel(res, yaxis = "vi", main = paste("Sampling Variance -", response_var))
    funnel(res, yaxis = "seinv", main = paste("Inverse Standard Error -", response_var))
    funnel(res, yaxis = "vinv", main = paste("Inverse Sampling Variance -", response_var))
  } else {
    message(paste("No valid data for response variable:", response_var))
  }
})
```




```{r}
final_loo_results %>%
  ggplot(aes(x = estimate, y = 1 / se)) +  # Precision (1/se) on y-axis
  geom_point(size = 3, aes(color = response_variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~response_variable, scales = "free", ncol = 1) +
  labs(
    x = "Effect Size Estimate",
    y = "Precision (1/SE)",
    title = "Funnel Plot for LOO Analysis",
    subtitle = "Grouped by Response Variable"
  ) +
  theme_minimal(base_size = 14)
```

```{r}
# Loop through response variables and generate funnel plots
results <- lapply(names(test_subset_splits), function(response_var) {
  data_split <- test_subset_splits[[response_var]]
  
  # Ensure data and V_matrix are valid
  if (!is.null(data_split$data) && all(c("yi", "vi") %in% names(data_split$data))) {
    # Fit a random-effects model
    res <- rma(
      yi = yi,
      vi = vi,
      data = data_split$data,
      method = "REML"  # Replace "EE" if random-effects model fits better
    )
    
    # Create a 2x2 plotting area for each response variable
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Adjust margins for clarity
    funnel(res, main = paste("Standard Error -", response_var))
    funnel(res, yaxis = "vi", main = paste("Sampling Variance -", response_var))
    funnel(res, yaxis = "seinv", main = paste("Inverse Standard Error -", response_var))
    funnel(res, yaxis = "vinv", main = paste("Inverse Sampling Variance -", response_var))
    
    return(res)  # Save model results for further evaluation
  } else {
    message(paste("No valid data for response variable:", response_var))
    return(NULL)
  }
})

# Reset the plotting area to 1x1 after the loop
par(mfrow = c(1, 1))
```







```{r}
# Subset data for Biodiversity
response_var <- "Biodiversity"
data <- test_subset_splits[[response_var]]$data

# Fit a random-effects model
# Fit the multivariate random-effects model
model_res <- rma.mv(
  yi = yi,
  vi = vi,
  #V = V_matrix,
  mods = moderator_formula,
  random = list(
    # ~ 1 | id_article,
    #     ~ 1 | id_article/response_variable,
    ~ 1 | exp_id),
  data = data,
  method = "ML",
  control = list(
    optimizer = "optim",
    optim.method = "BFGS",
    iter.max = 1000,
    rel.tol = 1e-8
  )
)

```


Influence Diagnostics
```{r}
# Print heterogeneity metrics
cat("Heterogeneity Metrics:\n")
cat("Tau²:", res$tau2, "\n")
cat("I²:", res$I2, "\n")
cat("Q-test (QE):", res$QE, "\n")
cat("Q-test p-value (QEp):", res$QEp, "\n")

# Influence diagnostics
inf <- influence(res)

# Print diagnostics
print(inf)

# Plot diagnostics
par(mfrow = c(8, 1))
plot(inf)
```



Between-Study Heterogeneity
```{r}
# Ensure clean data
data <- data[!is.na(data$yi) & !is.na(data$vi) & data$vi > 0, ]

# Fit a random-effects model
res <- rma(yi, vi, data = data, method = "REML")

# Extract heterogeneity metrics
heterogeneity <- list(
  Q = res$QE,
  Q_df = res$k - res$p,
  Q_pval = res$QEp,
  I2 = res$I2,
  tau2 = res$tau2
)

print(heterogeneity)

```
```{r}
# Convert heterogeneity metrics to a data frame
het_df <- data.frame(
  Metric = c("Q", "I2 (%)", "Tau2"),
  Value = c(heterogeneity$Q, heterogeneity$I2, heterogeneity$tau2)
)

# Plot
ggplot(het_df, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Between-Study Heterogeneity Metrics",
    x = "Metric",
    y = "Value"
  ) +
  theme_minimal()

```
```{r}
# Plot Q-test result
barplot(
  height = heterogeneity$Q,
  names.arg = "Q-Test",
  col = "blue",
  ylim = c(0, max(heterogeneity$Q, heterogeneity$Q_df) * 1.2),
  main = "Q-Test for Heterogeneity",
  ylab = "Q Value"
)
abline(h = heterogeneity$Q_df, col = "red", lty = 2)  # Add critical value
legend("topright", legend = c("Q Value", "Degrees of Freedom"),
       fill = c("blue", "red"), bty = "n")

```

```{r}
# Example of preparing a GOSH object
res.gosh <- gosh(res)
```

```{r}
# Perform GOSH diagnostics
res.gosh.diag <- gosh.diagnostics(
  res.gosh,
  km.params = list(centers = 2), # k-means clustering with 2 centers
  db.params = list(
    eps = 0.08, # Epsilon value for DBSCAN
    MinPts = 50 # Minimum points for a cluster
  )
)

# View diagnostics
print(res.gosh.diag)

```

```{r}
# Explore clusters identified by k-means and DBSCAN
str(res.gosh.diag)

# Summary of the diagnostics
summary(res.gosh.diag)

```

```{r}
# Visualize clusters (if available)
plot(res.gosh.diag, type = "clusters")

# Plot goodness-of-fit metrics
plot(res.gosh.diag, type = "diagnostics")

```






```{r}
# Example: Adding a placeholder RiskOfBias column
meta_data$RiskOfBias <- sample(c("Low", "Moderate", "High"), nrow(meta_data), replace = TRUE)
```

```{r}
# Updated function to compute key metrics for a given dataset and V_matrix
compute_model_metrics <- function(data, V_matrix, dataset_name) {
  tryCatch({
    # Define the moderators (if applicable)
    moderators <- c("tree_type", "crop_type", "age_system", "season", 
                    "soil_texture", "no_tree_per_m", "tree_height", "alley_width")
    moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
    
    # Prepare the data
    data <- data %>%
      mutate(across(all_of(moderators), as.factor)) %>%
      as.data.frame()
    
    # Fit the random-effects model
    model <- rma.mv(
      yi = yi,
      V = V_matrix,
      mods = moderator_formula,
      random = list(
        ~ 1 | id_article,
        ~ 1 | id_article/response_variable,
        ~ 1 | exp_id
      ),
      data = data,
      method = "REML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    )
    
    # Extract key metrics
    k.all <- model$k.all
    tau2 <- sum(model$sigma2)  # Between-study variance
    sigma2 <- mean(data$vi)    # Within-study variance
    I2 <- (tau2 / (tau2 + sigma2)) * 100  # Heterogeneity proportion
    QM <- model$QM             # Test statistic for moderators
    QMp <- model$QMp           # p-value for moderators
    aic <- AIC(model)          # Akaike Information Criterion
    bic <- BIC(model)          # Bayesian Information Criterion
    logLik_val <- as.numeric(logLik(model))  # Log-Likelihood
    
    tibble(
      dataset = dataset_name,
      k.all = k.all,
      tau2 = tau2,
      I2 = I2,
      QM = QM,
      QMp = QMp,
      AIC = aic,
      BIC = bic,
      logLik = logLik_val
    )
  }, error = function(e) {
    # Handle errors by returning NA values
    tibble(
      dataset = dataset_name,
      k.all = NA, tau2 = NA, I2 = NA, QM = NA, QMp = NA,
      AIC = NA, BIC = NA, logLik = NA
    )
  })
}
```

```{r}
datasets <- list(
  non_imp_dataset = non_imp_dataset,
  imp_dataset = imp_dataset,
  non_imp_dataset_imputed = non_imp_dataset_imputed,
  imp_dataset_imputed = imp_dataset_imputed
)

V_matrices <- lapply(names(datasets), function(dataset_name) {
  calculate_v_matrix(datasets[[dataset_name]], correlation = 0.5)
})
names(V_matrices) <- names(datasets)
```


```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

# Loop through datasets and compute metrics
model_results <- lapply(names(datasets), function(dataset_name) {
  cat("Processing dataset:", dataset_name, "\n")
  compute_model_metrics(
    data = datasets[[dataset_name]], 
    V_matrix = V_matrices[[dataset_name]], 
    dataset_name = dataset_name
  )
})

# Combine results into a single data frame
model_results_df <- bind_rows(model_results)

# View the results
print(model_results_df)


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################

# Time difference of 1.29801 mins
```
```{r}
# Compute whole-model metrics
model_metrics <- tibble(
  tau2 = full_model$tau2,  # Between-study variance
  I2 = if (!is.null(full_model$tau2) && !is.null(full_model$sigma2)) {
    100 * full_model$tau2 / (full_model$tau2 + sum(full_model$sigma2))
  } else {
    NA
  },  # Proportion of heterogeneity
  QM = full_model$QM,  # Omnibus test for moderators
  pval_QM = full_model$pval.QM,  # p-value for QM
  QE = full_model$QE,  # Cochran's Q
  pval_QE = full_model$QEp  # p-value for heterogeneity
)

# Print the metrics
model_metrics
```


```{r}
# Prepare data for the forest plot
forest_plot_data <- combined_loo_res %>%
  mutate(
    slab = paste0("Study ", id_article),  # Study labels
    ci.lb = estimate - 1.96 * se,        # Lower bound of 95% CI
    ci.ub = estimate + 1.96 * se         # Upper bound of 95% CI
  )

```
```{r}
res <- rma(yi = estimate, vi = se^2, data = forest_plot_data, method = "DL")
```

```{r}
# Set up the margins for the plot
par(mar = c(4, 4, 2, 2))  # Adjust margins as needed

# Generate the forest plot
sav <- forest(
  x = forest_plot_data$estimate,  # Effect sizes
  sei = forest_plot_data$se,     # Standard errors
  slab = forest_plot_data$slab,  # Study labels
  xlim = c(-1, 2),               # X-axis limits (adjust based on your data)
  alim = c(-0.5, 1.5),           # Area for effect sizes
  at = seq(-1, 2, by = 0.5),     # X-axis tick marks
  refline = 0,                   # Reference line at 0
  transf = exp,                  # Transform effect sizes (e.g., for ORs)
  digits = 2,                    # Number of decimal places
  cex = 0.8                      # Text size
)

# Add annotations for summary estimate (optional)
addpoly(x = res$b, ci.lb = res$ci.lb, ci.ub = res$ci.ub, row = -1, cex = 0.8, mlab = "Overall")

# Add custom annotations (heterogeneity stats, etc.)
text(-1, -2, pos = 4, bquote(paste("Heterogeneity: ",
                                   "Tau"^2, " = ", .(round(res$tau2, 3)), "; ",
                                   "I"^2, " = ", .(round(res$I2, 1)), "%")))

```
```{r}
# Loop through response variables
unique(forest_plot_data$response_variable) %>%
  lapply(function(resp) {
    data_subset <- subset(forest_plot_data, response_variable == resp)
    res <- rma(yi = estimate, vi = se^2, data = data_subset, method = "DL")
    
    forest(
      x = data_subset$estimate,
      sei = data_subset$se,
      slab = data_subset$slab,
      xlim = c(-1, 2),
      alim = c(-0.5, 1.5),
      at = seq(-1, 2, by = 0.5),
      refline = 0,
      transf = exp,
      digits = 2,
      cex = 0.8,
      main = paste("Forest Plot:", resp)  # Add title for each response variable
    )
    addpoly(res$b, ci.lb = res$ci.lb, ci.ub = res$ci.ub, row = -1, cex = 0.8, mlab = "Overall")
  })

```


```{r}
# Generic function to create forest plots
create_forest_plots <- function(data, moderators, response_var_col, yi_col, vi_col) {
  
  # Validate inputs
  if (!all(c(response_var_col, yi_col, vi_col) %in% names(data))) {
    stop("The specified column names do not exist in the data.")
  }
  if (!all(moderators %in% names(data))) {
    stop("Some specified moderators are not in the data.")
  }
  
  # Add confidence intervals to the data
  forest_plot_data <- data %>%
    mutate(
      lower_ci = !!sym(yi_col) - 1.96 * sqrt(!!sym(vi_col)),
      upper_ci = !!sym(yi_col) + 1.96 * sqrt(!!sym(vi_col))
    ) %>%
    select(
      all_of(response_var_col),
      all_of(moderators),
      !!sym(yi_col),
      lower_ci,
      upper_ci
    ) %>%
    filter(!is.na(!!sym(yi_col)), !is.na(lower_ci), !is.na(upper_ci)) %>%
    pivot_longer(cols = all_of(moderators), names_to = "moderator", values_to = "moderator_value") %>%
    filter(!is.na(moderator_value))
  
  # Create plots for each combination of response variable and moderator
  forest_plots <- forest_plot_data %>%
    group_by(across(all_of(response_var_col)), moderator) %>%
    group_split() %>%
    map(~ {
      response_variable <- unique(.x[[response_var_col]])
      moderator <- unique(.x$moderator)
      
      ggplot(.x, aes(
        x = !!sym(yi_col),
        y = moderator_value,
        color = !!sym(response_var_col)
      )) +
        geom_point(size = 3) +  # Effect size
        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # Confidence intervals
        facet_grid(moderator ~ ., scales = "free_x", switch = "y") +  # Stack facets vertically
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line
        labs(
          title = paste("Forest Plot: ", moderator, "vs", response_variable),
          x = "Effect Size (yi)",
          y = moderator
        ) +
        theme_minimal() +
        theme(
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.placement = "outside",
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          legend.position = "bottom",
          panel.spacing = unit(1, "lines")
        )
    })
  
  # Return the list of ggplot objects
  return(forest_plots)
}
```

```{r}
# Specify the required arguments
forest_plots <- create_forest_plots(
  data = imp_dataset_imputed,
  moderators = c("crop_type", "tree_type", "age_system", "season", "soil_texture"),
  response_var_col = "response_variable",
  yi_col = "yi",
  vi_col = "vi"
)

# Display plots (example for the first plot)
forest_plots[[20]]

# Save all plots (optional)
# map2(forest_plots, seq_along(forest_plots), ~ ggsave(
#   filename = paste0("forest_plot_", .y, ".png"),
#   plot = .x,
#   width = 10,
#   height = 8
# ))

```



```{r}
# Prepare and clean the data for effect sizes
forest_plot_data_clean <- forest_plot_data %>%
  rename(
    estimate = yi,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  )

# Calculate densities for each response variable
density_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarise(
    density_x = list(density(estimate, na.rm = TRUE)$x),  # Extract density x-coordinates
    density_y = list(density(estimate, na.rm = TRUE)$y),  # Extract density y-coordinates
    .groups = "drop"
  ) %>%
  unnest(cols = c(density_x, density_y))  # Flatten the lists into columns

# Add ranking and summary statistics
aggregated_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarize(
    overall_effect = mean(estimate, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    num_observations = n(),
    num_studies = n_distinct(id_article),
    size_category = case_when(
      num_studies <= 2 ~ "1-2",
      num_studies <= 4 ~ "3-4",
      num_studies > 4 ~ "5+"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    size_category = factor(size_category, levels = c("1-2", "3-4", "5+")),
    response_label = paste0(response_variable, " (", num_studies, " studies)"),
    mean_ci_label = paste0(
      sprintf("%.2f", overall_effect), " [",
      sprintf("%.2f", lower_ci), ", ", sprintf("%.2f", upper_ci), "]"
    ),
    response_rank = rank(-overall_effect)  # Negative for descending order
  )

# Merge the rank into the density data
density_data <- density_data %>%
  left_join(aggregated_data %>% select(response_variable, response_label, response_rank), by = "response_variable")

# Calculate overall effect size for all response variables
overall_effect <- aggregated_data %>%
  summarise(
    response_variable = "Overall",
    overall_effect = mean(overall_effect, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    num_observations = n_distinct(forest_plot_data_clean$id_obs),
    num_studies = n_distinct(forest_plot_data_clean$id_article),
    size_category = NA,
    mean_ci_label = paste0(
      sprintf("%.2f", overall_effect), " [",
      sprintf("%.2f", lower_ci), ", ", sprintf("%.2f", upper_ci), "]"
    )
  )

# Combine aggregated data with overall effect size
plot_data <- bind_rows(aggregated_data, overall_effect) %>%
  mutate(
    response_label = ifelse(
      response_variable == "Overall",
      paste0("Overall Effect Size (", num_studies, " studies)"),
      paste0(response_variable, " (", num_studies, " studies)")
    ),
    response_rank = ifelse(response_variable == "Overall", Inf, response_rank)  # Place "Overall" at the bottom
  )

# Adjust density data for plotting (exclude "Overall")
density_data <- density_data %>%
  filter(response_variable != "Overall")

# Adjust ranks to explicitly place "Overall" at the bottom
# Explicitly define the levels for response_label with "Overall" at the bottom
plot_data <- plot_data %>%
  arrange(response_variable != "Overall", desc(response_rank)) %>%
  mutate(
    response_label = factor(response_label, levels = unique(response_label))
  )


ggplot() +
  # Density ridges for individual response variables
  geom_ridgeline(
    data = density_data,
    aes(x = density_x, y = response_label, height = density_y, fill = response_variable),
    alpha = 0.3,
    scale = 0.05,
    color = NA
  ) +
  # Points for individual response variables
  geom_point(
    data = plot_data %>% filter(response_variable != "Overall"),
    aes(x = overall_effect, y = response_label, size = size_category),
    color = "black"
  ) +
  # Diamond for overall effect size
  geom_point(
    data = plot_data %>% filter(response_variable == "Overall"),
    aes(x = overall_effect, y = response_label),
    shape = 18,  # Diamond shape
    size = 5,
    color = "black"
  ) +
  # Error bars for all data
  geom_errorbarh(
    data = plot_data,
    aes(xmin = lower_ci, xmax = upper_ci, y = response_label),
    height = 0.2,
    color = "darkgray"
  ) +
  # Text annotations for mean and confidence intervals
  geom_text(
    data = plot_data,
    aes(x = 0.55, y = response_label, label = mean_ci_label),
    size = 3.5,
    hjust = 0,
    fontface = "italic",
    color = "black"
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # Customize scales
  scale_fill_manual(values = custom_colors, guide = "none") +
  scale_size_manual(
    values = c("1-2" = 3, "3-4" = 5, "5+" = 7),
    name = "Number of Studies"
  ) +
  # Adjust x-axis
  scale_x_continuous(limits = c(-0.25, 0.6), breaks = seq(-0.25, 0.5, by = 0.25)) +
  # Labels and titles
  labs(
    title = "Forest Plot with Adjusted Density and Study Details",
    x = "Effect Size (yi)",
    y = NULL
  ) +
  # Themes
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

```


```{r}
# Cleaning Data
clean_forest_data <- function(data, yi_col, vi_col, other_cols) {
  data %>%
    mutate(
      lower_ci = !!sym(yi_col) - 1.96 * sqrt(!!sym(vi_col)),
      upper_ci = !!sym(yi_col) + 1.96 * sqrt(!!sym(vi_col))
    ) %>%
    select(all_of(c(other_cols, yi_col, "lower_ci", "upper_ci"))) %>%
    filter(!is.na(!!sym(yi_col)), !is.na(lower_ci), !is.na(upper_ci))
}

forest_data_clean <- clean_forest_data(
  data = imp_dataset_imputed,
  yi_col = "yi",
  vi_col = "vi",
  other_cols = c("id_article", "id_obs", "response_variable", "crop_type", "tree_type")
)

```

```{r}
# Aggregating Data
aggregate_forest_data <- function(data, yi_col, ci_cols, group_col) {
  data %>%
    group_by(!!sym(group_col)) %>%
    summarize(
      overall_effect = mean(!!sym(yi_col), na.rm = TRUE),
      lower_ci = mean(!!sym(ci_cols[1]), na.rm = TRUE),
      upper_ci = mean(!!sym(ci_cols[2]), na.rm = TRUE),
      num_observations = n(),
      num_studies = n_distinct(id_article),
      size_category = case_when(
        num_studies <= 2 ~ "1-2",
        num_studies <= 4 ~ "3-4",
        num_studies > 4 ~ "5+"
      ),
      .groups = "drop"
    ) %>%
    mutate(
      size_category = factor(size_category, levels = c("1-2", "3-4", "5+")),
      response_label = paste0(!!sym(group_col), " (", num_studies, " studies)"),
      mean_ci_label = paste0(
        sprintf("%.2f", overall_effect), " [",
        sprintf("%.2f", lower_ci), ", ", sprintf("%.2f", upper_ci), "]"
      ),
      response_rank = rank(-overall_effect)  # Add rank for ordering
    )
}

aggregated_data <- aggregate_forest_data(
  data = forest_data_clean,
  yi_col = "yi",
  ci_cols = c("lower_ci", "upper_ci"),
  group_col = "response_variable"
)
```
```{r}
# Computing Densities
compute_densities <- function(data, yi_col, group_col, aggregated_data) {
  data %>%
    group_by(!!sym(group_col)) %>%
    summarise(
      density_x = list(density(!!sym(yi_col), na.rm = TRUE)$x),
      density_y = list(density(!!sym(yi_col), na.rm = TRUE)$y),
      .groups = "drop"
    ) %>%
    unnest(cols = c(density_x, density_y)) %>%
    left_join(
      aggregated_data %>% select(response_variable, response_label, response_rank),
      by = "response_variable"
    )
}

density_data <- compute_densities(
  data = forest_data_clean,
  yi_col = "yi",
  group_col = "response_variable",
  aggregated_data = aggregated_data
)
```

```{r}
# Adding Overall Effect Size
add_overall_effect <- function(agg_data, original_data) {
  agg_data %>%
    summarise(
      response_variable = "Overall",
      overall_effect = mean(overall_effect, na.rm = TRUE),
      lower_ci = mean(lower_ci, na.rm = TRUE),
      upper_ci = mean(upper_ci, na.rm = TRUE),
      num_observations = sum(num_observations, na.rm = TRUE),
      num_studies = sum(num_studies, na.rm = TRUE),
      size_category = NA,
      mean_ci_label = paste0(
        sprintf("%.2f", mean(overall_effect, na.rm = TRUE)), " [",
        sprintf("%.2f", mean(lower_ci, na.rm = TRUE)), ", ",
        sprintf("%.2f", mean(upper_ci, na.rm = TRUE)), "]"
      )
    ) %>%
    mutate(
      response_label = "Overall Effect Size (36 studies)",
      response_rank = 999  # Explicitly assign 999 to place at the bottom
    )
}

overall_effect <- add_overall_effect(
  agg_data = aggregated_data,
  original_data = forest_data_clean
)

overall_effect |> glimpse()
```

```{r}
# Combine aggregated data with overall effect size
# Ensure response_label exists in both aggregated_data and overall_effect
aggregated_data <- aggregated_data %>%
  mutate(
    response_label = paste0(response_variable, " (", num_studies, " studies)")
  )

overall_effect <- overall_effect %>%
  mutate(
    response_label = "Overall Effect Size (36 studies)"
  )

# Combine aggregated data with overall effect size
plot_data <- bind_rows(aggregated_data, overall_effect) %>%
  # Define factor levels explicitly to place "Overall" at the bottom
  mutate(
    response_label = factor(
      response_label,
      levels = c(
        setdiff(unique(aggregated_data$response_label), "Overall Effect Size (36 studies)"),
        "Overall Effect Size (36 studies)"
      )
    )
  ) %>%
  arrange(desc(response_rank))  # Arrange by descending rank

```
```{r}
# Ensure response_label is added to density_data during the join
density_data <- density_data %>%
  left_join(
    aggregated_data %>% select(response_variable, response_label, response_rank),
    by = "response_variable"
  ) %>%
  # Exclude the "Overall" row
  filter(response_variable != "Overall")

# Combine aggregated data with overall effect size
plot_data <- bind_rows(aggregated_data, overall_effect) %>%
  # Add response_label for the overall effect
  mutate(
    response_label = ifelse(
      response_variable == "Overall",
      "Overall Effect Size (36 studies)",
      response_label
    )
  ) %>%
  # Ensure factor levels place "Overall" at the bottom
  mutate(
    response_label = factor(
      response_label,
      levels = c(
        setdiff(unique(aggregated_data$response_label), "Overall Effect Size (36 studies)"),
        "Overall Effect Size (36 studies)"
      )
    )
  ) %>%
  arrange(desc(response_rank))  # Arrange by descending rank

```

```{r}
# Arrange `plot_data` to place "Overall" at the bottom
plot_data <- plot_data %>%
  arrange(ifelse(response_variable == "Overall", Inf, response_rank))

# Add `response_rank` to `density_data` by joining with `plot_data`
density_data <- density_data %>%
  left_join(
    plot_data %>% select(response_variable, response_rank),
    by = "response_variable"
  ) %>%
  # Exclude "Overall" and arrange by rank
  filter(response_variable != "Overall") %>%
  arrange(desc(response_rank))


# plot_data |> glimpse()
# density_data |> glimpse()
```


```{r}
# Plotting
# Prepare and clean the data for effect sizes
forest_plot_data_clean <- forest_plot_data %>%
  rename(
    estimate = yi,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  )

# Calculate densities for each response variable
density_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarise(
    density_x = list(density(estimate, na.rm = TRUE)$x),  # Extract density x-coordinates
    density_y = list(density(estimate, na.rm = TRUE)$y),  # Extract density y-coordinates
    .groups = "drop"
  ) %>%
  unnest(cols = c(density_x, density_y))  # Flatten the lists into columns

# Add ranking and summary statistics
aggregated_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarize(
    overall_effect = mean(estimate, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    num_observations = n(),
    num_studies = n_distinct(id_article),
    size_category = case_when(
      num_studies <= 2 ~ "1-2",
      num_studies <= 4 ~ "3-4",
      num_studies > 4 ~ "5+"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    size_category = factor(size_category, levels = c("1-2", "3-4", "5+")),
    response_label = paste0(response_variable, " (", num_studies, " studies)"),
    mean_ci_label = paste0(
      sprintf("%.2f", overall_effect), " [",
      sprintf("%.2f", lower_ci), ", ", sprintf("%.2f", upper_ci), "]"
    ),
    response_rank = rank(-overall_effect)  # Negative for descending order
  )

# Merge the rank into the density data
density_data <- density_data %>%
  left_join(aggregated_data %>% select(response_variable, response_label, response_rank), by = "response_variable")

# Calculate overall effect size for all response variables
overall_effect <- aggregated_data %>%
  summarise(
    response_variable = "Overall",
    overall_effect = mean(overall_effect, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    num_observations = n_distinct(forest_plot_data_clean$id_obs),
    num_studies = n_distinct(forest_plot_data_clean$id_article),
    size_category = NA,
    mean_ci_label = paste0(
      sprintf("%.2f", overall_effect), " [",
      sprintf("%.2f", lower_ci), ", ", sprintf("%.2f", upper_ci), "]"
    )
  ) %>%
  mutate(
    response_label = "Overall Effect Size (36 studies)",
    response_rank = Inf  # Ensure "Overall" is at the bottom
  )

# Combine aggregated data with overall effect size
plot_data <- bind_rows(aggregated_data, overall_effect) %>%
  arrange(response_rank) %>%
  mutate(
    response_label = factor(response_label, levels = unique(response_label))
  )

# Adjust density data for plotting (exclude "Overall")
density_data <- density_data %>%
  filter(response_variable != "Overall")

# Plot Forest Plot
# Add a new column to differentiate "Overall" from other response variables
plot_data <- plot_data %>%
  mutate(
    facet_label = ifelse(response_variable == "Overall", "Overall Effect Size", "Response Variables")
  )

density_data <- density_data %>%
  mutate(
    facet_label = ifelse(response_variable == "Overall", "Overall Effect Size", "Response Variables")
  )

# Add a new column for facet labels
plot_data <- plot_data %>%
  mutate(
    facet_label = ifelse(response_variable == "Overall", "Overall Effect Size", "Response Variables")
  )

density_data <- density_data %>%
  mutate(
    facet_label = ifelse(response_variable == "Overall", "Overall Effect Size", "Response Variables")
  )

# Ensure the ordering within facets is correct
plot_data <- plot_data %>%
  mutate(
    response_label = factor(
      response_label,
      levels = c(
        unique(aggregated_data$response_label),  # Other response variables
        "Overall Effect Size (36 studies)"      # Overall effect size at the end
      )
    )
  )

# Create a factor for facet_label with explicit levels
facet_order <- c("Response Variables", "Overall Effect Size")

plot_data <- plot_data %>%
  mutate(facet_label = factor(facet_label, levels = facet_order))

density_data <- density_data %>%
  mutate(facet_label = factor(facet_label, levels = facet_order))

# Add a relative space for facets (using strip scales to control heights)
forest_plot_density <- ggplot() +
  # Add density ridges for response variables
  geom_ridgeline(
    data = density_data %>% filter(response_variable != "Overall"),
    aes(
      x = density_x,
      y = response_label,
      height = density_y,
      fill = response_variable
    ),
    alpha = 0.3,
    scale = 0.05,
    color = NA
  ) +
  # Add points for response variables
  geom_point(
    data = plot_data %>% filter(response_variable != "Overall"),
    aes(
      x = overall_effect,
      y = response_label,
      size = size_category
    ),
    color = "black"
  ) +
  # Add diamond shape for the overall effect size
  geom_point(
    data = plot_data %>% filter(response_variable == "Overall"),
    aes(
      x = overall_effect,
      y = response_label
    ),
    shape = 18,
    size = 5,
    color = "black"
  ) +
  # Add horizontal error bars for all data
  geom_errorbarh(
    data = plot_data,
    aes(
      xmin = lower_ci,
      xmax = upper_ci,
      y = response_label
    ),
    height = 0.2,
    color = "darkgray"
  ) +
  # Add text annotations for mean and confidence intervals
  geom_text(
    data = plot_data,
    aes(
      x = 0.55,
      y = response_label,
      label = mean_ci_label
    ),
    size = 3.5,
    hjust = 0,
    fontface = "italic",
    color = "black"
  ) +
  # Add a vertical reference line at zero
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  # Customize fill colors
  scale_fill_manual(
    values = custom_colors,
    guide = "none"
  ) +
  # Customize point sizes for number of studies
  scale_size_manual(
    values = c("1-2" = 3, "3-4" = 5, "5+" = 7),
    name = "Number of Studies"
  ) +
  # Customize x-axis scale
  scale_x_continuous(
    limits = c(-0.25, 0.6),
    breaks = seq(-0.25, 0.5, by = 0.25)
  ) +
  # Facet by the new facet_label column with adjusted ratios
  facet_grid(
    facet_label ~ .,
    scales = "free_y",
    space = "free",
    switch = "y"
  ) +
  theme_minimal() +
  # Adjust strip and layout to control the space ratio
  theme(
    strip.text.y = element_text(size = 12, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Forest Plot with Adjusted Density and Study Details",
    x = "Effect Size (yi)",
    y = NULL
  )

# Display the plot
forest_plot_density

```
```{r}
# Prepare aggregated data with proper error bar adjustments
aggregated_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarize(
    overall_effect = mean(estimate, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),  # Include lower confidence interval
    upper_ci = mean(upper_ci, na.rm = TRUE),  # Include upper confidence interval
    num_observations = n(),
    num_studies = n_distinct(id_article),
    size_category = case_when(
      num_studies <= 2 ~ "1-2",
      num_studies <= 4 ~ "3-4",
      num_studies > 4 ~ "5+"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    size_category = factor(size_category, levels = c("1-2", "3-4", "5+")),
    response_rank = rank(overall_effect)  # Add rank based on overall effect size
  )

# Adjust error bars to ensure they stay within x-axis limits
aggregated_data <- aggregated_data %>%
  mutate(
    lower_ci = ifelse(lower_ci < -0.5, -0.5, lower_ci),
    upper_ci = ifelse(upper_ci > 0.5, 0.5, upper_ci)
  )

# Combine `aggregated_data` with "Overall" effect size
overall_effect <- aggregated_data %>%
  summarise(
    response_variable = "Overall",
    overall_effect = mean(overall_effect, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    num_observations = n_distinct(forest_plot_data_clean$id_obs),
    num_studies = n_distinct(forest_plot_data_clean$id_article),
    size_category = NA
  ) %>%
  mutate(
    response_label = "Overall Effect Size (36 studies)",
    response_rank = Inf  # Place "Overall" at the bottom
  )

# Combine data for plotting
plot_data <- bind_rows(aggregated_data, overall_effect) %>%
  mutate(
    response_label = ifelse(
      response_variable == "Overall",
      paste0("Overall Effect Size (", num_studies, " studies)"),
      paste0(response_variable, " (", num_studies, " studies)")
    ),
    response_rank = ifelse(response_variable == "Overall", Inf, response_rank)
  ) %>%
  arrange(response_rank) %>%
  mutate(response_label = factor(response_label, levels = unique(response_label)))

# Adjust density data
density_data <- density_data %>%
  mutate(
    facet_label = ifelse(response_variable == "Overall", "Overall Effect Size", "Response Variables"),
    facet_label = factor(facet_label, levels = c("Response Variables", "Overall Effect Size"))
  )

# Merge the rank and labels into density_data
density_data <- density_data %>%
  left_join(
    aggregated_data %>% select(response_variable, response_label),
    by = "response_variable"
  ) %>%
  mutate(
    facet_label = ifelse(response_variable == "Overall", "Overall Effect Size", "Response Variables"),
    facet_label = factor(facet_label, levels = c("Response Variables", "Overall Effect Size"))
  )

# Ensure `response_label` exists in `density_data`
if (!"response_label" %in% colnames(density_data)) {
  stop("Error: response_label column is missing in density_data.")
}

# Create the forest plot
forest_plot_density <- ggplot() +
  # Density ridges for response variables
  geom_ridgeline(
    data = density_data %>% filter(response_variable != "Overall"),
    aes(
      x = density_x,
      y = response_label,
      height = density_y,
      fill = response_variable
    ),
    alpha = 0.3,
    scale = 0.05,
    color = NA
  ) +
  # Points for response variables
  geom_point(
    data = plot_data %>% filter(response_variable != "Overall"),
    aes(
      x = overall_effect,
      y = response_label,
      size = size_category
    ),
    color = "black"
  ) +
  # Diamond for overall effect size
  geom_point(
    data = plot_data %>% filter(response_variable == "Overall"),
    aes(
      x = overall_effect,
      y = response_label
    ),
    shape = 18,
    size = 5,
    color = "black"
  ) +
  # Error bars for all data
  geom_errorbarh(
    data = plot_data,
    aes(
      xmin = lower_ci,
      xmax = upper_ci,
      y = response_label
    ),
    height = 0.2,
    color = "darkgray"
  ) +
  # Adjust x-axis scale
  scale_x_continuous(
    limits = c(-0.5, 0.5),
    breaks = seq(-0.5, 0.5, by = 0.25)
  ) +
  # Facet by overall and response variables
  facet_grid(
    facet_label ~ .,
    scales = "free_y",
    space = "free"
  ) +
  # Add labels and themes
  labs(
    title = "Forest Plot with Adjusted Density and Study Details",
    x = "Effect Size (yi)",
    y = NULL,
    size = "Number of Studies"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(size = 12, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the plot
forest_plot_density

```

```{r}
# Filter and prepare the data for Forest plot
forest_plot_data <- imp_dataset_imputed %>%
  mutate(
    lower_ci = yi - 1.96 * sqrt(vi),
    upper_ci = yi + 1.96 * sqrt(vi)
  ) |> 
  select(
    id_article,
    id_obs,
    # Key response variables (Ecosystem Services)
    response_variable, 
    # Key Moderators
    crop_type, 
    tree_type,
    age_system,
    season,
    soil_texture,
    # Effect Size Measure
    yi, 
    lower_ci, 
    upper_ci
  ) |> 
  filter(!is.na(yi), !is.na(lower_ci), !is.na(upper_ci))

forest_plot_data |> glimpse()
```

```{r}
# Ensure correct column names for lower and upper confidence intervals
forest_plot_data_clean <- forest_plot_data %>%
  rename(
    estimate = yi,        # Rename yi to estimate
    lower_ci = lower_ci,  # Use correct column name for lower confidence bound
    upper_ci = upper_ci   # Use correct column name for upper confidence bound
  )

# Create meaningful size categories based on the number of studies
aggregated_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarize(
    overall_effect = mean(estimate, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    num_studies = n_distinct(crop_type)  # Number of unique crop types as a proxy for studies
  ) %>%
  mutate(
    size_category = case_when(
      num_studies <= 2 ~ "1-2",   # Category 1
      num_studies <= 4 ~ "3-4",   # Category 2
      num_studies > 4 ~ "5+"      # Category 3
    )
  ) %>%
  mutate(
    size_category = factor(size_category, levels = c("1-2", "3-4", "5+"))  # Set the desired order
  )
```

```{r}
# Define a custom color palette for response_variable
custom_colors <- c(
  "Biodiversity" = "#FF9999",          # Light red
  "Greenhouse gas emission" = "#66C266",  # Green
  "Product quality" = "#FFC000",       # Yellow
  "Crop yield" = "#FF9933",           # Orange
  "Pest and Disease" = "#33CCCC",     # Teal
  "Soil quality" = "#9966CC",         # Purple
  "Water quality" = "#9999FF"         # Light blue
)


# Generate the forest plot with black dots and colored error bars
overall_effect_size <- aggregated_data |> 
  ggplot(aes(x = overall_effect, y = reorder(response_variable, overall_effect))) +
  geom_point(aes(size = size_category), color = "black", alpha = 0.8) +  # Mean effect size points are black
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, color = response_variable), height = 0.2, size = 1) +  # Error bars colored by response_variable
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line at 0
  scale_size_manual(
    values = c("1-2" = 3, "3-4" = 5, "5+" = 7),  # Map categories to sizes
    name = "Number of Studies"
  ) +
  scale_color_manual(
    values = custom_colors,  # Apply custom colors to error bars
    name = "Response Variable"
  ) +
  labs(
    title = "Overall Effect Size by Response Variable",
    x = "Effect Size (Mean Estimate)",
    y = "Response Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),  # Larger y-axis text
    axis.text.x = element_text(size = 12),  # Larger x-axis text
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered and bold title
    legend.position = "none",
    panel.spacing = unit(1, "lines")  # Increase spacing between panels
  )

overall_effect_size
```
```{r}
# Prepare and clean the data for effect sizes
forest_plot_data_clean <- forest_plot_data %>%
  rename(
    estimate = yi,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  )

# Calculate densities for each response variable
density_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarise(
    density_x = list(density(estimate, na.rm = TRUE)$x),  # Extract density x-coordinates
    density_y = list(density(estimate, na.rm = TRUE)$y),  # Extract density y-coordinates
    .groups = "drop"
  ) %>%
  unnest(cols = c(density_x, density_y))  # Flatten the lists into columns

# Add ranking for response variables based on overall effect size
aggregated_data <- forest_plot_data_clean %>%
  group_by(response_variable) %>%
  summarize(
    overall_effect = mean(estimate, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),  # Ensure lower_ci is included
    upper_ci = mean(upper_ci, na.rm = TRUE),  # Ensure upper_ci is included
    num_observations = n(),
    num_studies = n_distinct(id_article),
    size_category = case_when(
      num_studies <= 2 ~ "1-2",
      num_studies <= 4 ~ "3-4",
      num_studies > 4 ~ "5+"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    size_category = factor(size_category, levels = c("1-2", "3-4", "5+")),
    response_rank = rank(overall_effect)  # Add rank based on descending effect size
  ) # |> 
# Adjust error bars to fit within x-axis limits
# mutate(
#   lower_ci = ifelse(lower_ci < -0.25, -0.25, lower_ci),
#   upper_ci = ifelse(upper_ci > 0.5, 0.5, upper_ci)
# )

# Merge the rank into the density data
density_data <- density_data %>%
  left_join(aggregated_data %>% select(response_variable, response_rank), by = "response_variable")  # Add rank

# Plot with added error bars
density_forrest_plot <- ggplot() +
  # Add density ridges for actual effect sizes (`yi`) per response variable
  geom_ridgeline(
    data = density_data,
    aes(x = density_x, y = reorder(response_variable, response_rank), height = density_y, fill = response_variable),
    alpha = 0.3,
    scale = 0.08,  # Adjust scale for ridge height
    color = NA
  ) +
  # Add horizontal error bars for confidence intervals
  geom_errorbarh(
    data = aggregated_data,
    aes(xmin = lower_ci, xmax = upper_ci, y = reorder(response_variable, response_rank)),
    height = 0.1,
    color = "darkgray",
    size = 0.8
  ) +
  # Add clipped horizontal error bars
  # geom_errorbarh(
  #   data = aggregated_data,
  #   aes(xmin = lower_ci, xmax = upper_ci, y = reorder(response_variable, response_rank)),
  #   height = 0.1,
  #   color = "darkgray",
  #   size = 0.8
  # ) +
  # Add points for overall mean effect sizes
  geom_point(
    data = aggregated_data,
    aes(x = overall_effect, y = reorder(response_variable, response_rank), size = size_category),
    color = "black",
    alpha = 0.8
  ) +
  # Reference line at zero for effect sizes
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # Customize scales
  scale_fill_manual(values = custom_colors, guide = "none") +
  scale_size_manual(
    values = c("1-2" = 3, "3-4" = 5, "5+" = 7),
    name = "Number of Studies"
  ) +
  # Adjust x-axis scale
  scale_x_continuous(
    breaks = seq(-0.5, 1.0, by = 0.25)  # Customize tick marks without setting limits
    # limits = c(-0.25, 0.5),  # Set x-axis range
    # breaks = seq(-0.5, 1.0, by = 0.25)  # Optional: Customize tick marks
  ) +
  # Labels and titles
  labs(
    title = "Forest Plot with Adjusted Density of Effect Sizes",
    x = "Effect Size (yi)",
    y = NULL
  ) +
  # Themes and aesthetics
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

density_forrest_plot
```


```{r}
# Function for the generic forest plot for each moderator - response variable combination
create_forest_plot_moderator <- function(data, custom_colors = NULL) {
  # Calculate summary statistics (mean effect size and CI) for each response variable and tree_type
  summary_stats <- data %>%
    group_by(response_variable, tree_type) %>%
    summarize(
      mean_effect = mean(yi, na.rm = TRUE),
      lower_ci_mean = mean(lower_ci, na.rm = TRUE),
      upper_ci_mean = mean(upper_ci, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create the plot
  forest_plot <- ggplot(data) +
    # Add ridgeline for number of observations
    geom_density_ridges(
      aes(
        x = yi,
        y = tree_type,
        fill = response_variable
      ),
      alpha = 0.3,
      scale = 0.8
    ) +
    # Add points for individual observations
    geom_point(
      aes(
        x = yi,
        y = tree_type,
        color = response_variable
      ),
      size = 2,
      position = position_jitter(height = 0.1)  # Add slight jitter for visibility
    ) +
    # Add horizontal error bars for individual observations
    geom_errorbarh(
      aes(
        xmin = lower_ci,
        xmax = upper_ci,
        y = tree_type
      ),
      height = 0.2,
      color = "darkgray"
    ) +
    # Add text for mean effect size and CI on the right
    geom_text(
      data = summary_stats,
      aes(
        x = 2.5,  # Adjust this to position the text correctly
        y = tree_type,
        label = sprintf("%.2f [%.2f, %.2f]", mean_effect, lower_ci_mean, upper_ci_mean)
      ),
      size = 3.5,
      hjust = 0,
      color = "black"
    ) +
    # Facet by response variable
    facet_wrap(~response_variable, ncol = 1, scales = "free_x") +
    # Add vertical reference line
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    # Customize axes and scales
    scale_x_continuous(
      breaks = seq(-1, 3, by = 0.5),
      limits = c(-1, 3)  # Adjust limits as needed
    ) +
    scale_fill_manual(values = custom_colors) +
    scale_color_manual(values = custom_colors) +
    # Add labels and themes
    labs(
      title = "Generic Forest Plot with Moderators and Mean Effect Sizes",
      x = "Effect Size",
      y = "Tree Type"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      strip.placement = "outside",
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "none",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  return(forest_plot)
}
```
```{r}
# Generate the forest plot for tree_type moderator
forest_plot_tree_type <- create_forest_plot_moderator(
  forest_moderator, 
  custom_colors = custom_colors
)

forest_plot_tree_type
```
```{r}
# Summarize the data for error bars and unique studies
forest_moderator_summary <- forest_moderator %>%
  group_by(response_variable, tree_type) %>%
  summarise(
    mean_effect = mean(yi, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    num_studies = n_distinct(id_article),  # Unique studies
    ci_label = sprintf("%.2f [%.2f, %.2f]", mean(yi, na.rm = TRUE), mean(lower_ci, na.rm = TRUE), mean(upper_ci, na.rm = TRUE)),
    .groups = "drop"
  )

# Plot with density ridges, error bars, and unique study counts
forest_plot_tree_type <- ggplot() +
  # Density ridges for number of observations
  geom_density_ridges(
    data = forest_moderator,
    aes(x = yi, y = tree_type, fill = response_variable),
    alpha = 0.3,
    scale = 1.2,  # Adjust ridge height scaling
    rel_min_height = 0.01,  # Exclude very small densities
    color = "black"
  ) +
  # Error bars for mean effect sizes
  geom_errorbarh(
    data = forest_moderator_summary,
    aes(
      xmin = lower_ci,
      xmax = upper_ci,
      y = tree_type,
      color = response_variable
    ),
    height = 0.2
  ) +
  # Mean effect size markers
  geom_point(
    data = forest_moderator_summary,
    aes(
      x = mean_effect,
      y = tree_type,
      color = response_variable
    ),
    size = 3
  ) +
  # CI labels on the right side of the plot
  geom_text(
    data = forest_moderator_summary,
    aes(
      x = max(forest_moderator$yi, na.rm = TRUE) + 0.5,  # Position slightly outside the range
      y = tree_type,
      label = ci_label
    ),
    hjust = 0,  # Left-align the text
    size = 3.5,
    color = "black"
  ) +
  # Add study counts (number of unique studies) below the tree types
  geom_text(
    data = forest_moderator_summary,
    aes(
      x = min(forest_moderator$yi, na.rm = TRUE) - 0.5,  # Position slightly outside the range
      y = tree_type,
      label = paste0("n=", num_studies)
    ),
    hjust = 1,  # Right-align the text
    size = 3.5,
    color = "black"
  ) +
  # Facet for response variables
  facet_wrap(~response_variable, scales = "free", ncol = 1) +
  # Add vertical reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # Customize fill and color scales
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  # Adjust plot labels and theme
  labs(
    title = "Generic Forest Plot with Moderators and Mean Effect Sizes",
    x = "Effect Size",
    y = "Tree Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )

# Display the plot
forest_plot_tree_type

```


```{r}
# Adjusted forest plot with free x-axis scale
forest_plot_response_var_tree_type <- forest_plot_data |> 
  ggplot(aes(x = yi, y = tree_type, color = response_variable)) +
  geom_point(size = 3) +  # Effect size
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # Confidence intervals
  facet_wrap(~response_variable, scales = "free_x") +  # Free x-axis scale for each facet
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line
  labs(
    title = "Forest Plot by Ecosystem Service and Response Variable (AES)",
    x = "Effect Size",
    y = "Tree Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none"
  )

forest_plot_response_var_tree_type
```
```{r}
forest_plot_data |> 
  ggplot(aes(x = yi, y = tree_type, color = response_variable)) +
  geom_point(size = 3) +  # Effect size
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # Confidence intervals
  facet_wrap(~response_variable, scales = "free_x") +  # Free x-axis scale for each facet
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line
  labs(
    title = "Forest Plot by Ecosystem Service and Response Variable (AES)",
    x = "Effect Size",
    y = "Tree Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none"
  )
```

