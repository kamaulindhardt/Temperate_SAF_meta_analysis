
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




