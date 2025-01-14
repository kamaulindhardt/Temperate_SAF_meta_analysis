
*Points to address after the Tshering meeting with her supervisor (26/11-2024)*

* Subgroup Analysis:
Keep subgroup analyses for each response_variable (e.g., biodiversity, crop yield) to respect conceptual differences. This ensures tailored insights into unique trends and moderators like tree_type or crop_type. Account for reduced statistical power in smaller subgroups by adjusting for multiple comparisons (e.g., Bonferroni). Compare heterogeneity metrics (Q, I², τ²) across subgroups to identify variability differences.

* Meta-Regression:
Use meta-regression as a complementary step to capture overarching patterns. Include response_variable as a moderator and test interactions (e.g., tree_type * response_variable) to detect shared vs. outcome-specific effects. Start with a simpler model and add key moderators incrementally to maintain interpretability.

* Heterogeneity Comparison:
Compare heterogeneity metrics (I², τ²) between original and imputed datasets to assess the impact of imputation. Highlight changes in variability and their implications.

* Visualizations:
Simplify forest plots, focusing on clarity (e.g., one plot per response variable). Use ggplot for clean annotations, log-scaled axes (if relevant), and consistent aesthetics. Add funnel plots for individual response variable models and meta-regression to assess bias and precision.

*Points to address after the meeting with Maarit (03/12-2024*

#### **1. Imputation**
   - **Evaluate Missingness:** 
     - Check patterns of missingness in the original data and verify study reliability.
     - Assess missingness in moderator-response variable combinations.
   - **Non-Random Missingness:** 
     - Test for non-random missingness in standard deviations for moderator-response combinations.
   - **Imputation Methodology:**
     - Use robust methods (e.g., upper quartile) for imputation if data quality is uncertain.
     - Avoid imputing response variables, moderators, or fixed-effect variables (especially continuous ones).
   - **Model Comparison:**
     - Compare model results using imputed vs. non-imputed datasets to assess imputation impact.

---> missingness pattern assessment before imputation on the _sd (we should have received something from Maarit). The pmm method might not be appropriate. We should impute based on the missingness. We can also test the linear reg. method. Remind Maarit about the imputation paper + script. Add the test the linear reg. method

---> Missingness pattern (across response variable and moderators). 
Are there specific patterns. And then what to do with that? 


---

#### **2. Moderators**
   - Ensure a minimum of 10 studies per sublevel of moderators for sufficient statistical power.



---

#### **3. Random Effects**
   - **Revise `exp_id`:**
     - Aggregate location data within `exp_id` to simplify its structure.
   - **Simplify Random Effects:**
     - Remove unnecessary random effects (`id_article/response_variable`) if `exp_id` sufficiently captures random intercepts/slopes.

---

#### **4. Year as a Fixed Effect**
   - Add standardized year as a fixed-effect, continuous variable to capture potential slope effects.
   - Validate the structure and missingness of the `exp_id` variable before inclusion.
   - "This morning, I worked on our database to determine which level we can use for the location. Since we are already focusing on the temperate climatic zone, I thought it would be good to use the country as the location."
---

#### **5. Multilevel Modelling vs. Subgroup Analysis**
   - Use **multilevel modelling** to account for incomplete combinations of moderators and response variables.
   - Multilevel modelling is valid for disentangling hierarchical structures but, for transparency and communication purposes, subgroup meta-analysis will remain the primary approach.
   - Consider specific interaction terms between moderators after including additive effects in the model.

---

#### **6. Bias Assessment**
   - Test for publication bias and variance error bias for each response variable and moderator.

---

#### **7. Global Mean Comparisons**
   - Include a global mean to compare overall responses, particularly in cases of sign changes across responses.

---

#### **8. Model Diagnostics**
   - **Delta AIC:**
     - Use delta AIC to compare model performance between alternative specifications.
   - **Influence Diagnostics:**
     - Perform diagnostics on a study level using the `nlme` package with `id_article` as the grouping variable.




Key Action Points
	1. Check and Address Missing Data:
		○ Assess patterns of missingness and trustworthiness of studies.
		○ Implement robust imputation for error values only.
	2. Aggregate and Simplify:
		○ Aggregate soil texture levels and location data within exp_id.
	3. Review Model Structure:
		○ Simplify random effects.
		○ Include standardized year as a fixed effect.
	4. Evaluate Bias:
		○ Perform publication and variance error bias assessments.
	5. Use Multilevel Models:
		○ Implement multilevel modeling to manage hierarchical structures.
	6. Conduct Diagnostics:
		○ Apply influence diagnostics at the study level (nlme).
	7. Include Interaction Terms:
		○ Incorporate interactions only after main effects in the model.
	8. Global Response Comparisons:
Use a global mean for overarching response evaluations.


*Points to address after our 2025 meeting (02/01-2025)*

---> missingness pattern assessment before imputation on the _sd (we should have received something from Maarit). 
	The pmm method might not be appropriate. We should impute based on the missingness. We can also test the linear reg. method. 
	Remind Maarit about the imputation paper + script. Add the test the linear reg. method

---> Missingness pattern (across response variable and moderators). 
Are there specific patterns. And then what to do with that? 

----> Water qual and GGE is left out of the analysis because of too little data

----> Leave-one-out analysis for sensitivity analysis on moderators and articles/studies on the effect size. This should be done on the full dataset and not on subsets of response variables

----> Publication-ready plots: Forest plots, caterpillar plots, funnel plots, and geographical maps of study locations.

----> Update the methods section with the new approach for the multivariate analysis















(1)
# Group data by relevant columns for Experiment ID
# The exp_id variable is created as a unique identifier for experiments, 
# based on grouping by: id_article (the article ID), location (the geographical location of the experiment), and 
# the experiment_year (the year the experiment was conducted)
group_by(id_article, location, experiment_year) |>
  mutate(exp_id = cur_group_id()) |>
  ungroup() |> 
  
  
  (2)
# Group data by relevant columns for Experiment ID
# The exp_id variable is created as a unique identifier for experiments, 
# based on grouping by: id_article (the article ID), location (the geographical location of the experiment), 
# the experiment_year (the year the experiment was conducted), and study duration (the number of years the study has been completed)
group_by(id_article, location, experiment_year, study duration) |>
  mutate(exp_id = cur_group_id()) |>
  ungroup() |> 
  
  A)
# Fit a multivariate meta-analytic model using `rma.mv` within a try-catch block
model <- tryCatch({
  # Fit a random-effects multivariate meta-analysis model
  rma.mv(
    # Dependent variable: effect size (yi)
    yi = yi,
    # Variance-covariance matrix (V): accounts for within-study sampling variance and potential correlations
    V = v_matrix,
    # Moderators: a formula specifying the covariates to include in the model
    mods = moderator_formula,
    # Random-effects structure: defines how the random effects are modeled hierarchically
    random = list(
      ~ 1 | id_article,                           # Random intercept for each article/study
      ~ 1 | id_article/response_variable,         # Nested random intercept for each response variable within articles
      ~ 1 | exp_id                                # Random intercept for individual experiments
    ),
    
    
    B)      
  # Fit a multivariate meta-analytic model using `rma.mv` within a try-catch block
  model <- tryCatch({
    # Fit a random-effects multivariate meta-analysis model
    rma.mv(
      # Dependent variable: effect size (yi)
      yi = yi,
      # Variance-covariance matrix (V): accounts for within-study sampling variance and potential correlations
      V = v_matrix,
      # Moderators: a formula specifying the covariates to include in the model
      mods = moderator_formula,
      # Random-effects structure: defines how the random effects are modeled hierarchically
      random = list(
        ~ 1 | id_article/response_variable,         # Nested random intercept for each response variable within articles
        ~ 1 | exp_id                                # Random intercept for individual experiments
      ),
      
      
      C)
    # Fit a multivariate meta-analytic model using `rma.mv` within a try-catch block
    model <- tryCatch({
      # Fit a random-effects multivariate meta-analysis model
      rma.mv(
        # Dependent variable: effect size (yi)
        yi = yi,
        # Variance-covariance matrix (V): accounts for within-study sampling variance and potential correlations
        V = v_matrix,
        # Moderators: a formula specifying the covariates to include in the model
        mods = moderator_formula,
        # Random-effects structure: defines how the random effects are modeled hierarchically
        random = list(
          ~ 1 | exp_id                                # Random intercept for individual experiments
        ),
        
        D)
      # Fit a multivariate meta-analytic model using `rma.mv` within a try-catch block
      model <- tryCatch({
        # Fit a random-effects multivariate meta-analysis model
        rma.mv(
          # Dependent variable: effect size (yi)
          yi = yi,
          # Variance-covariance matrix (V): accounts for within-study sampling variance and potential correlations
          V = v_matrix,
          # Moderators: a formula specifying the covariates to include in the model
          mods = moderator_formula,
          # Random-effects structure: defines how the random effects are modeled hierarchically
          random = list(
            ~ 1 + Year | id_article,                    # Random intercept for each year and article/study
            ~ 1 | id_article/response_variable,         # Nested random intercept for each response variable within articles
            ~ 1 | exp_id                                # Random intercept for individual experiments
          ),
          
          





























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



```{r, eval=FALSE}
# Function to safely convert to numeric, replacing non-numeric values with NA
safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Data Preprocessing
database_clean_dummy <- database_dummy |>
  # Step 1: Clean column names
  janitor::clean_names() |>
  
  # Step 2: Convert id_article and id_obs to integer
  mutate(
    id_article = as.integer(id_article),
    id_obs = as.integer(id_obs)
  ) |>
  
  # Step 3: Convert standard errors and other numeric columns
  mutate(
    silvo_mean = safe_as_numeric(silvo_mean),
    silvo_se = safe_as_numeric(silvo_se),
    silvo_sd = safe_as_numeric(silvo_sd),
    silvo_n = safe_as_numeric(silvo_n),
    control_mean = safe_as_numeric(control_mean),
    control_se = safe_as_numeric(control_se),
    control_sd = safe_as_numeric(control_sd),
    control_n = safe_as_numeric(control_n),
    tree_age = safe_as_numeric(tree_age),
    no_tree_per_m = as.character(no_tree_per_m)) |> 
  
  # Step 4: Create Identifiers (Experiment, Treatment, Common Control)
  # Group data by relevant columns for Treatment ID
  group_by(id_article, tree_type, crop_type, location, experiment_year) |>
  mutate(treat_id = cur_group_id()) |>
  ungroup() |>
  
  # Group data by relevant columns for Experiment ID
  group_by(id_article, location, experiment_year) |>
  mutate(exp_id = cur_group_id()) |>
  ungroup() |> 
  
  # Step 5: Ensure no infinite or NaN values are present in any columns
  mutate(across(everything(), ~ifelse(is.infinite(.) | is.nan(.), NA, .))
  ) |> 
  
  # Step 6: Convert "NA" strings to real NA values, excluding 'id_article' and 'id_obs'
  mutate(
    across(
      .cols = where(is.character) & !c("id_article", "id_obs"),
      .fns = ~ na_if(., "NA")
    )
  ) |>
  
  # Step 7: Convert year columns to date format
  # Convert to proper Date format using "YYYY-01-01"
  mutate(
    experiment_year = as.Date(paste0(experiment_year, "-01-01")),
    year_est_exp = as.Date(paste0(year_est_exp, "-01-01")),
    study_year_start = as.Date(paste0(study_year_start, "-01-01")),
    study_year_end = as.Date(paste0(study_year_end, "-01-01"))
  ) |> 
  # Step 8: Rename Latitude and Longitude to lat and lon
  rename(
    lat = latitude,
    lon = longitude
  ) |>
  
  # Step 9: Convert lat and lon to numeric coordinates
  mutate(
    lat = str_replace_all(lat, "[°NS]", "") |> safe_as_numeric(),
    lon = str_replace_all(lon, "[°EW]", "") |> safe_as_numeric(),
    lat = if_else(str_detect(lat, "S$"), -lat, lat),
    lon = if_else(str_detect(lon, "W$"), -lon, lon)
  ) |>
  
  # Step 10: Create a Coherent 'site_x' Column
  mutate(
    # If `lat` and `lon` are present, use them; otherwise, use the `location` name
    site_x = case_when(
      !is.na(lat) & !is.na(lon) ~ paste(lat, lon, sep = ", "),
      !is.na(location) ~ location,
      TRUE ~ NA_character_
    )
  ) 
```

mutate(
  # Clean site_x values to remove extra spaces
  site_x = str_trim(site_x),
  # Extract latitude and longitude from 'site_x' with a more robust pattern
  extracted_lat = str_extract(site_x, "[-]?\\d+\\.\\d+(?=, ?[-]?\\d+\\.\\d+)") |> as.numeric(),
  extracted_lon = str_extract(site_x, "(?<=, ?)[-]?\\d+\\.\\d+") |> as.numeric()
)


The geocoder is not needed becasue all observations has geographical coorordinates (lat. lon.)

# Step 2: Identify rows that need geocoding (i.e., where coordinates are missing)
locations_to_geocode <- database_clean |>
  filter(is.na(extracted_lat) | is.na(extracted_lon)) |>
  distinct(location) |>
  filter(!is.na(location))

# Step 3: Geocode Location Names
geocoded_locations <- locations_to_geocode |>
  geocode(address = location, method = "osm", lat = "geo_lat", long = "geo_lon")

# Step 4: Merge Geocoded Coordinates Back to the Dataset
database_clean <- database_clean |>
  left_join(geocoded_locations, by = "location") |>
  mutate(
    # Use extracted coordinates if available, otherwise use geocoded coordinates
    final_lat = coalesce(extracted_lat, geo_lat),
    final_lon = coalesce(extracted_lon, geo_lon),
    # Create the `exp_site_loc` column with final coordinates
    exp_site_loc = if_else(!is.na(final_lat) & !is.na(final_lon),
                           paste(final_lat, final_lon, sep = ", "),
                           NA_character_)
  ) 



```{r}
# Step 1: Extract Coordinates from `site_x` if available
database_clean <- database_clean |>
  mutate(
    # Extract latitude: Matches integers or decimals before a comma
    extracted_lat = str_extract(site_x, "[-]?\\d+(\\.\\d+)?(?=, )"),
    # Extract longitude: Matches integers or decimals after a comma and space
    extracted_lon = str_extract(site_x, "(?<=, )[-]?\\d+(\\.\\d+)?")
  ) |>
  mutate(
    # Convert extracted values to numeric
    extracted_lat = as.numeric(extracted_lat),
    extracted_lon = as.numeric(extracted_lon)
  )

# Step 2: Identify rows that need geocoding (i.e., where coordinates are missing)
locations_to_geocode <- database_clean |>
  filter(is.na(extracted_lat) | is.na(extracted_lon)) |>
  distinct(location) |>
  filter(!is.na(location))

# Step 3: Geocode Location Names
geocoded_locations <- locations_to_geocode |>
  geocode(address = location, method = "osm", lat = "geo_lat", long = "geo_lon")

# Step 4: Merge Geocoded Coordinates Back to the Dataset
database_clean <- database_clean |>
  left_join(geocoded_locations, by = "location") |>
  mutate(
    # Use extracted coordinates if available, otherwise use geocoded coordinates
    final_lat = coalesce(extracted_lat, geo_lat),
    final_lon = coalesce(extracted_lon, geo_lon),
    # Create the `exp_site_loc` column with final coordinates
    exp_site_loc = if_else(!is.na(final_lat) & !is.na(final_lon),
                           paste(final_lat, final_lon, sep = ", "),
                           NA_character_)
  ) |>
  select(-extracted_lat, -extracted_lon, -geo_lat, -geo_lon)|> 
  
  
  
  # Step 5: Relocate columns to the desired order
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )

# Passing 23 addresses to the Nominatim single address geocoder
# Query completed in: 23.4 seconds

# Last run (03/12-2024)
# Passing 4 addresses to the Nominatim single address geocoder
# Query completed in: 4.3 seconds

# Last run (01/01-2025)
# Passing 3 addresses to the Nominatim single address geocoder
# Query completed in: 3 seconds
```

Id_article	Id_obs	Site	Location	Latitude	Longitude	Experimental_design	Experiment_Year	Study_duration	Comparator	Tree_type	Crop_type
10	366	Leeds	England	53.883333° N	1.5491° W	NA	1992	7	Monoculture	Biomass	Cereal

Id_article	Id_obs	Site	Location	Latitude	Longitude	Experimental_design	Experiment_Year	Study_duration	Comparator	Tree_type	Crop_type	Year_est_exp
29	873	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	874	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	875	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	876	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	877	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	878	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	879	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	880	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	881	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	882	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	883	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	884	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	885	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	886	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	887	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	888	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	889	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	890	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	891	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	892	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	893	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	894	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	895	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	896	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	897	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	898	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	899	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	900	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	901	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	902	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	903	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	904	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	905	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	906	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	907	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987
29	908	Nothern England	England	53.866667°N	1.320278°W	NA	1990	>2	Monoculture	Timber	Legume	1987

Id_article	Id_obs	Site	Location	Latitude	Longitude	Experimental_design	Experiment_Year	Study_duration	Comparator	Tree_type	Crop_type	Year_est_exp
36	1074	Xinjiang	China	73.370° N	34.200° E	Single-factor design	2011	2	Monoculture	Fruit,nut & other	Cereal	2011
36	1075	Xinjiang	China	73.370° N	34.200° E	Single-factor design	2011	2	Monoculture	Fruit,nut & other	Cereal	2011



Id_article	Id_obs	Site	Location	Latitude	Longitude	Experimental_design	Experiment_Year	Study_duration	Comparator	Tree_type	Crop_type	Year_est_exp
10	380	Silsoe	UK	52.0° N	0.433333° W	NA	1995	7	Monoculture	Biomass	Cereal	1992
10	381	Silsoe	UK	52.0° N	0.433333° W	NA	1995	7	Monoculture	Biomass	Cereal	1992
10	382	Silsoe	UK	52.0° N	0.433333° W	NA	1996	7	Monoculture	Biomass	Cereal	1992
10	383	Silsoe	UK	52.0° N	0.433333° W	NA	1996	7	Monoculture	Biomass	Cereal	1992
10	384	Silsoe	UK	52.0° N	0.433333° W	NA	1997	7	Monoculture	Biomass	Cereal	1992
10	385	Silsoe	UK	52.0° N	0.433333° W	NA	1997	7	Monoculture	Biomass	Cereal	1992
10	386	Silsoe	UK	52.0° N	0.433333° W	NA	1998	7	Monoculture	Biomass	Legume	1992
21	622	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	623	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	624	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	625	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	626	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	627	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	628	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	629	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	630	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	631	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
21	632	Bedfordshire	UK	52.000000° N	-0.430000° W	NA	2011	>2	Monoculture	Biomass	Cereal	1992
















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




```{r}
impute_and_merge <- function(dataset, moderators, dataset_name = "Dataset") {
  
  cat("Starting imputation for", dataset_name, "...\n")
  
  # Step 1: Prepare data for imputation
  cols_for_impute <- dataset %>%
    select(
      yi, vi,
      id_article, id_obs, exp_id,
      response_variable, all_of(moderators)
    )
  
  # Step 2: Convert categorical variables to factors
  cols_for_impute <- cols_for_impute %>%
    mutate(across(all_of(moderators), as.factor))
  
  # Step 3: Perform multiple imputation using mice
  set.seed(1234)
  imputed_data <- mice(
    cols_for_impute,
    m = 20,         # Number of imputations
    maxit = 100,    # Maximum iterations
    method = 'pmm', # Predictive Mean Matching
    printFlag = FALSE
  )
  
  # Step 4: Extract the first imputed dataset for merging
  completed_data <- complete(imputed_data, 1)
  
  # Step 5: Join the imputed values back to the original dataset
  merged_dataset <- dataset %>%
    left_join(
      completed_data %>%
        select(id_article, id_obs, exp_id, all_of(moderators)),
      by = c("id_article", "id_obs", "exp_id"),
      suffix = c("_original", "_imputed")
    )
  
  # Step 6: Replace missing values in the original columns with imputed values
  for (mod in moderators) {
    original_col <- paste0(mod, "_original")
    imputed_col <- paste0(mod, "_imputed")
    
    if (original_col %in% colnames(merged_dataset) && imputed_col %in% colnames(merged_dataset)) {
      merged_dataset[[mod]] <- ifelse(
        is.na(merged_dataset[[original_col]]),
        merged_dataset[[imputed_col]],
        merged_dataset[[original_col]]
      )
    }
  }
  
  # Step 7: Drop the temporary columns
  merged_dataset <- merged_dataset %>%
    select(-ends_with("_original"), -ends_with("_imputed"))
  
  cat("Imputation completed for", dataset_name, ".\n")
  
  return(merged_dataset)
}
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


# Performing moderator imputations
moderators <- c("tree_type", "crop_type", "age_system", "season", 
                "soil_texture", "no_tree_per_m", "tree_height", "alley_width")

# Impute and merge for non-imputed dataset
non_imp_dataset_imputed <- impute_and_merge(non_imp_dataset, moderators, "Non-Imputed Dataset")

# Impute and merge for imputed dataset
imp_dataset_imputed <- impute_and_merge(imp_dataset, moderators, "Imputed Dataset")


############################################################################################################################
# Helper function to convert numeric imputed values to categorical factors
convert_to_factors <- function(data) {
  # Convert 'no_tree_per_m' to character factors (Low, High)
  data <- data %>%
    mutate(
      no_tree_per_m = case_when(
        no_tree_per_m %in% c(1, "1") ~ "Low",
        no_tree_per_m %in% c(2, "2") ~ "High",
        TRUE ~ as.character(no_tree_per_m)
      ) %>% as.factor()
    )
  
  # Convert 'tree_height' to character factors (Short, Tall)
  data <- data %>%
    mutate(
      tree_height = case_when(
        tree_height %in% c(1, "1") ~ "Short",
        tree_height %in% c(2, "2") ~ "Tall",
        TRUE ~ as.character(tree_height)
      ) %>% as.factor()
    )
  
  # Convert 'alley_width' to character factors (Narrow, Wide)
  data <- data %>%
    mutate(
      alley_width = case_when(
        alley_width %in% c(1, "1") ~ "Narrow",
        alley_width %in% c(2, "2") ~ "Wide",
        TRUE ~ as.character(alley_width)
      ) %>% as.factor()
    )
  
  # Convert 'age_system' to character factors (Narrow, Wide)
  data <- data %>%
    mutate(
      age_system = case_when(
        age_system %in% c(1, "1") ~ "Young",
        age_system %in% c(2, "2") ~ "Medium",
        age_system %in% c(3, "3") ~ "Mature",
        TRUE ~ as.character(age_system)
      ) %>% as.factor()
    )
  
  # Convert 'season' to character factors (Narrow, Wide)
  data <- data %>%
    mutate(
      season = case_when(
        season %in% c(1, "1") ~ "Summer",
        season %in% c(2, "2") ~ "Winter",
        season %in% c(3, "3") ~ "WinterSummer",
        TRUE ~ as.character(season)
      ) %>% as.factor()
    )
  
  return(data)
}

# Apply the conversion function to both datasets
non_imp_dataset_imputed <- convert_to_factors(non_imp_dataset_imputed) |> 
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Effect size measure
    yi, vi,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )


imp_dataset_imputed <- convert_to_factors(imp_dataset_imputed) |> 
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Effect size measure
    yi, vi,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )



############################################################################################################################


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (16/11-24)
# Starting imputation for Non-Imputed Dataset ...
# Advarsel: Number of logged events: 1Imputation completed for Non-Imputed Dataset .
# Starting imputation for Imputed Dataset ...
# Advarsel: Number of logged events: 1Imputation completed for Imputed Dataset .
# Time difference of 1.3963 mins

# Check the structure of the datasets
# str(non_imp_dataset_imputed)
# str(imp_dataset_imputed)
```

Assessing imputation of moderators again

```{r}
# Assessing Moderator missingness

moderators <- c("tree_type", "crop_type", "age_system", "season", 
                "soil_texture", "no_tree_per_m", "tree_height", "alley_width")

# Assess missing data for non-imputed dataset
assess_missing_data(non_imp_dataset_imputed, moderators, "Non-Imputed Dataset")

# Assess missing data for imputed dataset
assess_missing_data(imp_dataset_imputed, moderators, "Imputed Dataset")
```

Additional assessment of the moderator imputation

```{r}
# Function to calculate missing data proportions
calculate_missing_proportions <- function(data, moderators) {
  data %>%
    pivot_longer(cols = all_of(moderators), names_to = "moderator", values_to = "value") %>%
    group_by(response_variable, moderator) %>%
    summarise(
      missing_proportion = mean(is.na(value), na.rm = TRUE)
    )
}

# Function to plot missing data proportions per response variable
plot_missing_proportions <- function(original_data, imputed_data, moderators, dataset_name) {
  cat("\nStarting plot creation for", dataset_name, "...\n")
  
  # Calculate missing proportions for original and imputed datasets
  missing_original <- calculate_missing_proportions(original_data, moderators) %>%
    mutate(data_source = "Original")
  
  missing_imputed <- calculate_missing_proportions(imputed_data, moderators) %>%
    mutate(data_source = "Imputed")
  
  # Combine the results
  combined_missing <- bind_rows(missing_original, missing_imputed)
  
  # Create the plot
  plot <- ggplot(combined_missing, aes(x = response_variable, y = missing_proportion, fill = data_source)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ moderator, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = paste("Proportion of Missing Data per Response Variable -", dataset_name),
      x = "Response Variable",
      y = "Missing Proportion"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  
  cat("\nPlot creation completed for", dataset_name, ".\n")
  
  return(plot)
}

# List of moderators
moderators <- c("tree_type", "crop_type", "age_system", "season", 
                "soil_texture", "no_tree_per_m", "tree_height", "alley_width")

# Create plots for Non-Imputed and Imputed datasets
plot_non_imp <- plot_missing_proportions(non_imp_dataset, non_imp_dataset_imputed, moderators, "Non-Imputed Dataset")
plot_imp <- plot_missing_proportions(imp_dataset, imp_dataset_imputed, moderators, "Imputed Dataset")

# Display the plots side by side
plot_non_imp + plot_imp
```


Perform imputation using "mice" (Multivariate Imputation by Chained Equations)

```{r, eval = TRUE}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

############################################################################################################################
# Set seed for reproducibility
set.seed(1234)

# Perform imputation using mice
# - read about mice() here: https://www.metafor-project.org/doku.php/tips:multiple_imputation_with_mice_and_metafor
# - col_for_impute: the data frame containing the columns to be imputed
# - m = 5: number of multiple imputations to perform
# - maxit = 100: maximum number of iterations to perform for each imputation
# - method = 'pmm': method to use for imputation, 'pmm' stands for predictive mean matching
# - seed = 500: random seed for reproducibility of the imputations
# - printFlag: If TRUE, mice will print history on console. Use print=FALSE for silent computation.



# Step 1: Check and enforce correct data types
col_for_impute <- database_clean_sd |> 
  as.data.frame() |> 
  select(-geometry) |> 
  select(
    # Columns that need to be imputed
    silvo_se, control_se, silvo_n, control_n,
    # Columns that are used by mice to impute values
    tree_age, crop_type, tree_type, sub_region, experiment_year, alley_width,
    # IDs that are used to back-link imputed values to the dataset
    id_article, id_obs, treat_id, exp_id
  ) |> 
  # Convert relevant columns to the correct data types
  mutate(
    silvo_se = as.numeric(silvo_se),
    control_se = as.numeric(control_se),
    silvo_n = as.numeric(silvo_n),
    control_n = as.numeric(control_n),
    tree_age = as.numeric(tree_age),
    crop_type = as.factor(crop_type),
    tree_type = as.factor(tree_type),
    sub_region = as.factor(sub_region),
    alley_width = as.factor(alley_width),
    id_article = as.numeric(id_article),
    id_obs = as.numeric(id_obs),
    treat_id = as.numeric(treat_id),
    exp_id = as.numeric(exp_id)
  )

# Step 2: Define the predictor matrix
pred_matrix <- mice::make.predictorMatrix(col_for_impute)

# Allow only specific columns to be imputed
# Set all columns except 'silvo_se', 'control_se', 'silvo_n', and 'control_n' to be non-imputed
pred_matrix[, c("tree_age", "crop_type", "tree_type", "sub_region", "experiment_year", "alley_width", "id_article", "id_obs", "treat_id", "exp_id")] <- 0

# Step 3: Update the method vector to specify imputation only for target columns
# Use 'pmm' (predictive mean matching) for numeric columns to be imputed and "" for others
method <- c(
  "silvo_se" = "pmm",
  "control_se" = "pmm",
  "silvo_n" = "pmm",
  "control_n" = "pmm",
  "tree_age" = "",           # Not imputed
  "crop_type" = "",          # Not imputed
  "tree_type" = "",          # Not imputed
  "sub_region" = "",         # Not imputed
  "experiment_year" = "",    # Not imputed
  "alley_width" = "",        # Not imputed
  "id_article" = "",         # Not imputed
  "id_obs" = "",             # Not imputed
  "treat_id" = "",           # Not imputed
  "exp_id" = ""              # Not imputed
)

# Step 4: Perform imputation using mice
set.seed(1234)
imputed_data <- mice(
  col_for_impute,
  m = 20,
  maxit = 100,
  method = method,
  predictorMatrix = pred_matrix,
  seed = 1234,
  printFlag = FALSE
)

# Step 5: Extract a completed dataset for inspection
completed_data <- mice::complete(imputed_data, 1)
print(head(completed_data))
# Step 5: Extract the completed data
completed_data <- mice::complete(imputed_data)

##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (16/11-24)
# Time difference of 14.25402 secs
```

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################

# Start time tracking
start.time <- Sys.time()

##########################################################################
# Step 1: Check and enforce correct data types
col_for_impute <- database_clean_sd |> 
  as.data.frame() |> 
  select(-geometry) |> 
  select(
    # Columns that need to be imputed
    silvo_se, control_se, silvo_n, control_n,
    # Columns that are used by mice to impute values
    tree_age, crop_type, tree_type, sub_region, experiment_year, alley_width,
    # IDs that are used to back-link imputed values to the dataset
    id_article, id_obs, treat_id, exp_id
  ) |> 
  mutate(
    silvo_se = as.numeric(silvo_se),
    control_se = as.numeric(control_se),
    silvo_n = as.numeric(silvo_n),
    control_n = as.numeric(control_n),
    tree_age = as.numeric(tree_age),
    crop_type = as.factor(crop_type),
    tree_type = as.factor(tree_type),
    sub_region = as.factor(sub_region),
    alley_width = as.factor(alley_width),
    id_article = as.numeric(id_article),
    id_obs = as.numeric(id_obs),
    treat_id = as.numeric(treat_id),
    exp_id = as.numeric(exp_id)
  )

##########################################################################
# Step 2: Define the function for each imputation method
impute_data <- function(data, method_name) {
  if (method_name == "pmm") {
    # Predictive Mean Matching
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "sub_region", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0
    
    # Define imputation method for PMM
    method <- c(
      "silvo_se" = "pmm",
      "control_se" = "pmm",
      "silvo_n" = "pmm",
      "control_n" = "pmm",
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "sub_region" = "",         # Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",         # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    # Perform imputation using mice
    imputed_data <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(mice::complete(imputed_data))
    
  } else if (method_name == "upper_quartile") {
    # Upper Quartile Imputation for Variance
    upper_quartile_variance <- data %>%
      summarise(across(c(silvo_se, control_se), ~ quantile(.^2, 0.75, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "upper_quartile")
    
    # Impute missing variance with the upper quartile
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), sqrt(upper_quartile_variance$upper_quartile[1]), silvo_se),
        control_se = ifelse(is.na(control_se), sqrt(upper_quartile_variance$upper_quartile[2]), control_se)
      )
    return(data)
    
  } else if (method_name == "mean_imputation") {
    # Example: Mean Imputation
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), mean(silvo_se, na.rm = TRUE), silvo_se),
        control_se = ifelse(is.na(control_se), mean(control_se, na.rm = TRUE), control_se),
        silvo_n = ifelse(is.na(silvo_n), mean(silvo_n, na.rm = TRUE), silvo_n),
        control_n = ifelse(is.na(control_n), mean(control_n, na.rm = TRUE), control_n)
      )
    return(data)
  } else {
    stop("Invalid method name.")
  }
}

##########################################################################
# Step 3: Apply each imputation method
imputation_methods <- c("pmm", "upper_quartile", "mean_imputation")
imputed_datasets <- list()

for (method_name in imputation_methods) {
  cat("Applying", method_name, "imputation...\n")
  imputed_datasets[[method_name]] <- impute_data(col_for_impute, method_name)
}

##########################################################################
# Step 4: Compare results
for (method_name in imputation_methods) {
  cat("\nSummary of Imputed Dataset -", method_name, ":\n")
  print(summary(imputed_datasets[[method_name]]))
}

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")
##########################################################################

```

```{r}
# Step 1: Extract observed values for 'silvo_se'
# - `col_for_impute` is the data frame containing the columns to be imputed.
# - We filter out the non-missing values from the original 'silvo_se' column.
observed_silvo_se <- col_for_impute$silvo_se[!is.na(col_for_impute$silvo_se)]

# Step 2: Extract imputed values for 'silvo_se'
# - We use `lapply()` to loop over all 20 imputed datasets generated by `mice`.
# - `mice::complete(imputed_data, i)` extracts the completed dataset for the i-th imputation.
# - `data$silvo_se[is.na(col_for_impute$silvo_se)]` selects the imputed values where the original data had missing values.
# - `unlist()` is used to flatten the list of imputed values into a vector.
imputed_silvo_se <- unlist(lapply(1:20, function(i) {
  data <- mice::complete(imputed_data, i) # Extract the i-th imputed dataset
  data$silvo_se[is.na(col_for_impute$silvo_se)] # Select only the imputed values
}))

# Step 3: Create a combined data frame for plotting
# - `value`: A combined vector of both observed and imputed values.
# - `type`: A vector indicating whether the value is "Original" (observed) or "Imputed".
# - `rep()`: Repeats the labels for the respective lengths of observed and imputed values.
plot_data <- data.frame(
  value = c(observed_silvo_se, imputed_silvo_se),
  type = c(rep("Original", length(observed_silvo_se)),
           rep("Imputed", length(imputed_silvo_se)))
)

# Step 4: Plot the density of observed vs. imputed values using ggplot2
ggplot(plot_data, aes(x = value, fill = type)) +
  # `geom_density()`: Plots the density curve for each type ("Original" and "Imputed").
  # `alpha = 0.5`: Sets the transparency of the density curves (0 = fully transparent, 1 = fully opaque).
  geom_density(alpha = 0.5) +
  # `labs()`: Adds titles and labels to the plot.
  labs(
    title = "Density Plot of Original vs. Imputed Values for silvo_se",
    x = "silvo_se Values",
    y = "Density"
  ) +
  # `scale_fill_manual()`: Manually sets the colors for the fill based on the "type" variable.
  # - "blue" for the "Original" values and "red" for the "Imputed" values.
  scale_fill_manual(values = c("blue", "red")) +
  # `scale_x_log10()`: Applies a log10 transformation to the x-axis (silvo_se values).
  # - This transformation helps visualize the data if there is a large range or skewness.
  scale_x_log10() +
  # vertical lines indicating the mean or median of each group
  geom_vline(aes(xintercept = mean(observed_silvo_se)), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(imputed_silvo_se)), color = "red", linetype = "dashed") +
  # `theme_minimal()`: Uses a minimal theme for a clean look.
  theme_minimal() +
  # `theme()`: Customizes the appearance of the plot.
  theme(
    legend.title = element_text(size = 10), # Sets the font size for the legend title
    legend.position = "top" # Places the legend at the top of the plot
  )
```

```{r}
# Step 1: Initialize a list to store summaries of all imputations
imputed_summaries <- list()

# Loop through all 20 imputed datasets in the mids object
for (i in 1:20) {
  data <- mice::complete(imputed_mids_pmm, i) # Extract the i-th imputed dataset
  
  # Calculate summary statistics for each imputation
  summary <- data %>%
    summarise(
      mean_silvo_se = mean(silvo_se, na.rm = TRUE),
      sd_silvo_se = sd(silvo_se, na.rm = TRUE),
      mean_control_se = mean(control_se, na.rm = TRUE),
      sd_control_se = sd(control_se, na.rm = TRUE)
    )
  
  # Store the summary in the list
  imputed_summaries[[i]] <- summary
}

# Step 2: Combine summaries into a single data frame
imputed_summaries_df <- bind_rows(imputed_summaries, .id = "imputation")

# Step 3: Calculate the median values for both silvo_se and control_se
median_silvo_se <- median(imputed_summaries_df$mean_silvo_se)
median_control_se <- median(imputed_summaries_df$mean_control_se)

# Add a column calculating the Euclidean distance from the median for both silvo_se and control_se
imputed_summaries_df <- imputed_summaries_df %>%
  mutate(
    distance_from_median = sqrt(
      (mean_silvo_se - median_silvo_se)^2 + (mean_control_se - median_control_se)^2
    )
  )

# Step 4: Choose the imputation with the smallest distance
chosen_imputation <- imputed_summaries_df %>%
  slice(which.min(distance_from_median))

# Print the chosen imputation
cat("Chosen imputation based on combined proximity to medians of silvo_se and control_se:\n")
print(chosen_imputation)

# Step 5: Extract the complete dataset corresponding to the chosen imputation
chosen_imputation_number <- as.integer(chosen_imputation$imputation)
imputed_col_data <- mice::complete(imputed_mids_pmm, chosen_imputation_number)

# Check the structure of the chosen imputation dataset
cat("\nStructure of the chosen imputed dataset:\n")
glimpse(imputed_col_data)

# Step 6: Add the chosen imputation to the imputed_datasets list
imputed_datasets$pmm_best <- imputed_col_data
```




```{r}
# Update the original data with imputed values
# Step 1: Join the imputed values back to the original dataset using identifiers
imp_dataset <- database_clean_sd %>%
  left_join(
    completed_data %>%
      select(id_article, id_obs, silvo_se, control_se),
    by = c("id_article", "id_obs"),
    suffix = c("_original", "_imputed")
  )|> 
  as.data.frame() |> 
  select(-geometry)

# Step 2: Replace missing values in the original columns with imputed values
imp_dataset <- imp_dataset %>%
  mutate(
    silvo_se = ifelse(is.na(silvo_se_original), silvo_se_imputed, silvo_se_original),
    control_se = ifelse(is.na(control_se_original), control_se_imputed, control_se_original)
  ) %>%
  select(-silvo_se_original, -silvo_se_imputed, -control_se_original, -control_se_imputed)

imp_dataset
```


Visualising the distribution of imputed values for silvo_se and control_se together with the original data for the chosen imputation

```{r}
# Prepare the original data
original_data_x <- database_clean_sd %>%
  select(id_article, id_obs, response_variable, silvo_se, control_se) |> 
  mutate(data_source = "Original") |> 
  as.data.frame() 

imputed_data_y <- imp_dataset |> 
  select(id_article, id_obs, response_variable, silvo_se, control_se) |> 
  mutate(data_source = "Imputed") 

# Combine the original and imputed data
combined_data <- bind_rows(original_data_x, imputed_data_y)

combined_data
```


```{r}
# Join the original and imputed data to directly compare
comparison_data <- original_data_x %>%
  full_join(imputed_data_y, by = c("id_article", "response_variable"), suffix = c("_original", "_imputed")) %>%
  distinct()
# Advarsel: Detected an unexpected many-to-many relationship between `x` and `y`

# Identify rows where imputation occurred by checking if originally missing values are filled
imputation_evaluation <- comparison_data %>%
  filter(
    (is.na(silvo_se_original) & !is.na(silvo_se_imputed)) |
      (is.na(control_se_original) & !is.na(control_se_imputed))
  ) %>%
  select(id_article, response_variable) %>%
  distinct()

# Count the number of unique articles where imputation occurred
n_imputed_studies <- imputation_evaluation %>%
  distinct(id_article) %>%
  nrow()

# Output the results
imputation_evaluation
n_imputed_studies
```
```{r}
imputation_summary <- comparison_data %>%
  summarise(
    total_missing = sum(is.na(silvo_se_original) & !is.na(silvo_se_imputed)),
    total_imputed = sum(!is.na(silvo_se_imputed)),
    proportion_imputed = total_missing / total_imputed
  )

imputation_summary
```
```{r}
# Q-Q plot for silvo_se
qqplot_silvo_se <- ggplot(combined_data, aes(sample = silvo_se)) +
  stat_qq(aes(color = data_source)) +
  stat_qq_line(aes(color = data_source)) +
  ggtitle("Q-Q Plot of Original vs. Imputed silvo_se") +
  theme_minimal()

qqplot_silvo_se
```


```{r}
# Create density plots for silvo_se with log transformation
silvo_se_impute_original_plot <- combined_data |> 
  ggplot(aes(x = silvo_se, color = data_source)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Density Distribution of silvo_se (Log-Transformed)") +
  theme_minimal()

# Create density plots for control_se with log transformation
control_se_impute_original_plot <- combined_data |> 
  ggplot(aes(x = control_se, color = data_source)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Density Distribution of control_se (Log-Transformed)") +
  theme_minimal()

library(patchwork)

silvo_se_impute_original_plot + control_se_impute_original_plot
```



















```{r}
# Q-Q plot for silvo_se
qqplot_silvo_se <- ggplot(combined_data, aes(sample = silvo_se)) +
  stat_qq(aes(color = data_source)) +
  stat_qq_line(aes(color = data_source)) +
  ggtitle("Q-Q Plot of Original vs. Imputed silvo_se") +
  theme_minimal()

qqplot_silvo_se
```


```{r}
# Create density plots for silvo_se with log transformation
silvo_se_impute_original_plot <- combined_data |> 
  ggplot(aes(x = silvo_se, color = data_source)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Density Distribution of silvo_se (Log-Transformed)") +
  theme_minimal()

# Create density plots for control_se with log transformation
control_se_impute_original_plot <- combined_data |> 
  ggplot(aes(x = control_se, color = data_source)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Density Distribution of control_se (Log-Transformed)") +
  theme_minimal()

library(patchwork)

silvo_se_impute_original_plot + control_se_impute_original_plot
```











Comparing and evaluating the two meta-data sets (imputed vs. non-imputed)

```{r}
# Create a combined dataset for comparison
comparison_data <- non_imp_data_rom %>%
  select(id_article, id_obs, response_variable, yi_non_imp = yi, vi_non_imp = vi) %>%
  left_join(imp_data_rom %>%
              select(id_article, id_obs, yi_imp = yi, vi_imp = vi),
            by = c("id_article", "id_obs"))

# Scatter plot of effect sizes
scatter_plot <- ggplot(comparison_data, aes(x = yi_non_imp, y = yi_imp)) +
  geom_point(alpha = 0.6, color = "#0072B2") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Comparison of Effect Sizes (ROM): Imputed vs. Non-Imputed",
       x = "Effect Size (Non-Imputed Data)", y = "Effect Size (Imputed Data)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

scatter_plot
```

Paired t-test for Effect Sizes (yi)
A paired t-test can help assess if there is a significant difference between the effect sizes of the imputed and non-imputed datasets.

```{r}
clean_data <- comparison_data %>%
  filter(!is.na(yi_non_imp), !is.na(yi_imp), 
         !is.infinite(yi_non_imp), !is.infinite(yi_imp)) %>%
  mutate(diff = yi_imp - yi_non_imp)

summary(clean_data$diff)
```

Bland-Altman Analysis
A Bland-Altman plot can provide a graphical method to assess agreement between the two datasets by plotting the differences against the means.

```{r}
# Calculate the mean and difference of effect sizes
comparison_data <- comparison_data %>%
  mutate(
    mean_yi = (yi_non_imp + yi_imp) / 2,
    diff_yi = yi_imp - yi_non_imp
  )

# Create a Bland-Altman plot
bland_altman_plot <- ggplot(comparison_data, aes(x = mean_yi, y = diff_yi)) +
  geom_point(alpha = 0.6, color = "#0072B2") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Bland-Altman Plot: Imputed vs. Non-Imputed Effect Sizes",
       x = "Mean Effect Size", y = "Difference in Effect Size (Imputed - Non-Imputed)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(bland_altman_plot)

```

Correlation Analysis
Calculate the correlation between the effect sizes from the imputed and non-imputed datasets.
```{r}
# Calculate Pearson and Spearman correlations
# Calculate Pearson and Spearman correlations
pearson_corr <- cor(comparison_data$yi_non_imp, comparison_data$yi_imp, method = "pearson")
spearman_corr <- cor(comparison_data$yi_non_imp, comparison_data$yi_imp, method = "spearman")

# Print the correlation results
cat("Pearson Correlation:", pearson_corr, "\n")
cat("Spearman Correlation:", spearman_corr, "\n")

```

Pearson correlation measures the linear relationship between the two sets of effect sizes. A value close to 1 indicates strong linear agreement.
Spearman correlation measures the rank correlation, providing a non-parametric measure of the relationship. This is useful if the data has outliers or is not normally distributed.


Mean Absolute Difference (MAD) and Root Mean Square Error (RMSE)
These metrics provide an indication of the overall difference between the two sets of effect sizes.

```{r}
# Calculate Mean Absolute Difference (MAD)
mad <- mean(abs(comparison_data$yi_imp - comparison_data$yi_non_imp), na.rm = TRUE)

# Calculate Root Mean Square Error (RMSE)
rmse <- sqrt(mean((comparison_data$yi_imp - comparison_data$yi_non_imp)^2, na.rm = TRUE))

# Print the results
cat("Mean Absolute Difference (MAD):", mad, "\n")
cat("Root Mean Square Error (RMSE):", rmse, "\n")

```























##########################################################################################################################################
CREATING A VARIANCE-COVARIANCE MATRIX
##########################################################################################################################################

Creating a variance-covariance matrix is crucial in multivariate meta-analysis because it captures the dependencies among the effect sizes from different outcomes measured within the same study. Without accounting for these dependencies, the analysis could be biased and less efficient.

Why a Variance-Covariance Matrix is Needed
- Account for Within-Study Correlations: When multiple outcomes are reported within the same study, they are often correlated. Ignoring these correlations can lead to inaccurate estimates of the overall effect size and its variance.
- Borrowing Strength: The variance-covariance matrix allows the analysis to borrow strength across different outcomes, leading to more precise estimates.
- Improve Model Accuracy: Including the correct variance-covariance structure improves the accuracy of the random-effects model, leading to better inference.

```{r}
# Function to calculate the variance-covariance matrix for a given dataset
calculate_v_matrix <- function(data, correlation = 0.5) {
  cat("\nCalculating Variance-Covariance Matrix...\n")
  
  # Initialize an empty list to store the variance-covariance matrices for each study
  v_list <- list()
  
  # Loop through each unique study ID
  for (study in unique(data$id_article)) {
    # Subset the data for the current study
    study_data <- data[data$id_article == study, ]
    
    # Check if the study has more than one outcome
    if (nrow(study_data) > 1) {
      # Create a diagonal matrix of variances
      v <- diag(study_data$vi)
      
      # Set the off-diagonal elements assuming a constant correlation
      for (i in 1:nrow(v)) {
        for (j in 1:nrow(v)) {
          if (i != j) {
            v[i, j] <- correlation * sqrt(v[i, i] * v[j, j])
          }
        }
      }
      
      # Store the matrix in the list
      v_list[[as.character(study)]] <- v
    } else {
      # For single outcome studies, use the variance directly
      v_list[[as.character(study)]] <- matrix(study_data$vi, nrow = 1, ncol = 1)
    }
  }
  
  # Combine all matrices into a block-diagonal matrix
  V_matrix <- bldiag(v_list)
  cat("Variance-Covariance Matrix Calculation Complete.\n")
  
  return(V_matrix)
}
```



##########################################################################################################################################
MODEL FITTING
##########################################################################################################################################

Fitting the multivariate random-effects model on both moderator-imputed and non-moderator-imputed datasets

Applying model fitting process on both datasets (non_imp_dataset and imp_dataset)

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################


# Updated function to fit the multivariate random-effects model using V_matrix
fit_meta_model <- function(data, dataset_name, V_matrix) {
  cat("\nStarting model fitting for", dataset_name, "...\n")
  
  # Define the formula for moderators
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture", "no_tree_per_m", "tree_height", "alley_width")
  moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  
  # Prepare the data
  data <- data %>%
    mutate(across(all_of(moderators), as.factor)) %>%
    as.data.frame()
  
  # Fit the multivariate random-effects model
  model <- tryCatch({
    rma.mv(
      yi = yi,                             # Fixed effect
      V = V_matrix,
      mods = moderator_formula,
      random = list(                       # Random effects $ ssh -T git@github.com
        ~ 1 | id_article,
        ~ 1 | id_article/response_variable,
        ~ 1 | exp_id
      ),
      data = data,
      method = "ML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", dataset_name, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Model fitting completed for", dataset_name, ".\n")
    
    # Extract model statistics
    aic <- AIC(model)
    bic <- BIC(model)
    logLik_val <- logLik(model)
    
    # Calculate I² (heterogeneity)
    tau2 <- sum(model$sigma2)
    sigma2 <- mean(data$vi)
    I2 <- (tau2 / (tau2 + sigma2)) * 100
    
    # Create a summary list
    model_summary <- list(
      model = model,
      aic = aic,
      bic = bic,
      logLik = logLik_val,
      I2 = I2
    )
    
    return(model_summary)
  } else {
    return(NULL)
  }
}


# List of datasets and names
datasets <- list(
  non_imp_dataset = non_imp_dataset,
  imp_dataset = imp_dataset
)

# Calculate the variance-covariance matrix for each dataset
V_matrices <- lapply(names(datasets), function(dataset_name) {
  calculate_v_matrix(datasets[[dataset_name]], correlation = 0.5)
})
names(V_matrices) <- names(datasets)

# Fit the model on all datasets using the calculated V_matrices
model_results <- lapply(names(datasets), function(dataset_name) {
  fit_meta_model(datasets[[dataset_name]], dataset_name, V_matrices[[dataset_name]])
})
names(model_results) <- names(datasets)

# Function to extract and compile model statistics
extract_model_summary <- function(model_summary, dataset_name) {
  if (is.null(model_summary)) {
    return(data.frame(
      Dataset = dataset_name,
      AIC = NA,
      BIC = NA,
      LogLikelihood = NA,
      I2 = NA
    ))
  }
  
  data.frame(
    Dataset = dataset_name,
    AIC = model_summary$aic,
    BIC = model_summary$bic,
    LogLikelihood = model_summary$logLik,
    I2 = model_summary$I2
  )
}

# Compile the model summaries into a single data frame
model_summaries <- bind_rows(
  extract_model_summary(model_results$non_imp_dataset, "Non-Imputed Dataset"),
  extract_model_summary(model_results$imp_dataset, "Imputed Dataset")
)

# Print the combined summary table
model_summaries

# Save the summary table
# write.csv(model_summaries, file = "model_summaries.csv")


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (17/11-24)
# Time difference of 3.211481 mins

# str(model_results)
```










# Function to fit meta-analytic model for a specific subgroup
fit_subgroup_model <- function(data, subgroup_name, V_matrix, moderators = NULL) {
  cat("\nFitting model for subgroup:", subgroup_name, "...\n")
  
  # Define the moderator formula
  moderator_formula <- if (!is.null(moderators)) {
    as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  } else {
    as.formula("yi ~ 1")  # Intercept-only model if no moderators
  }
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      yi = yi,
      V = V_matrix,
      mods = moderator_formula,
      random = list(~ 1 | id_article, ~ 1 | id_article/response_variable, ~ 1 | exp_id),
      data = data,
      method = "ML",
      control = list(optimizer = "optim", optim.method = "BFGS", iter.max = 1000, rel.tol = 1e-8)
    )
  }, error = function(e) {
    cat("Error fitting model for subgroup:", subgroup_name, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Model fitting completed for subgroup:", subgroup_name, ".\n")
    # Extract statistics
    return(list(
      subgroup = subgroup_name,
      aic = AIC(model),
      bic = BIC(model),
      logLik = logLik(model),
      tau2 = sum(model$sigma2),  # Variance components
      model = model
    ))
  } else {
    return(NULL)
  }
}

# Function to run subgroup analyses
run_subgroup_analysis <- function(data, V_matrix, split_var, moderators = NULL) {
  # Split data into subgroups
  subgroups <- split(data, data[[split_var]])
  
  # Fit a model for each subgroup
  results <- map(names(subgroups), ~ {
    fit_subgroup_model(subgroups[[.x]], .x, V_matrix, moderators)
  })
  
  # Compile results
  results <- results[!sapply(results, is.null)]  # Remove failed models
  summary_df <- bind_rows(lapply(results, function(res) {
    data.frame(
      Subgroup = res$subgroup,
      AIC = res$aic,
      BIC = res$bic,
      LogLikelihood = as.numeric(res$logLik),
      Tau2 = res$tau2
    )
  }))
  
  list(results = results, summary = summary_df)
}

# Define dataset and variables
meta_dataset <- imp_data_rom  # Example dataset
response_variable <- "response_variable"  # Column to split by
moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")  # Example moderators

# Generate the variance-covariance matrix (replace with your function)
V_matrix <- calculate_v_matrix(meta_dataset, correlation = 0.5)

# Run subgroup analysis
subgroup_analysis_results <- run_subgroup_analysis(meta_dataset, V_matrix, response_variable, moderators)

# Display subgroup summary
print(subgroup_analysis_results$summary)

# Save results for each subgroup
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

lapply(subgroup_analysis_results$results, function(res) {
  if (!is.null(res)) {
    saveRDS(res$model, file = file.path(output_dir, paste0("subgroup_", res$subgroup, "_model.rds")))
  }
})


```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- imp_data_rom

# Subgroup levels (e.g., unique response variables)
subgroups <- unique(data$response_variable)

# Fit Overall Random-Effects Model
overall_model <- rma(yi, vi, data = data)

# Helper function for Q-test, I², and τ² information
mlabfun <- function(text, x) {
  list(bquote(paste(
    .(text), " (Q = ", .(formatC(x$QE, digits = 2, format = "f")),
    ", df = ", .(x$k - x$p), ", ", .(format.pval(x$QEp, digits = 2)), "; ",
    I^2, " = ", .(formatC(x$I2, digits = 1, format = "f")), "%, ",
    tau^2, " = ", .(formatC(x$tau2, digits = 2, format = "f")), ")"
  )))
}

# Prepare the supplementary information
ilab_data <- cbind(
  "Response Variable" = data$response_variable,
  "Metric" = data$measured_metrics,
  "Silvo N" = data$silvo_n,
  "Control N" = data$control_n
)

# Forest plot with `ilab`
forest(
  overall_model,
  xlim = c(-4, 2),
  at = log(c(0.1, 0.5, 1, 2)), 
  atransf = exp,
  ilab = ilab_data,  # Add the supplementary data
  ilab.xpos = c(-6, -4, -2, -1),  # Adjust column positions
  cex = 0.8,
  ylim = c(-2, 5 + length(data$yi)), 
  top = 2,
  mlab = mlabfun("Random-Effects Model for All Data", overall_model),
  header = c("Subgroup and Study", "Effect Size [95% CI]")
)

# Add labels for ilab columns
text(c(-6, -4, -2, -1), max(data$yi) + 2, 
     c("Response Variable", "Metric", "Silvo N", "Control N"), pos = 4, font = 2)



# Subgroup Row Positions
row_positions <- cumsum(sapply(subgroups, function(sg) sum(data$response_variable == sg)))
row_positions <- c(1, row_positions + 1)  # Adjust for spacing

# Add Subgroup Models and Summary Polygons
for (i in seq_along(subgroups)) {
  subgroup <- subgroups[i]
  
  # Fit random-effects model for the subgroup
  subgroup_model <- rma(
    yi, vi, data = data, subset = (response_variable == subgroup)
  )
  
  # Add summary polygon for the subgroup
  addpoly(
    subgroup_model, row = row_positions[i], 
    mlab = mlabfun(paste("Random-Effects Model for", subgroup), subgroup_model)
  )
  
  # Add text for subgroup
  text(-4, row_positions[i] + 1, pos = 4, subgroup, font = 4)
}

# Test for Subgroup Differences (Meta-Regression Model)
subgroup_test <- rma(yi, vi, mods = ~ response_variable, data = data)

# Add Test for Subgroup Differences
text(-4, -1.5, pos = 4, cex = 0.75, bquote(paste(
  "Test for Subgroup Differences: ",
  Q[M], " = ", .(formatC(subgroup_test$QM, digits = 2, format = "f")),
  ", df = ", .(subgroup_test$p - 1), ", ", .(format.pval(subgroup_test$QMp, digits = 2))
)))

```

```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- imp_data_rom

# Select only specific subgroups
selected_subgroups <- c("Biodiversity", "Crop yield", "Soil quality")
filtered_data <- data %>% filter(response_variable %in% selected_subgroups)

# Subgroup levels (filtered)
subgroups <- unique(filtered_data$response_variable)

# Fit Overall Random-Effects Model for the filtered dataset
overall_model <- rma(yi, vi, data = filtered_data)

# Helper function for Q-test, I², and τ² information
mlabfun <- function(text, x) {
  list(bquote(paste(
    .(text), " (Q = ", .(formatC(x$QE, digits = 2, format = "f")),
    ", df = ", .(x$k - x$p), ", ", .(format.pval(x$QEp, digits = 2)), "; ",
    I^2, " = ", .(formatC(x$I2, digits = 1, format = "f")), "%, ",
    tau^2, " = ", .(formatC(x$tau2, digits = 2, format = "f")), ")"
  )))
}

# Prepare supplementary data for ilab
ilab_data <- cbind(
  "Silvo N" = filtered_data$silvo_n,
  "Control N" = filtered_data$control_n
)

# Forest Plot for the Filtered Data
forest(
  overall_model,
  xlim = c(-4, 2),
  at = log(c(0.1, 0.5, 1, 2)), 
  atransf = exp,
  ilab = ilab_data,  # Add the supplementary data
  ilab.xpos = c(-2, -1),  # Adjust column positions for supplementary info
  cex = 0.8,
  ylim = c(-2, 3 + length(subgroups) * 2),  # Adjust ylim for fewer elements
  top = 2,
  mlab = mlabfun("Random-Effects Model for Selected Subgroups", overall_model),
  header = c("Subgroup and Study", "Effect Size [95% CI]")
)

# Add labels for ilab columns
text(c(-2, -1), max(filtered_data$yi) + 2, 
     c("Silvo N", "Control N"), pos = 4, font = 2)

# Row positions for subgroup summary polygons
row_positions <- seq(1, by = 2, length.out = length(subgroups))

# Add Subgroup Summary Polygons
for (i in seq_along(subgroups)) {
  subgroup <- subgroups[i]
  
  # Fit random-effects model for each subgroup
  subgroup_model <- rma(
    yi, vi, data = filtered_data, subset = (response_variable == subgroup)
  )
  
  # Add summary polygon for the subgroup
  addpoly(
    subgroup_model, row = row_positions[i], 
    mlab = mlabfun(paste("Random-Effects Model for", subgroup), subgroup_model)
  )
  
  # Add subgroup label
  text(-4, row_positions[i] + 1, pos = 4, subgroup, font = 4)
}

# Test for Subgroup Differences (Meta-Regression Model)
subgroup_test <- rma(yi, vi, mods = ~ response_variable, data = filtered_data)

# Add Test for Subgroup Differences
text(-4, -1.5, pos = 4, cex = 0.75, bquote(paste(
  "Test for Subgroup Differences: ",
  Q[M], " = ", .(formatC(subgroup_test$QM, digits = 2, format = "f")),
  ", df = ", .(subgroup_test$p - 1), ", ", .(format.pval(subgroup_test$QMp, digits = 2))
)))

```
```{r}
# Filter the dataset to include only selected subgroups
selected_subgroups <- c("Biodiversity", "Crop yield", "Soil quality", "Greenhouse gas emission")
filtered_data <- imp_data_rom %>% filter(response_variable %in% selected_subgroups)

# Unique subgroup levels
subgroups <- unique(filtered_data$response_variable)

# Fit the overall random-effects model
overall_model <- rma(yi, vi, data = filtered_data)

# Helper function for Q-test, I², and τ²
mlabfun <- function(text, x) {
  list(bquote(paste(
    .(text), " (Q = ", .(formatC(x$QE, digits = 2, format = "f")),
    ", df = ", .(x$k - x$p), ", ", .(format.pval(x$QEp, digits = 2)), "; ",
    I^2, " = ", .(formatC(x$I2, digits = 1, format = "f")), "%, ",
    tau^2, " = ", .(formatC(x$tau2, digits = 2, format = "f")), ")"
  )))
}

# Prepare supplementary data for the ilab columns
ilab_data <- cbind(
  "Silvo N" = filtered_data$silvo_n,
  "Control N" = filtered_data$control_n
)

# Set plot limits based on data
plot_rows <- length(filtered_data$yi) + length(subgroups) * 2  # Add space for subgroups

# Create the forest plot
forest(
  overall_model,
  xlim = c(-4, 2),                           # Horizontal axis limits
  at = log(c(0.1, 0.5, 1, 2)),               # Tick marks for log scale
  atransf = exp,                             # Back-transform log values
  ilab = ilab_data,                          # Add supplementary info
  ilab.xpos = c(-2, -1),                     # Position of ilab columns
  ylim = c(-2, plot_rows),                   # Vertical axis limits
  cex = 0.8,                                 # Font size for main plot
  top = 2,                                   # Extra space at the top
  mlab = mlabfun("Random-Effects Model for Selected Subgroups", overall_model),
  header = c("Subgroup and Study", "Effect Size [95% CI]")
)

# Add labels for ilab columns
text(c(-2, -1), plot_rows - 1, c("Silvo N", "Control N"), pos = 4, font = 2)

# Adjust row positions for subgroup summaries
row_positions <- seq(1, by = 3, length.out = length(subgroups))

# Add subgroup summaries and labels
for (i in seq_along(subgroups)) {
  subgroup <- subgroups[i]
  
  # Fit random-effects model for the subgroup
  subgroup_model <- rma(yi, vi, data = filtered_data, subset = (response_variable == subgroup))
  
  # Add summary polygon for the subgroup
  addpoly(
    subgroup_model,
    row = row_positions[i], 
    mlab = mlabfun(paste("Random-Effects Model for", subgroup), subgroup_model),
    cex = 0.8
  )
  
  # Add subgroup label
  text(-4, row_positions[i] + 1, pos = 4, subgroup, font = 4, cex = 0.9)
}

# Add test for subgroup differences
subgroup_test <- rma(yi, vi, mods = ~ response_variable, data = filtered_data)
text(-4, -1.5, pos = 4, cex = 0.8, bquote(paste(
  "Test for Subgroup Differences: ",
  Q[M], " = ", .(formatC(subgroup_test$QM, digits = 2, format = "f")),
  ", df = ", .(subgroup_test$p - 1), ", ", .(format.pval(subgroup_test$QMp, digits = 2))
)))

```

```{r}
# Filter data to include only subgroups with sufficient studies
min_studies <- 10
subgroup_counts <- table(imp_data_rom$response_variable)
selected_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
filtered_data <- imp_data_rom %>%
  filter(response_variable %in% selected_subgroups)

# Fit random-effects models for each subgroup and calculate summary statistics
subgroup_results <- filtered_data %>%
  group_by(response_variable) %>%
  summarise(
    model = list(rma(yi, vi, data = pick(everything()))),  # Use `pick()` instead of `cur_data()`
    .groups = "drop"
  ) %>%
  mutate(
    estimate = map_dbl(model, ~ coef(.x)["intrcpt"]),
    lower_ci = map_dbl(model, ~ confint(.x)$random[1, 1]),
    upper_ci = map_dbl(model, ~ confint(.x)$random[1, 2]),
    I2 = map_dbl(model, ~ .x$I2)
  )

# Add summary data back to the main dataset for plotting
filtered_data <- filtered_data %>%
  left_join(subgroup_results, by = "response_variable")

# Generate the forest plot using ggplot2
forest_plot <- ggplot() +
  # Add forest plot points and confidence intervals
  geom_pointrange(
    data = subgroup_results,
    aes(
      x = estimate, ymin = lower_ci, ymax = upper_ci,
      y = response_variable, color = response_variable
    ),
    size = 0.8
  ) +
  # Add ridge density plot for effect sizes
  geom_density_ridges(
    data = filtered_data,
    aes(x = yi, y = response_variable, fill = response_variable),
    alpha = 0.5, scale = 1.5, rel_min_height = 0.01
  ) +
  # Add vertical reference line at x = 1
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Custom colors for subgroups
  scale_color_viridis_d(option = "D", name = "Subgroups") +
  scale_fill_viridis_d(option = "D", guide = "none") +
  # Customize labels and theme
  labs(
    title = "Forest Plot with Subgroups and Ridge Density",
    x = "Effect Size (Log Scale)", y = "Subgroups",
    caption = "Random-effects models fitted for each subgroup"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

# Show the Forest Plot
forest_plot

```


```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- non_imp_data_dummy

# Filter subgroups with sufficient data points (e.g., at least 10 studies)
min_studies <- 10
subgroup_counts <- table(data$response_variable)
selected_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
filtered_data <- data %>% filter(response_variable %in% selected_subgroups)

# Initialize a list to store subgroup summaries
subgroup_summaries <- list()

# Fit random-effects model for each subgroup
for (subgroup in selected_subgroups) {
  model <- tryCatch(
    rma(yi, vi, data = filtered_data, subset = (response_variable == subgroup)), 
    error = function(e) NULL
  )
  
  if (!is.null(model)) {
    summary <- list(
      response_variable = subgroup,
      mean_effect = coef(model)["intrcpt"],
      conf_low = confint(model)$random["ci.lb"],
      conf_high = confint(model)$random["ci.ub"],
      I2 = model$I2,
      tau2 = model$tau2,
      n_studies = model$k
    )
  } else {
    summary <- list(
      response_variable = subgroup,
      mean_effect = NA,
      conf_low = NA,
      conf_high = NA,
      I2 = NA,
      tau2 = NA,
      n_studies = NA
    )
  }
  
  subgroup_summaries[[subgroup]] <- summary
}

# Combine summaries into a dataframe
summary_df <- do.call(rbind, lapply(subgroup_summaries, as.data.frame))

# Inspect the summary dataframe
print(summary_df)

# Plot the overall effect sizes for each subgroup
forest_plot <- ggplot(summary_df, aes(x = mean_effect, y = response_variable)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.2, color = "blue") +
  labs(
    title = "Subgroup Overall Effects",
    x = "Mean Effect Size (Log Scale)",
    y = "Subgroups"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

# Display the plot
print(forest_plot)

```






















########################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################################################################################################
MODEL FITTING FROM SCRATCH


Points to address after the Tshering meeting with her supervisor (26/11-2024)


########################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################


### Workflow Modifications and Notes Post-Meeting (26/11/2024)

#### Subgroup Analysis vs. Meta-Regression
**Key Decisions and Rationale:**
  - **Subgroup Analysis**: 
  - Keep separate subgroup analyses for each `response_variable` (e.g., biodiversity, crop yield) to respect conceptual differences.
- Allows us to focus on unique trends and moderators relevant to each outcome.
- Plan to include subgroup-specific moderators like `tree_type` and `crop_type` to explore targeted relationships.
- Acknowledge the limitations: potential for reduced power in smaller subgroups and the need to adjust for multiple comparisons (e.g., Bonferroni).

- **Meta-Regression**:
  - Use meta-regression as a complementary step, not a replacement, to capture cross-cutting trends.
- Include `response_variable` as a moderator to leverage the entire dataset, preserving power.
- Test interaction terms (e.g., `tree_type * response_variable`) to detect shared vs. outcome-specific patterns.
- Be mindful of complexity—flag variables or interactions that might make interpretation unwieldy.

#### Immediate Adjustments:
1. **Subgroup Models**:
  - Ensure models are well-documented to highlight their scope (e.g., biodiversity-specific vs. crop-yield-specific).
- Add notes to ensure consistent handling of moderators within subgroups (e.g., same scaling, inclusion criteria).
- Compare subgroup heterogeneity indices (`Q`, `I²`, `τ²`) to identify differences in variability across outcomes.

2. **Meta-Regression**:
  - Begin with a simpler meta-regression using `response_variable` as the sole moderator.
- Gradually add key moderators (e.g., `tree_type`, `crop_type`) and their interactions with `response_variable`.
- Consider whether subgroup findings are confirmed or contradicted by meta-regression results.
- Prepare visualizations that overlay subgroup-specific results with meta-regression trends.

3. **Heterogeneity Focus**:
  - Compare heterogeneity indices (I², τ²) between original and imputed datasets.
- Report changes in heterogeneity metrics after imputation to understand its influence on variability.

4. **Visualizations**:
  - Update forest plots to simplify presentation, particularly for subgroup models (e.g., one plot per response variable).
- Add funnel plots for individual response variable models and meta-regression.
- Use ggplot for standardized aesthetics, log scales (if needed), and cleaner annotations.

#### To-Do List for Next Steps:
- [ ] Refactor subgroup analysis workflow to make models more modular and reusable across response variables.
- [ ] Draft comparison table of heterogeneity indices across subgroups and the meta-regression.
- [ ] Pilot a meta-regression with key moderators and test its alignment with subgroup findings.
- [ ] Investigate trends in imputed vs. original data heterogeneity metrics.
- [ ] Review final visualizations with the team for clarity and presentation impact.

**Key Takeaway**: This combined workflow maximizes insights by leveraging the strengths of both subgroup analyses and meta-regression while mitigating their respective limitations.



```{r}

meta_data <- non_imp_dataset

base_model <- rma.mv(
  yi = yi, V = vi,
  random = ~ 1 | id_article/response_variable/exp_id, 
  data = meta_data,
  method = "REML",
  tdist = TRUE
)
summary(base_model)
```

```{r}
subgroups <- split(meta_data, meta_data$response_variable)


subgroup_models <- lapply(names(subgroups), function(subgroup) {
  subgroup_data <- subgroups[[subgroup]]
  tryCatch(
    rma.mv(
      yi = yi, V = vi,
      random = ~ 1 | id_article/exp_id,
      data = subgroup_data,
      method = "REML"
    ),
    error = function(e) {
      cat("Error for subgroup:", subgroup, "\n", e$message, "\n")
      return(NULL)
    }
  )
})

subgroups
```

```{r}
sapply(subgroup_models, function(model) {
  if (!is.null(model)) {
    list(
      Q = model$QE, 
      I2 = model$I2, 
      tau2 = model$sigma2
    )
  }
})
```

```{r}
lapply(subgroup_models, function(model) {
  if (!is.null(model)) forest(model, header = TRUE)
})
```
```{r}
funnel(base_model)
```

```{r}
leave_one_out <- function(data, model) {
  lapply(unique(data$id_article), function(article) {
    data_subset <- data[data$id_article != article, ]
    rma.mv(
      yi = yi, V = V_lnR_imputed,
      random = ~ 1 | id_article/exp_id,
      data = data_subset,
      method = "REML"
    )
  })
}
sensitivity_results <- leave_one_out(meta_data, base_model)

```

```{r}
# Summarize the data by response variable
summary_data <- meta_data %>%
  group_by(response_variable) %>%
  summarise(
    mean_yi = mean(yi, na.rm = TRUE),              # Mean effect size
    lower_ci = mean_yi - 1.96 * sqrt(mean(vi, na.rm = TRUE)), # Lower CI
    upper_ci = mean_yi + 1.96 * sqrt(mean(vi, na.rm = TRUE)), # Upper CI
    n = n()                                        # Number of studies
  ) %>%
  ungroup()


# Define custom color palette for response variables
custom_colors <- c(
  "Biodiversity" = "#FF9999",
  "Greenhouse gas emission" = "#66C266",
  "Product quality" = "#FFC000",
  "Crop yield" = "#FF9933",
  "Pest and Disease" = "#33CCCC",
  "Soil quality" = "#9966CC",
  "Water quality" = "#9999FF"
)

# Create the forest plot
# Updated forest plot with log scale and vertical line
forest_plot <- ggplot(summary_data, aes(x = mean_yi, y = response_variable)) +
  geom_point(aes(color = response_variable), size = 3) +  # Effect size points
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +  # Confidence intervals
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1) +  # Red dotted vertical line
  scale_x_continuous(
    name = "Effect Size (Log Response Ratio ± 95% CI)",
    breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)# Customize x-axis breaks
  ) +
  scale_color_manual(values = custom_colors) +  # Use custom color palette
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # Remove legend
    axis.text.y = element_text(size = 12, hjust = 1),  # Adjust y-axis text
    axis.title.x = element_text(size = 14),  # Larger font for x-axis title
    axis.title.y = element_text(size = 14)   # Larger font for y-axis title
  ) +
  labs(
    y = "Response Variable",
    title = "Summarized Forest Plot by Response Variable (Log Scale)"
  )



# Display the plot
forest_plot
```





#############
# STEP 1
##########################################################################################################################################
CREATING A VARIANCE-COVARIANCE MATRIX
##########################################################################################################################################

Creating a variance-covariance matrix is crucial in multivariate meta-analysis because it captures the dependencies among the effect sizes from different outcomes measured within the same study. Without accounting for these dependencies, the analysis could be biased and less efficient.

Why a Variance-Covariance Matrix is Needed
- Account for Within-Study Correlations: When multiple outcomes are reported within the same study, they are often correlated. Ignoring these correlations can lead to inaccurate estimates of the overall effect size and its variance.
- Borrowing Strength: The variance-covariance matrix allows the analysis to borrow strength across different outcomes, leading to more precise estimates.
- Improve Model Accuracy: Including the correct variance-covariance structure improves the accuracy of the random-effects model, leading to better inference.

```{r}
# Function to calculate the variance-covariance matrix for a given dataset
calculate_v_matrix <- function(data, correlation = 0.5) {
  cat("\nCalculating Variance-Covariance Matrix...\n")
  
  # Initialize an empty list to store the variance-covariance matrices for each study
  v_list <- list()
  
  # Loop through each unique study ID
  for (study in unique(data$id_article)) {
    # Subset the data for the current study
    study_data <- data[data$id_article == study, ]
    
    # Check if the study has more than one outcome
    if (nrow(study_data) > 1) {
      # Create a diagonal matrix of variances
      v <- diag(study_data$vi)
      
      # Set the off-diagonal elements assuming a constant correlation
      for (i in 1:nrow(v)) {
        for (j in 1:nrow(v)) {
          if (i != j) {
            v[i, j] <- correlation * sqrt(v[i, i] * v[j, j])
          }
        }
      }
      
      # Store the matrix in the list
      v_list[[as.character(study)]] <- v
    } else {
      # For single outcome studies, use the variance directly
      v_list[[as.character(study)]] <- matrix(study_data$vi, nrow = 1, ncol = 1)
    }
  }
  
  # Combine all matrices into a block-diagonal matrix
  v_matrix <- bldiag(v_list)
  cat("Variance-Covariance Matrix Calculation Complete.\n")
  
  return(v_matrix)
}
```







#############
# STEP 2
##########################################################################################################################################
SUBGROUP META-ANALYSIS, MULTIVARIATE/MULTILEVEL LINEAR (MIXED-EFFECTS) MODELLING 
##########################################################################################################################################

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

# Function to fit a meta-analytic model for a given subgroup
# This function takes a subset of the data, a subgroup name, the variance-covariance matrix (v_matrix),
# and optional moderators to fit a random-effects meta-analytic model.
fit_subgroup_model <- function(data, subgroup_name, v_matrix, moderators = NULL) {
  cat("\nFitting model for subgroup:", subgroup_name, "...\n")
  
  # Create a formula for the model. If moderators are provided, they are included in the model.
  # Otherwise, it defaults to an intercept-only model.
  moderator_formula <- if (!is.null(moderators)) {
    as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  } else {
    as.formula("yi ~ 1")  # Intercept-only model
  }
  
  # Try fitting the meta-analytic model, handling errors gracefully.
  model <- tryCatch({
    rma.mv(
      yi = yi,                               # Dependent variable (effect sizes)
      V = v_matrix,                          # Variance-covariance matrix
      mods = moderator_formula,              # Moderators (if any)
      random = list(                         # Random effects structure
        ~ 1 | id_article,                    # Random intercept for articles
        ~ 1 | id_article/response_variable,  # Nested random effect for response variables
        ~ 1 | exp_id                         # Random intercept for experiments
      ),
      data = data,                           # Data subset for this subgroup
      method = "ML",                         # Maximum Likelihood estimation
      control = list(                        # Optimization settings
        optimizer = "optim",
        optim.method = "BFGS",               # Optimization algorithm
        iter.max = 1000,                     # Maximum number of iterations
        rel.tol = 1e-8                       # Convergence tolerance
      )
    )
  }, error = function(e) {              # Error handling
    cat("Error fitting model for subgroup:", subgroup_name, ":", e$message, "\n")
    return(NULL)                        # Return NULL if the model fitting fails
  })
  
  # If the model is successfully fitted, extract key statistics and return them.
  if (!is.null(model)) {
    cat("Model fitting completed for subgroup:", subgroup_name, ".\n")
    return(list(
      subgroup = subgroup_name,         # Name of the subgroup
      aic = AIC(model),                 # Akaike Information Criterion (model fit)
      bic = BIC(model),                 # Bayesian Information Criterion (model fit)
      logLik = logLik(model),           # Log-likelihood value
      tau2 = sum(model$sigma2),         # Total variance components
      model = model                     # The fitted model object
    ))
  } else {
    return(NULL)                        # Return NULL if model fitting fails
  }
}

# Function to perform subgroup analysis across different levels of a specified variable
# This function splits the data into subgroups, fits meta-analytic models for each subgroup,
# and compiles the results into a summary table.
run_subgroup_analysis <- function(data, v_matrix, split_var, moderators = NULL) {
  # Split the data into subsets based on the levels of the specified variable (split_var).
  subgroups <- split(data, data[[split_var]])
  
  # Fit a meta-analytic model for each subgroup using map.
  results <- map(names(subgroups), ~ {
    # Extract the data for the current subgroup
    subgroup_data <- subgroups[[.x]]
    
    # Subset the variance-covariance matrix to match the rows of the current subgroup
    subgroup_indices <- which(data[[split_var]] == .x)
    v_matrix_subgroup <- v_matrix[subgroup_indices, subgroup_indices, drop = FALSE]
    
    # Fit the model for the current subgroup
    fit_subgroup_model(subgroup_data, .x, v_matrix_subgroup, moderators)
  })
  
  # Remove any NULL results (models that failed to fit).
  results <- results[!sapply(results, is.null)]
  
  # Compile the results into a summary data frame with key statistics for each subgroup.
  summary_df <- bind_rows(lapply(results, function(res) {
    data.frame(
      Subgroup = res$subgroup,          # Subgroup name
      AIC = res$aic,                    # Akaike Information Criterion
      BIC = res$bic,                    # Bayesian Information Criterion
      LogLikelihood = as.numeric(res$logLik),  # Log-likelihood
      Tau2 = res$tau2                   # Total variance components
    )
  }))
  
  # Return both the detailed results and the summary table.
  list(results = results, summary = summary_df)
}


# Define the dataset to use for meta-analysis
meta_dataset <- imp_data_rom  # Imputed dataset (replace with your actual dataset)

# Variable to split the data into subgroups
response_variable <- "response_variable"  # Column to split the data on (e.g., response type)

# Moderators to include in the model (optional)
moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")

# Calculate the variance-covariance matrix (replace with your actual function)
v_matrix <- calculate_v_matrix(meta_dataset, correlation = 0.5)

# Run the subgroup analysis
subgroup_analysis_results <- run_subgroup_analysis(meta_dataset, v_matrix, response_variable, moderators)

# Print the summary of subgroup results
print(subgroup_analysis_results$summary)

# Define output directory for saving results
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save fitted models for each subgroup
lapply(subgroup_analysis_results$results, function(res) {
  if (!is.null(res)) {
    saveRDS(res$model, file = file.path(output_dir, paste0("subgroup_", res$subgroup, "_model.rds")))
  }
})

# Save the summary table
write.csv(subgroup_analysis_results$summary, file = file.path(output_dir, "subgroup_analysis_summary.csv"))


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (17/11-24)
# Time difference of 3.211481 mins

# str(model_results)
```

```{r}
# Run the subgroup analysis with the corrected V_matrix handling
subgroup_analysis_results <- run_subgroup_analysis(meta_dataset, v_matrix, response_variable, moderators)

# Print the summary of subgroup results
print(subgroup_analysis_results$summary)

# Save the summary table
write.csv(subgroup_analysis_results$summary, file = file.path(output_dir, "subgroup_analysis_summary.csv"))

```




















































```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- non_imp_data_dummy

# Filter subgroups with sufficient data points (e.g., at least 10 studies)
min_studies <- 10
subgroup_counts <- table(data$response_variable)
selected_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
filtered_data <- data %>% filter(response_variable %in% selected_subgroups)

# Subgroup levels (filtered)
subgroups <- unique(filtered_data$response_variable)

# Fit Overall Random-Effects Model
overall_model <- rma(yi, vi, data = filtered_data)

# Helper function for Q-test, I², and τ² information
mlabfun <- function(text, x) {
  list(bquote(paste(
    .(text), " (Q = ", .(formatC(x$QE, digits = 2, format = "f")),
    ", df = ", .(x$k - x$p), ", ", .(format.pval(x$QEp, digits = 2)), "; ",
    I^2, " = ", .(formatC(x$I2, digits = 1, format = "f")), "%, ",
    tau^2, " = ", .(formatC(x$tau2, digits = 2, format = "f")), ")"
  )))
}

# Prepare supplementary data for ilab
ilab_data <- cbind(
  "Silvo N" = filtered_data$silvo_n,
  "Control N" = filtered_data$control_n
)

# Forest Plot for the Overall Model
forest(
  overall_model,
  xlim = c(-4, 2),
  at = log(c(0.1, 0.5, 1, 2)), 
  atransf = exp,
  ilab = ilab_data,  # Add the supplementary data
  ilab.xpos = c(-2, -1),  # Adjust column positions for supplementary info
  cex = 0.8,
  ylim = c(-2, 3 + length(subgroups) * 2),  # Adjust ylim for fewer elements
  top = 2,
  mlab = mlabfun("Random-Effects Model for All Data", overall_model),
  header = c("Subgroup and Study", "Effect Size [95% CI]")
)

# Add labels for ilab columns
text(c(-2, -1), max(filtered_data$yi) + 2, 
     c("Silvo N", "Control N"), pos = 4, font = 2)

# Row positions for subgroup summary polygons
row_positions <- seq(1, by = 2, length.out = length(subgroups))

# Add Subgroup Summary Polygons
for (i in seq_along(subgroups)) {
  subgroup <- subgroups[i]
  
  # Fit random-effects model for each subgroup
  subgroup_model <- rma(
    yi, vi, data = filtered_data, subset = (response_variable == subgroup)
  )
  
  # Add summary polygon for the subgroup
  addpoly(
    subgroup_model, row = row_positions[i], 
    mlab = mlabfun(paste("Random-Effects Model for", subgroup), subgroup_model)
  )
  
  # Add subgroup label
  text(-4, row_positions[i] + 1, pos = 4, subgroup, font = 4)
}

# Test for Subgroup Differences (Meta-Regression Model)
subgroup_test <- rma(yi, vi, mods = ~ response_variable, data = filtered_data)

# Add Test for Subgroup Differences
text(-4, -1.5, pos = 4, cex = 0.75, bquote(paste(
  "Test for Subgroup Differences: ",
  Q[M], " = ", .(formatC(subgroup_test$QM, digits = 2, format = "f")),
  ", df = ", .(subgroup_test$p - 1), ", ", .(format.pval(subgroup_test$QMp, digits = 2))
)))

```

```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- non_imp_data_dummy

# Filter subgroups with sufficient data points (e.g., at least 10 studies)
min_studies <- 10
subgroup_counts <- table(data$response_variable)
selected_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
filtered_data <- data %>% filter(response_variable %in% selected_subgroups)


# Fit random-effects models for each subgroup and extract correct metrics
subgroup_results <- filtered_data %>%
  group_by(response_variable) %>%
  summarise(
    model = list(rma(yi, vi, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(
    conf_low = map_dbl(model, ~ exp(confint(.x)$random[1, 1])),
    conf_high = map_dbl(model, ~ exp(confint(.x)$random[1, 2])),
    mean_effect = map_dbl(model, ~ exp(coef(.x)["intrcpt"]))
  )

# Add subgroup diagnostics to the dataset
filtered_data <- filtered_data %>%
  left_join(
    subgroup_results %>% select(response_variable, conf_low, conf_high, mean_effect),
    by = "response_variable"
  )

# Create ridge density plot with subgroup diagnostics
ridge_plot_with_metrics <- ggplot(filtered_data, aes(x = yi, y = response_variable, fill = response_variable)) +
  ggridges::geom_density_ridges(alpha = 0.8, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = c(
    "Biodiversity" = "#7B3294", "Crop yield" = "#C2A5CF",
    "Greenhouse gas emission" = "#008837", "Soil quality" = "#A6D96A",
    "Pest and Disease" = "#1B7837", "Product quality" = "#E6F5D0",
    "Water quality" = "#FFD700"
  )) +
  labs(
    title = "Forest Plot with Subgroups and Diagnostics",
    subtitle = "Random-effects models fitted for each subgroup",
    x = "Effect Size (Log Scale)", y = "Subgroups",
    fill = "Subgroups"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.title.y = element_text(angle = 90, vjust = 0.5)
  ) +
  geom_text(
    data = subgroup_results,
    aes(
      x = 3.2, y = response_variable,
      label = paste0(
        "n = ", map_int(model, ~ .x$k), "\n",
        "Mean: ", round(mean_effect, 2), "\n",
        "CI: [", round(conf_low, 2), ", ", round(conf_high, 2), "]"
      )
    ),
    hjust = 0, vjust = 0, size = 4, color = "black"
  ) +
  scale_x_continuous(limits = c(-2, 3.5))  # Adjust x-axis range for annotations

ridge_plot_with_metrics

```


```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- non_imp_dataset

# Filter subgroups with sufficient data points (e.g., at least 10 studies)
min_studies <- 10
subgroup_counts <- table(data$response_variable)
selected_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
filtered_data <- data %>% filter(response_variable %in% selected_subgroups)


# Fit random-effects models for each subgroup and extract correct metrics
subgroup_results <- filtered_data %>%
  group_by(response_variable) %>%
  summarise(
    model = list(rma(yi, vi, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(
    conf_low = map_dbl(model, ~ exp(confint(.x)$random[1, 1])),
    conf_high = map_dbl(model, ~ exp(confint(.x)$random[1, 2])),
    mean_effect = map_dbl(model, ~ exp(coef(.x)["intrcpt"]))
  )

# Add subgroup diagnostics to the dataset
filtered_data <- filtered_data %>%
  left_join(
    subgroup_results %>% select(response_variable, conf_low, conf_high, mean_effect),
    by = "response_variable"
  )

# Create ridge density plot with subgroup diagnostics
ridge_plot_with_metrics <- ggplot(filtered_data, aes(x = yi, y = response_variable, fill = response_variable)) +
  ggridges::geom_density_ridges(alpha = 0.8, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = c(
    "Biodiversity" = "#7B3294", "Crop yield" = "#C2A5CF",
    "Greenhouse gas emission" = "#008837", "Soil quality" = "#A6D96A",
    "Pest and Disease" = "#1B7837", "Product quality" = "#E6F5D0",
    "Water quality" = "#FFD700"
  )) +
  labs(
    title = "Forest Plot with Subgroups and Diagnostics",
    subtitle = "Random-effects models fitted for each subgroup",
    x = "Effect Size (Log Scale)", y = "Subgroups",
    fill = "Subgroups"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.title.y = element_text(angle = 90, vjust = 0.5)
  ) +
  geom_text(
    data = subgroup_results,
    aes(
      x = 3.2, y = response_variable,
      label = paste0(
        "n = ", map_int(model, ~ .x$k), "\n",
        "Mean: ", round(mean_effect, 2), "\n",
        "CI: [", round(conf_low, 2), ", ", round(conf_high, 2), "]"
      )
    ),
    hjust = 0, vjust = 0, size = 4, color = "black"
  ) +
  scale_x_continuous(limits = c(-2, 3.5))  # Adjust x-axis range for annotations

ridge_plot_with_metrics

```

```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- non_imp_data_dummy

# Filter subgroups with sufficient data points (e.g., at least 10 studies)
min_studies <- 10
subgroup_counts <- table(data$response_variable)
selected_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
filtered_data <- data %>% filter(response_variable %in% selected_subgroups)

# Subgroup levels (filtered)
subgroups <- unique(filtered_data$response_variable)

# Initialize empty list to store subgroup metrics
subgroup_metrics <- list()

# Iterate over subgroups and extract metrics
for (subgroup in subgroups) {
  # Fit random-effects model for each subgroup
  subgroup_model <- tryCatch(
    rma(yi, vi, data = filtered_data, subset = (response_variable == subgroup)), 
    error = function(e) NULL
  )
  
  if (!is.null(subgroup_model)) {
    # Extract metrics
    metrics <- list(
      response_variable = subgroup,
      mean_effect = coef(subgroup_model)["intrcpt"], # Mean effect size
      conf_low = confint(subgroup_model)$random["ci.lb"], # CI lower bound
      conf_high = confint(subgroup_model)$random["ci.ub"], # CI upper bound
      I2 = subgroup_model$I2,  # Heterogeneity (I^2)
      tau2 = subgroup_model$tau2,  # Between-study variance (τ²)
      n_studies = subgroup_model$k  # Number of studies
    )
  } else {
    # If model fitting fails, populate NA values
    metrics <- list(
      response_variable = subgroup,
      mean_effect = NA,
      conf_low = NA,
      conf_high = NA,
      I2 = NA,
      tau2 = NA,
      n_studies = NA
    )
  }
  
  # Append metrics to the list
  subgroup_metrics[[subgroup]] <- metrics
}

# Convert metrics list to dataframe
subgroup_metrics_df <- do.call(rbind, lapply(subgroup_metrics, as.data.frame))

# Inspect the resulting dataframe
print(subgroup_metrics_df)

```

```{r}

# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- non_imp_data_dummy

# Filter subgroups with sufficient data points and valid variances
filtered_data <- data %>%
  filter(response_variable %in% selected_subgroups & !is.na(yi) & !is.na(vi) & vi > 0)

# Fit random-effects models for each subgroup and safely extract metrics
# Fit random-effects models for each subgroup and calculate CI
subgroup_results <- filtered_data %>%
  group_by(response_variable) %>%
  summarise(
    # Fit model with tryCatch to handle potential errors
    model = list(
      tryCatch(
        rma(yi, vi, data = cur_data()), 
        error = function(e) NULL
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    # Extract confidence intervals and metrics safely
    conf_low = map_dbl(model, ~ if (!is.null(.x)) {
      tryCatch(confint(.x)$random["ci.lb"], error = function(e) NA_real_)
    } else NA_real_),
    conf_high = map_dbl(model, ~ if (!is.null(.x)) {
      tryCatch(confint(.x)$random["ci.ub"], error = function(e) NA_real_)
    } else NA_real_),
    mean_effect = map_dbl(model, ~ if (!is.null(.x)) {
      tryCatch(coef(.x)["intrcpt"], error = function(e) NA_real_)
    } else NA_real_),
    n_studies = map_int(model, ~ if (!is.null(.x)) .x$k else NA_integer_),
    I2 = map_dbl(model, ~ if (!is.null(.x)) .x$I2 else NA_real_)
  )

# Add subgroup results to the dataset
filtered_data <- filtered_data %>%
  left_join(
    subgroup_results %>% select(response_variable, conf_low, conf_high, mean_effect, n_studies, I2),
    by = "response_variable"
  )

# Ridge density plot with subgroup diagnostics
ridge_plot_with_metrics <- ggplot(filtered_data, aes(x = yi, y = response_variable, fill = response_variable)) +
  ggridges::geom_density_ridges(alpha = 0.8, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = c(
    "Biodiversity" = "#7B3294", "Crop yield" = "#C2A5CF",
    "Greenhouse gas emission" = "#008837", "Soil quality" = "#A6D96A",
    "Pest and Disease" = "#1B7837", "Product quality" = "#E6F5D0",
    "Water quality" = "#FFD700"
  )) +
  labs(
    title = "Forest Plot with Subgroups and Diagnostics",
    subtitle = "Random-effects models fitted for each subgroup",
    x = "Effect Size (Log Scale)", y = "Subgroups",
    fill = "Subgroups"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.title.y = element_text(angle = 90, vjust = 0.5)
  ) +
  geom_text(
    data = subgroup_results,
    aes(
      x = 3.2, y = response_variable,
      label = paste0(
        "n = ", ifelse(is.na(n_studies), "NA", n_studies), "\n",
        "Mean: ", ifelse(is.na(mean_effect), "NA", round(mean_effect, 2)), "\n",
        "CI: [", ifelse(is.na(conf_low), "NA", round(conf_low, 2)), ", ",
        ifelse(is.na(conf_high), "NA", round(conf_high, 2)), "]\n",
        "I² = ", ifelse(is.na(I2), "NA", round(I2, 1)), "%"
      )
    ),
    hjust = 0, vjust = 0, size = 4, color = "black"
  ) +
  scale_x_continuous(limits = c(-2, 3.5))  # Adjust x-axis range for annotations

filtered_data |> glimpse()
ridge_plot_with_metrics
```

```{r}
# Ensure 'yi', 'vi', and 'response_variable' columns exist in your dataset
data <- non_imp_data_dummy

# Filter subgroups with sufficient data points (e.g., at least 10 studies)
min_studies <- 10
subgroup_counts <- table(data$response_variable)
selected_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
filtered_data <- data %>% filter(response_variable %in% selected_subgroups)



# Fit random-effects models for each subgroup and handle potential errors
subgroup_results <- filtered_data %>%
  group_by(response_variable) %>%
  summarise(
    model = list(
      tryCatch(
        rma(yi, vi, data = cur_data()), # Fit random-effects model
        error = function(e) NULL        # Return NULL if model fails
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    # Safely extract confidence intervals
    conf_low = map_dbl(model, ~ if (!is.null(.x) && !is.null(confint(.x)$fixed)) {
      tryCatch(
        exp(confint(.x)$fixed["intrcpt", "ci.lb"]), # Exponentiate lower bound
        error = function(e) NA_real_
      )
    } else if (!is.null(.x) && !is.null(confint(.x)$random)) { # Fall back to random effects
      tryCatch(
        exp(confint(.x)$random["tau2", "ci.lb"]), # Example for random-effects CI
        error = function(e) NA_real_
      )
    } else {
      NA_real_
    }),
    conf_high = map_dbl(model, ~ if (!is.null(.x) && !is.null(confint(.x)$fixed)) {
      tryCatch(
        exp(confint(.x)$fixed["intrcpt", "ci.ub"]), # Exponentiate upper bound
        error = function(e) NA_real_
      )
    } else if (!is.null(.x) && !is.null(confint(.x)$random)) { # Fall back to random effects
      tryCatch(
        exp(confint(.x)$random["tau2", "ci.ub"]), # Example for random-effects CI
        error = function(e) NA_real_
      )
    } else {
      NA_real_
    }),
    mean_effect = map_dbl(model, ~ if (!is.null(.x)) {
      tryCatch(exp(coef(.x)["intrcpt"]), error = function(e) NA_real_)
    } else {
      NA_real_
    }),
    n_studies = map_int(model, ~ if (!is.null(.x)) .x$k else NA_integer_),
    I2 = map_dbl(model, ~ if (!is.null(.x)) .x$I2 else NA_real_)
  )


# Log problematic subgroups for debugging
problematic <- subgroup_results %>% filter(is.na(conf_low) | is.na(conf_high))
if (nrow(problematic) > 0) {
  cat("Warning: Issues with the following subgroups:\n")
  print(problematic$response_variable)
}


# Add subgroup diagnostics to the dataset for plotting
filtered_data <- filtered_data %>%
  left_join(
    subgroup_results %>% select(response_variable, conf_low, conf_high, mean_effect, n_studies, I2),
    by = "response_variable"
  )

# Ridge density plot with subgroup diagnostics
ridge_plot_with_metrics <- ggplot(filtered_data, aes(x = yi, y = response_variable, fill = response_variable)) +
  ggridges::geom_density_ridges(alpha = 0.8, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = c(
    "Biodiversity" = "#7B3294", "Crop yield" = "#C2A5CF",
    "Greenhouse gas emission" = "#008837", "Soil quality" = "#A6D96A",
    "Pest and Disease" = "#1B7837", "Product quality" = "#E6F5D0",
    "Water quality" = "#FFD700"
  )) +
  labs(
    title = "Forest Plot with Subgroups and Diagnostics",
    subtitle = "Random-effects models fitted for each subgroup",
    x = "Effect Size (Log Scale)", y = "Subgroups",
    fill = "Subgroups"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.title.y = element_text(angle = 90, vjust = 0.5)
  ) +
  geom_text(
    data = subgroup_results,
    aes(
      x = 3.2, y = response_variable,
      label = paste0(
        "n = ", ifelse(is.na(n_studies), "NA", n_studies), "\n",
        "Mean: ", ifelse(is.na(mean_effect), "NA", round(mean_effect, 2)), "\n",
        "CI: [", ifelse(is.na(conf_low), "NA", round(conf_low, 2)), ", ",
        ifelse(is.na(conf_high), "NA", round(conf_high, 2)), "]\n",
        "I² = ", ifelse(is.na(I2), "NA", round(I2, 1)), "%"
      )
    ),
    hjust = 0, vjust = 0, size = 4, color = "black"
  ) +
  scale_x_continuous(limits = c(-2, 3.5))  # Adjust x-axis range for annotations

ridge_plot_with_metrics

```
























INTERPRETATION




```{r}
# Define the output directory
output_dir <- here::here("DATA", "OUTPUT_FROM_R")

# Save the V_matrix for each dataset
saveRDS(V_matrices$non_imp_dataset, file = file.path(output_dir, "v_matrix_non_imp_dataset.rds"))
saveRDS(V_matrices$imp_dataset, file = file.path(output_dir, "v_matrix_imp_dataset.rds"))

cat("Variance-covariance matrices have been saved to:", output_dir, "\n")
```

#############
# STEP 2
##########################################################################################################################################
EVALUATION OF MODEL FITTING 
##########################################################################################################################################


##########################################################################################################################################
Evaluation of model fitting - comparing the two models
##########################################################################################################################################

```{r}
# Extract AIC, BIC, Log-Likelihood, and I²
model_stats <- model_summaries %>%
  pivot_longer(cols = c(AIC, BIC, LogLikelihood, I2),
               names_to = "Statistic",
               values_to = "Value")

# Extract fixed effects estimates for each model
extract_fixed_effects <- function(model_summary, dataset_name) {
  if (is.null(model_summary)) {
    return(data.frame(
      Dataset = dataset_name,
      Term = NA,
      Estimate = NA,
      CI_Lower = NA,
      CI_Upper = NA
    ))
  }
  
  coef_df <- data.frame(
    Term = rownames(model_summary$model$b),
    Estimate = model_summary$model$b[, 1],
    CI_Lower = model_summary$model$ci.lb,
    CI_Upper = model_summary$model$ci.ub
  )
  
  coef_df$Dataset <- dataset_name
  return(coef_df)
}

# Combine fixed effects data across all models
fixed_effects_data <- bind_rows(
  extract_fixed_effects(model_results$non_imp_dataset, "Non-Imputed Dataset"),
  extract_fixed_effects(model_results$imp_dataset, "Imputed Dataset")
)

# Filter out rows with NA values
fixed_effects_data <- fixed_effects_data %>% drop_na()
fixed_effects_data
```

##########################################################################################################################################
Visualization 1: Model Fit Comparison (AIC, BIC, Log-Likelihood, and I²)
##########################################################################################################################################

```{r}
# Plot Model Fit Statistics
fit_plot <- ggplot(model_stats, aes(x = fct_reorder(Dataset, Value), y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Statistic, scales = "free") +
  labs(title = "Comparison of Model Fit Statistics",
       x = "Dataset",
       y = "Value",
       fill = "Statistic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fit_plot)

```



Visualization 2: Fixed Effects Estimates Comparison

```{r}
# Plot Fixed Effects Estimates with Confidence Intervals
coef_plot <- ggplot(fixed_effects_data, aes(x = Term, y = Estimate, color = Dataset)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(title = "Comparison of Fixed Effects Estimates Across Models",
       x = "Fixed Effect Term",
       y = "Estimate",
       color = "Dataset") +
  theme_minimal()

print(coef_plot)

```


Visualization 3: Heterogeneity (I²) Comparison

```{r}
# Heterogeneity Comparison Plot
I2_plot <- ggplot(model_summaries, aes(x = Dataset, y = I2, fill = Dataset)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(I2, 2)), vjust = -0.5) +
  labs(title = "Comparison of I² (Heterogeneity) Across Models",
       x = "Dataset",
       y = "I² (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(I2_plot)

```

Comparison table of key model statistics

```{r}
colnames(model_summaries)

```

```{r}
# Define the updated function to extract key statistics
extract_model_summary <- function(model_list, dataset_name) {
  # Check if the model list is not NULL and contains a model object
  if (is.null(model_list) || !inherits(model_list, "list")) {
    return(data.frame(
      Dataset = dataset_name,
      k.all = NA,
      LogLikelihood = NA,
      AIC = NA,
      BIC = NA,
      I2 = NA,
      QM = NA,
      QMp = NA
    ))
  }
  
  # Extract the actual model object from the list
  model <- model_list$model
  
  # If the model object is NULL or does not have class "rma.mv", return NA
  if (is.null(model) || !inherits(model, "rma.mv")) {
    return(data.frame(
      Dataset = dataset_name,
      k.all = NA,
      LogLikelihood = NA,
      AIC = NA,
      BIC = NA,
      I2 = NA,
      QM = NA,
      QMp = NA
    ))
  }
  
  # Extract key statistics
  k.all <- model$k.all
  logLik <- as.numeric(logLik(model))
  AIC <- AIC(model)
  BIC <- BIC(model)
  I2 <- round((sum(model$sigma2) / (sum(model$sigma2) + mean(model$vi))) * 100, 1)
  QM <- model$QM
  QMp <- model$QMp
  
  # Create a summary data frame
  data.frame(
    Dataset = dataset_name,
    k.all = k.all,
    LogLikelihood = logLik,
    AIC = AIC,
    BIC = BIC,
    I2 = I2,
    QM = QM,
    QMp = QMp
  )
}

# Apply the updated function to all models in `model_results`
model_summaries <- bind_rows(
  extract_model_summary(model_results$non_imp_dataset, "Non-Imputed Dataset"),
  extract_model_summary(model_results$imp_dataset, "Imputed Dataset"),
  extract_model_summary(model_results$non_imp_dataset_imputed, "Non-Imputed Imputed Dataset"),
  extract_model_summary(model_results$imp_dataset_imputed, "Imputed Imputed Dataset")
)

# View the combined summary table
print(model_summaries)

```


```{r}
# Create a summary table with existing columns
comparison_table <- model_summaries %>%
  mutate(
    LogLikelihood = round(LogLikelihood, 2),
    AIC = round(AIC, 2),
    BIC = round(BIC, 2),
    I2 = paste0(round(I2, 1), "%")
  )

# Create a formatted table using `gt`
comparison_gt <- comparison_table %>%
  gt() %>%
  tab_header(
    title = "Model Comparison Summary",
    subtitle = "Key Statistics for Evaluating Model Fit"
  ) %>%
  cols_label(
    Dataset = "Dataset",
    LogLikelihood = "Log-Likelihood",
    AIC = "AIC",
    BIC = "BIC",
    I2 = "I² (%)"
  ) %>%
  fmt_number(
    columns = c(LogLikelihood, AIC, BIC),
    decimals = 2
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f5f5f5"),
      cell_borders(sides = "all", color = "gray", weight = px(1))
    ),
    locations = cells_body()
  ) %>%
  tab_options(
    table.font.size = "small",
    table.border.top.color = "gray",
    table.border.bottom.color = "gray"
  )

# Optionally export the table
# Define the output folder path
output_folder <- here("DATA", "OUTPUT_FROM_R")

# Export the table to HTML and PDF in the specified folder
gtsave(comparison_gt, file.path(output_folder, "model_comparison_summary.html"))
gtsave(comparison_gt, file.path(output_folder, "model_comparison_summary.pdf"))

# Display the table
comparison_gt
```






INTERPRETATION OF MODEL 



Evaluation plots

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

# Define the model names and colors
model_names <- c("Non-Imputed Dataset", "Imputed Dataset", "Non-Imputed Imputed Dataset", "Imputed Imputed Dataset")
colors <- c("#0072B2", "#E69F00", "#009E73", "#D55E00")

# Initialize lists to store the plots
residuals_plots <- list()
conf_intervals_plots <- list()
std_residuals_plots <- list()

# Define the output directory
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "FIGURES")

# Loop through each model and generate the plots
for (i in seq_along(model_results)) {
  model <- model_results[[i]]$model
  model_name <- model_names[i]
  color <- colors[i]
  V_matrix <- V_matrices[[i]]
  
  # Generate Residuals vs. Fitted Values Plot
  residuals_plot <- tryCatch({
    plot_residuals_vs_fitted(model, model_name, color)
  }, error = function(e) {
    cat("Error generating Residuals vs. Fitted plot for", model_name, ":", e$message, "\n")
    NULL
  })
  residuals_plots[[i]] <- residuals_plot
  
  # Updated function to calculate bootstrap confidence intervals and return a ggplot
  bootstrap_conf_intervals <- function(model, model_name, V_matrix, n_boot = 1000, alpha = 0.05) {
    cat("\nCalculating Bootstrap Confidence Intervals for", model_name, "...\n")
    
    # Initialize a matrix to store the bootstrap estimates
    boot_estimates <- matrix(NA, nrow = n_boot, ncol = length(coef(model)))
    colnames(boot_estimates) <- names(coef(model))
    
    # Bootstrap loop
    for (b in 1:n_boot) {
      resample_indices <- sample(nrow(model$data), replace = TRUE)
      resampled_data <- model$data[resample_indices, ]
      
      boot_model <- tryCatch({
        rma.mv(
          yi = resampled_data$yi,
          V = V_matrix[resample_indices, resample_indices],
          mods = model$mods,
          random = model$random,
          data = resampled_data,
          method = "ML"
        )
      }, error = function(e) NULL)
      
      if (!is.null(boot_model)) {
        boot_estimates[b, ] <- coef(boot_model)
      }
    }
    
    boot_estimates <- boot_estimates[complete.cases(boot_estimates), ]
    lower_bound <- apply(boot_estimates, 2, quantile, probs = alpha / 2)
    upper_bound <- apply(boot_estimates, 2, quantile, probs = 1 - alpha / 2)
    
    conf_int_df <- data.frame(
      Term = names(coef(model)),
      Estimate = coef(model),
      CI.Lower = lower_bound,
      CI.Upper = upper_bound
    )
    
    cat("Bootstrap Confidence Intervals Calculation Complete for", model_name, ".\n")
    
    # Create a ggplot object
    ggplot(conf_int_df, aes(x = Term, y = Estimate)) +
      geom_point(color = "#0072B2") +
      geom_errorbar(aes(ymin = CI.Lower, ymax = CI.Upper), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = paste("Bootstrap Confidence Intervals -", model_name),
        x = "Terms",
        y = "Estimates"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  
  # Generate Standardized Residuals Plot
  std_residuals_plot <- tryCatch({
    plot_standardized_residuals(model, model_name, color)
  }, error = function(e) {
    cat("Error generating Standardized Residuals plot for", model_name, ":", e$message, "\n")
    NULL
  })
  std_residuals_plots[[i]] <- std_residuals_plot
  
  # Generate and Save Forest Plot Individually
  tryCatch({
    create_forest_plot(model, model_name, V_matrix)
    ggsave(
      filename = file.path(output_dir, paste0("forest_plot_", model_name, ".jpg")),
      width = 10, height = 8, dpi = 300
    )
    cat("Forest plot saved for", model_name, ".\n")
  }, error = function(e) {
    cat("Error generating or saving Forest plot for", model_name, ":", e$message, "\n")
  })
}


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (17/11-24)
# Time difference of 25.3915 secs
# 
# Calculating Bootstrap Confidence Intervals for Non-Imputed Dataset ...
# Bootstrap Confidence Intervals Calculation Complete for Non-Imputed Dataset .
# 
# Forest Plot for Non-Imputed Dataset :
# 
# Calculating Bootstrap Confidence Intervals for Imputed Dataset ...
# Bootstrap Confidence Intervals Calculation Complete for Imputed Dataset .
# 
# Forest Plot for Imputed Dataset :
# Advarsel: longer object length is not a multiple of shorter object length
# Calculating Bootstrap Confidence Intervals for Non-Imputed Imputed Dataset ...
# Bootstrap Confidence Intervals Calculation Complete for Non-Imputed Imputed Dataset .
# 
# Forest Plot for Non-Imputed Imputed Dataset :
# 
# Calculating Bootstrap Confidence Intervals for Imputed Imputed Dataset ...
# Bootstrap Confidence Intervals Calculation Complete for Imputed Imputed Dataset .
# 
# Forest Plot for Imputed Imputed Dataset :
```

Saving plots

```{r}

```



##########################################################################################################################################
SAVING DATASETS AND MODEL OBJECTS
##########################################################################################################################################

```{r}
# Define the output directory
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List of datasets and their names
datasets <- list(
  non_imp_dataset = non_imp_dataset,
  imp_dataset = imp_dataset,
  non_imp_dataset_imputed = non_imp_dataset_imputed,
  imp_dataset_imputed = imp_dataset_imputed
)

# Save each dataset
for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  saveRDS(dataset, file = file.path(output_dir, paste0(dataset_name, ".rds")))
  cat("Dataset saved:", dataset_name, "\n")
}


# Save each variance-covariance matrix
for (matrix_name in names(V_matrices)) {
  V_matrix <- V_matrices[[matrix_name]]
  saveRDS(V_matrix, file = file.path(output_dir, paste0("V_matrix_", matrix_name, ".rds")))
  cat("Variance-Covariance Matrix saved:", matrix_name, "\n")
}


# Save each model object
for (model_name in names(model_results)) {
  model <- model_results[[model_name]]$model
  if (!is.null(model)) {
    saveRDS(model, file = file.path(output_dir, paste0("model_", model_name, ".rds")))
    cat("Model object saved:", model_name, "\n")
  } else {
    cat("Model object for", model_name, "is NULL. Skipping save.\n")
  }
}


# Save the model summary table
saveRDS(model_summaries, file = file.path(output_dir, "model_summaries.rds"))
cat("Model summary table saved.\n")


# List all saved files
saved_files <- list.files(output_dir, full.names = TRUE)
cat("All saved files:\n")
print(saved_files)

```


```{r}
# Define a threshold for Cook's distance
cook_threshold <- 4 / nrow(meta_data) # Adjust as needed

# Initialize a data frame to store results
influential_studies <- data.frame()

# Loop through each diagnostics entry
for (i in seq_along(diagnostics_list)) {
  diagnostics <- diagnostics_list[[i]]
  
  if (is.null(diagnostics)) {
    cat("\nDiagnostics for index", i, "is NULL. Skipping...\n")
    next
  }
  
  # Get the response variable name from the data
  response_variable <- unique(diagnostics$ResponseVariable)[1]
  cat("\nProcessing diagnostics for response variable:", response_variable, "\n")
  
  # Identify highly influential observations
  influential_obs <- diagnostics %>%
    filter(cook.d > cook_threshold) %>%
    select(Study, cook.d) # Keep Study ID and Cook's distance
  
  # Skip if no influential observations are found
  if (nrow(influential_obs) == 0) {
    cat("No influential observations for:", response_variable, "\n")
    next
  }
  
  # Map to `id_article` using `meta_data`
  influential_obs <- influential_obs %>%
    left_join(meta_data, by = c("Study" = "id_obs")) %>%
    select(id_article, cook.d, response_variable) %>%
    rename(StudyID = id_article, CookDistance = cook.d, ResponseVariable = response_variable)
  
  # Append to results
  influential_studies <- bind_rows(influential_studies, influential_obs)
}

# Check the results
influential_studies

# Save as RDS and CSV
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
saveRDS(influential_studies, file.path(output_dir, "influential_studies.rds"))
write.csv(influential_studies, file.path(output_dir, "influential_studies.csv"), row.names = FALSE)

cat("Influential studies saved to:", output_dir, "\n")
```



# Fit the model with random effects for id_article
res <- tryCatch({
  rma(
    yi = yi,
    vi = vi,
    # Random-effects structure: defines how the random effects are modeled hierarchically
    random = list(
      ~ 1 | id_article,                           # Random intercept for each article/study
      ~ 1 | id_article/response_variable,         # Nested random intercept for each response variable within articles
      ~ 1 | exp_id                                # Random intercept for individual experiments
    ),
    data = data_subset,
    method = "REML"
  )
}, error = function(e) {
  cat("Model fitting failed for", response, ":", e$message, "\n")
  return(NULL)
})

# Save the fitted model
model_results[[response]] <- res



# Initialize leave-one-out results
leave1out_results <- list()

for (response in names(model_results)) {
  cat("\nRunning Leave-One-Out for:", response, "...\n")
  
  # Retrieve the fitted model for the current response variable
  model <- model_results[[response]]
  
  if (!is.null(model)) {
    loo <- leave1out(model)  # Perform LOO at observation level
    
    # Map LOO results to `id_article`
    loo_data <- meta_data[meta_data$response_variable == response, ]
    study_level_results <- loo_data %>%
      group_by(id_article) %>%
      summarise(
        Estimate = mean(loo$estimate[match(id_obs, rownames(loo$estimate))], na.rm = TRUE),
        SE = mean(loo$se[match(id_obs, rownames(loo$estimate))], na.rm = TRUE),
        CI_Lower = mean(loo$estimate[match(id_obs, rownames(loo$estimate))] - 
                          1.96 * loo$se[match(id_obs, rownames(loo$estimate))], na.rm = TRUE),
        CI_Upper = mean(loo$estimate[match(id_obs, rownames(loo$estimate))] + 
                          1.96 * loo$se[match(id_obs, rownames(loo$estimate))], na.rm = TRUE)
      )
    
    study_level_results$ResponseVariable <- response
    leave1out_results[[response]] <- study_level_results
  } else {
    cat("Skipping Leave-One-Out for:", response, "due to missing model.\n")
  }
}

# Combine results into a single data frame
loo_combined <- bind_rows(leave1out_results)


```{r}
# Initialize list for study-level results
study_level_loo_results <- list()

for (response in names(model_results)) {
  cat("\nProcessing Response Variable:", response, "...\n")
  
  # Retrieve the model
  model <- model_results[[response]]
  
  if (!is.null(model)) {
    # Perform Leave-One-Out Diagnostics
    loo <- leave1out(model)
    
    # Map results back to meta_data
    loo_data <- meta_data[meta_data$response_variable == response, ] %>%
      mutate(
        Estimate = loo$estimate[match(id_obs, rownames(loo$estimate))],
        SE = loo$se[match(id_obs, rownames(loo$estimate))],
        CI_Lower = Estimate - 1.96 * SE,
        CI_Upper = Estimate + 1.96 * SE
      )
    
    # Aggregate results by id_article
    aggregated_results <- loo_data %>%
      group_by(id_article) %>%
      summarise(
        Estimate = mean(Estimate, na.rm = TRUE),
        SE = mean(SE, na.rm = TRUE),
        CI_Lower = mean(CI_Lower, na.rm = TRUE),
        CI_Upper = mean(CI_Upper, na.rm = TRUE),
        ResponseVariable = first(response)
      )
    
    study_level_loo_results[[response]] <- aggregated_results
  }
}
```
```{r}
loo_data |> glimpse()
```

```{r}
# Combine all study-level results into a single data frame
study_level_loo_combined <- bind_rows(study_level_loo_results)

# Visualize the Study-Level Leave-One-Out Results
loo_plot_corrected <- study_level_loo_combined %>%
  ggplot(aes(x = factor(id_article), y = Estimate, color = ResponseVariable)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  facet_wrap(~ ResponseVariable, scales = "free", ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right"
  ) +
  labs(
    title = "Leave-One-Out Effect Sizes by Study",
    x = "Study (id_article)",
    y = "Effect Size (Estimate ± 95% CI)",
    color = "Response Variable"
  )

# Save the plot
ggsave(
  filename = file.path(output_dir, "LOO_Study_Level_Effect_Sizes_Corrected.png"),
  plot = loo_plot_corrected,
  width = 12,
  height = 8,
  dpi = 300
)

# Print the plot
loo_plot_corrected
```

```{r}
loo_data |> glimpse()
```


```{r}
# Inspect the structure of leave1out_results
str(leave1out_results)

# Check the structure
str(loo_combined)
```

# Extract Leave-One-Out diagnostics into a data frame
loo_data <- bind_rows(
  lapply(names(leave1out_results), function(response) {
    loo <- leave1out_results[[response]]
    if (!is.null(loo)) {
      data.frame(
        StudyRemoved = if (!is.null(names(loo$estimate))) names(loo$estimate) else seq_along(loo$estimate),
        Estimate = loo$estimate,
        SE = loo$se,
        CI_Lower = loo$ci.lb,
        CI_Upper = loo$ci.ub,
        ResponseVariable = response,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
)

# Inspect the resulting data frame
str(loo_data)

```{r}
# Visualize Leave-One-Out Results at Study Level
loo_plot_corrected <- loo_combined %>%
  ggplot(aes(x = factor(id_article), y = Estimate, color = ResponseVariable)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  facet_wrap(~ ResponseVariable, scales = "free", ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right"
  ) +
  labs(
    title = "Leave-One-Out Effect Sizes by Study",
    x = "Study (id_article)",
    y = "Effect Size (Estimate ± 95% CI)",
    color = "Response Variable"
  )

```


leave1out_results <- list()

for (response in unique(diagnostics_data$ResponseVariable)) {
  cat("\nRunning Leave-One-Out for:", response, "...\n")
  
  # Retrieve the fitted model for the current response variable
  model <- model_results[[response]]
  
  # Perform Leave-One-Out analysis if the model is valid
  if (!is.null(model)) {
    leave1out_results[[response]] <- leave1out(model)
  } else {
    cat("Skipping Leave-One-Out for:", response, "due to missing model.\n")
  }
}

# Save Leave-One-Out results
saveRDS(leave1out_results, file.path(output_dir, "leave1out_results.rds"))
cat("\nLeave-One-Out results saved to:", file.path(output_dir, "leave1out_results.rds"), "\n")



```{r}
# Visualize Leave-One-Out Effect Sizes with Confidence Intervals
loo_plot <-
  loo_data |> 
  ggplot(aes(x = factor(StudyRemoved), y = Estimate, color = ResponseVariable)) +
  geom_point(size = 2) +  # Plot points for effect sizes
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +  # Add confidence intervals
  facet_wrap(~ ResponseVariable, scales = "free", ncol = 2) +  # Create panels for each response variable
  theme_minimal(base_size = 14) +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotate x-axis text for readability
    legend.position = "right"  # Place the legend at the bottom
  ) +
  labs(
    title = "Leave-One-Out Effect Sizes by Response Variable",
    x = "Study Removed",
    y = "Effect Size (Estimate ± 95% CI)",
    color = "Response Variable"
  )


loo_plot
```


# Initialize leave-one-out results
# Initialize leave-one-out results
leave1out_results <- list()

for (response in names(model_results)) {
  cat("\nRunning Leave-One-Out for:", response, "...\n")
  
  # Retrieve the fitted model for the current response variable
  model <- model_results[[response]]
  
  if (!is.null(model)) {
    # Perform Leave-One-Out diagnostics
    loo <- leave1out(model)
    
    # Map LOO results to meta_data
    loo_data <- meta_data %>%
      filter(response_variable == response) %>%
      mutate(
        Estimate = loo$estimate[match(as.character(id_article), rownames(loo$estimate))],
        SE = loo$se[match(as.character(id_article), rownames(loo$estimate))],
        CI_Lower = Estimate - 1.96 * SE,
        CI_Upper = Estimate + 1.96 * SE
      )
    
    # Check if mapping worked
    if (all(is.na(loo_data$Estimate))) {
      stop("Matching id_obs to leave-one-out results failed!")
    }
    
    # Aggregate results by id_article
    study_level_results <- loo_data %>%
      group_by(id_article) %>%
      summarise(
        Estimate = mean(Estimate, na.rm = TRUE),
        SE = mean(SE, na.rm = TRUE),
        CI_Lower = mean(CI_Lower, na.rm = TRUE),
        CI_Upper = mean(CI_Upper, na.rm = TRUE),
        ResponseVariable = first(response)
      )
    
    leave1out_results[[response]] <- study_level_results
  } else {
    cat("Skipping Leave-One-Out for:", response, "due to missing model.\n")
  }
}

# Combine all results into a single data frame
loo_combined <- bind_rows(leave1out_results)

# Glimpse the results
glimpse(loo_combined)




# Initialize leave-one-out results
# Initialize leave-one-out results
leave1out_results <- list()

for (response in names(model_results)) {
  cat("\nRunning Leave-One-Out for:", response, "...\n")
  
  # Retrieve the fitted model for the current response variable
  model <- model_results[[response]]
  
  if (!is.null(model)) {
    # Perform Leave-One-Out diagnostics
    loo <- leave1out(model)
    
    # Map LOO results to meta_data
    loo_data <- meta_data %>%
      filter(response_variable == response) %>%
      mutate(
        Estimate = loo$estimate[match(as.character(id_article), rownames(loo$estimate))],
        SE = loo$se[match(as.character(id_article), rownames(loo$estimate))],
        CI_Lower = Estimate - 1.96 * SE,
        CI_Upper = Estimate + 1.96 * SE
      )
    
    # Check if mapping worked
    if (all(is.na(loo_data$Estimate))) {
      stop("Matching id_obs to leave-one-out results failed!")
    }
    
    # Aggregate results by id_article
    study_level_results <- loo_data %>%
      group_by(id_article) %>%
      summarise(
        Estimate = mean(Estimate, na.rm = TRUE),
        SE = mean(SE, na.rm = TRUE),
        CI_Lower = mean(CI_Lower, na.rm = TRUE),
        CI_Upper = mean(CI_Upper, na.rm = TRUE),
        ResponseVariable = first(response)
      )
    
    leave1out_results[[response]] <- study_level_results
  } else {
    cat("Skipping Leave-One-Out for:", response, "due to missing model.\n")
  }
}

# Combine all results into a single data frame
loo_combined <- bind_rows(leave1out_results)

# Glimpse the results
glimpse(loo_combined)








```{r}
# Function to fit an rma model for a given subset
fit_response_variable_rma <- function(data, response_variable, moderators = NULL) {
  cat("\nFitting rma model for response variable:", response_variable, "...\n")
  
  # Define moderator formula
  moderator_formula <- if (!is.null(moderators)) {
    as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  } else {
    as.formula("yi ~ 1")  # Intercept-only model
  }
  
  # Fit the model
  model <- tryCatch({
    rma(
      yi = yi,
      vi = vi,
      mods = moderator_formula,
      random = ~ 1 | id_article,
      data = data,
      method = "REML"
    )
  }, error = function(e) {
    cat("Error for response variable:", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}
```


```{r}
# Fit models for each response variable
model_results <- list()

for (response in response_variables) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Fit the model
  model <- fit_response_variable_rma(data_subset, response, moderators = c("tree_type", "crop_type", "age_system", "season", "soil_texture"))
  
  # Save the model
  model_results[[response]] <- model
}

# Save fitted models
saveRDS(model_results, file = file.path(output_dir, "fitted_rma_models_by_response_variable.rds"))
cat("\nAll models fitted and saved to:", output_dir, "\n")
```
```{r}
# Define response variable
response_var <- "Biodiversity"

# Subset data
data <- meta_data[meta_data$response_variable == response_var, ]

# Define moderator formula (customize as needed)
moderator_formula <- as.formula("yi ~ tree_type + crop_type + age_system + season + soil_texture")

# Fit the model
cat("Fitting model for response variable:", response_var, "...\n")
model_res <- tryCatch({
  rma.mv(
    yi = yi,
    vi = vi,
    mods = moderator_formula,
    random = ~ 1 | exp_id,  # Simplified random-effects structure
    data = data,
    method = "ML",
    control = list(
      optimizer = "optim",
      optim.method = "BFGS",
      iter.max = 1000,
      rel.tol = 1e-8
    )
  )
}, error = function(e) {
  cat("Error fitting model for", response_var, ":", e$message, "\n")
  return(NULL)
})

if (!is.null(model_res)) {
  cat("Model fitting completed for response variable:", response_var, ".\n")
} else {
  stop("Model fitting failed for response variable:", response_var)
}

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


# Compute diagnostics for all models
influence_diagnostics <- bind_rows(
  lapply(names(model_results), function(response) {
    compute_influence_diagnostics_rma(model_results[[response]], response)
  })
)

# Filter out rows with NA studies if needed
influence_diagnostics <- influence_diagnostics %>% filter(!is.na(Study))

# Save influence diagnostics
write.csv(influence_diagnostics, file.path(output_dir, "influence_diagnostics_rma_summary.csv"), row.names = FALSE)
cat("\nInfluence diagnostics saved to:", output_dir, "\n")



##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
```

```{r}
# Function to create and save influence diagnostic plots
plot_influence_diagnostics_rma <- function(diagnostics, response_variable) {
  if (is.null(diagnostics)) {
    cat("No diagnostics available for response variable:", response_variable, "\n")
    return(NULL)
  }
  
  # Filter diagnostics for the response variable
  data <- diagnostics %>% filter(ResponseVariable == response_variable)
  
  # Create plots for residuals and Cook's Distance
  plot <- ggplot(data, aes(x = Study)) +
    geom_point(aes(y = StandardizedResiduals, color = "Standardized Residuals")) +
    geom_point(aes(y = CookDistance, color = "Cook's Distance")) +
    geom_point(aes(y = HatValues, color = "Hat Values")) +
    labs(
      title = paste("Influence Diagnostics -", response_variable),
      x = "Study",
      y = "Value",
      color = "Diagnostics"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the plot
  plot_file <- file.path(influence_dir, paste0("influence_plot_", tolower(gsub(" ", "_", response_variable)), ".png"))
  ggsave(plot_file, plot, width = 10, height = 6, dpi = 300)
  cat("Influence plot saved for response variable:", response_variable, "at", plot_file, "\n")
  
  return(plot)
}

# Generate influence plots for each response variable
plots <- lapply(unique(influence_diagnostics$ResponseVariable), function(response) {
  plot_influence_diagnostics_rma(influence_diagnostics, response)
})

```

```{r}
# Example dataset for visualizing results
forest_data <- model_diagnostics %>% 
  mutate(
    ci.lb = LogLikelihood - 1.96 * sqrt(Tau2),
    ci.ub = LogLikelihood + 1.96 * sqrt(Tau2)
  )

# Create the forest plot
forest_plot <- ggplot(forest_data, aes(x = LogLikelihood, y = ResponseVariable, color = ResponseVariable)) +
  geom_point(size = 3) +  # Effect size points
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.2) +  # Confidence intervals
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +  # Null line
  scale_color_manual(values = custom_colors) +  # Custom colors
  labs(
    title = "Forest Plot: Effects of Silvoarable Agroforestry on Ecosystem Services",
    x = "Effect Size (Log Scale ± 95% CI)",
    y = "Response Variables"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12, hjust = 1),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

# Display the forest plot
print(forest_plot)

```


Correcting unclassified in sub_region

```{r}
# Manually update the sub-region for the specific locations
database_clean_sf <- database_clean_sf %>%
  mutate(
    # Update both sub_region and climate_zone based on location and site
    sub_region = case_when(
      location == "Vézénobres" ~ "Mediterranean Europe",
      location == "Restinclières" ~ "Mediterranean Europe",
      location == "Xinjiang" ~ "Continental Asia",
      site == "Vézénobres" ~ "Mediterranean Europe",
      site == "Restinclières" ~ "Mediterranean Europe",
      site == "Leeds" ~ "Continental Europe",
      site == "Nothern England" ~ "Continental Europe",
      TRUE ~ sub_region
    ))

,
climate_zone = case_when(
  sub_region == "England" ~ 3,  # Example climate zone codes
  sub_region == "China" ~ 7,
  sub_region == "Continental Europe" ~ 5,
  TRUE ~ climate_zone
)
)

# Verify if all missing values for climate_zone are handled
database_clean_sf %>%
  filter(is.na(climate_zone))
```

```{r}
database_clean |> glimpse() 

# database_clean %>%
#   mutate(
#     running_year = as.numeric(experiment_year - min(experiment_year, na.rm = TRUE))
#   ) |> 
#   relocate(running_year, experiment_year)
# 

database_clean %>%
  as.data.frame() |> 
  select(-geometry) |> 
  mutate(
    # Extract numeric years from Date columns
    study_year_start_numeric = as.numeric(format(study_year_start, "%Y")),
    study_year_end_numeric = as.numeric(format(study_year_end, "%Y")),
    
    # Handle cases where study_year_end is NA
    study_year_end_numeric = ifelse(is.na(study_year_end_numeric), 
                                    study_year_start_numeric, 
                                    study_year_end_numeric),
    
    # Calculate the midpoint year as the average of start and end years
    midpoint_year = (study_year_start_numeric + study_year_end_numeric) / 2,
    
    # Running year based on the earliest midpoint year
    running_year_midpoint = midpoint_year - min(midpoint_year, na.rm = TRUE),
    
    # Z-score normalization for running year
    running_year_normalized = scale(running_year_midpoint)
  ) %>%
  select(exp_id, id_article,
         running_year_normalized, midpoint_year, running_year_midpoint, study_year_start_numeric, study_year_end_numeric,
         study_year_start, study_year_end, experiment_year) |> 
  relocate(running_year_normalized, midpoint_year, running_year_midpoint, study_year_start_numeric, study_year_end_numeric,
           study_year_start, study_year_end, experiment_year) |> 
  arrange(study_year_start_numeric) |> 
  glimpse()

```

```{r}
# Fix invalid year data
d <- database_clean %>%
  mutate(
    study_year_start = if_else(study_year_start < as.Date("1900-01-01"), NA, study_year_start),
    study_year_end = if_else(study_year_end < as.Date("1900-01-01"), NA, study_year_end),
    study_year_end = if_else(is.na(study_year_end), study_year_start, study_year_end)
  ) %>%
  
  # Recalculate temporal variables
  mutate(
    study_year_start_numeric = as.numeric(format(study_year_start, "%Y")),
    study_year_end_numeric = as.numeric(format(study_year_end, "%Y")),
    midpoint_year = (study_year_start_numeric + study_year_end_numeric) / 2,
    running_year_midpoint = midpoint_year - min(midpoint_year, na.rm = TRUE),
    running_year_normalized = scale(running_year_midpoint)
  )

# Visualization: Distribution of Midpoint Years
ggplot(d, aes(x = midpoint_year)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
  labs(
    title = "Distribution of Experiment Midpoint Years",
    x = "Midpoint Year",
    y = "Count"
  ) +
  theme_minimal()

# Visualization: Running Year vs Response Variables
ggplot(d, aes(x = running_year_normalized, y = silvo_mean)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Normalized Running Year vs. Silvo Mean",
    x = "Normalized Running Year",
    y = "Silvo Mean"
  ) +
  theme_minimal()
```


database_clean %>%
  as.data.frame() |> 
  select(-geometry) |>
  group_by(id_article, location, experiment_year) %>%
  summarise(exp_id_count = n_distinct(exp_id), .groups = "drop") %>%
  pivot_longer(
    cols = c(id_article, location, experiment_year),
    names_to = "component",
    values_to = "count"
  ) %>%
  ggplot(aes(x = component, y = count, fill = component)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Distribution of exp_id Across Components",
    x = "Component",
    y = "Count of exp_id"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


```{r}
# Check the names of moderator-related columns
moderator_columns <- c("tree_type", "crop_type", "age_system", "soil_texture", "alley_width")  # Replace with actual moderator names

# Prepare data for visualization
moderators_data <- database_clean %>%
  select(exp_id, all_of(moderator_columns)) %>%
  pivot_longer(
    cols = all_of(moderator_columns),
    names_to = "moderator",
    values_to = "value"
  ) %>%
  mutate(missing_count = is.na(value)) %>%
  group_by(exp_id, moderator) %>%
  summarise(missing_count = sum(missing_count), .groups = "drop")

# Visualize missingness for individual moderators
ggplot(moderators_data, aes(x = exp_id, y = missing_count, fill = moderator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Missingness Overview for Individual Moderators",
    x = "Experiment ID (exp_id)",
    y = "Missing Values Count",
    fill = "Moderator"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
```




AFTER MEETING WITH MAARIT



# Dummy non-imputed dataset 
non_imp_dataset_dummy <- database_clean_sd_dummy |> 
  as.data.frame() |> 
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    #location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )

```{r}
impute_and_merge <- function(dataset, moderators, dataset_name = "Dataset") {
  
  cat("Starting imputation for", dataset_name, "...\n")
  
  # Step 1: Prepare data for imputation
  cols_for_impute <- dataset %>%
    select(
      yi, vi,
      id_article, id_obs, exp_id,
      response_variable, all_of(moderators)
    )
  
  # Step 2: Convert categorical variables to factors
  cols_for_impute <- cols_for_impute %>%
    mutate(across(all_of(moderators), as.factor))
  
  # Step 3: Perform multiple imputation using mice
  set.seed(1234)
  imputed_data <- mice(
    cols_for_impute,
    m = 20,         # Number of imputations
    maxit = 100,    # Maximum iterations
    method = 'pmm', # Predictive Mean Matching
    printFlag = FALSE
  )
  
  # Step 4: Extract the first imputed dataset for merging
  completed_data <- complete(imputed_data, 1)
  
  # Step 5: Join the imputed values back to the original dataset
  merged_dataset <- dataset %>%
    left_join(
      completed_data %>%
        select(id_article, id_obs, exp_id, all_of(moderators)),
      by = c("id_article", "id_obs", "exp_id"),
      suffix = c("_original", "_imputed")
    )
  
  # Step 6: Replace missing values in the original columns with imputed values
  for (mod in moderators) {
    original_col <- paste0(mod, "_original")
    imputed_col <- paste0(mod, "_imputed")
    
    if (original_col %in% colnames(merged_dataset) && imputed_col %in% colnames(merged_dataset)) {
      merged_dataset[[mod]] <- ifelse(
        is.na(merged_dataset[[original_col]]),
        merged_dataset[[imputed_col]],
        merged_dataset[[original_col]]
      )
    }
  }
  
  # Step 7: Drop the temporary columns
  merged_dataset <- merged_dataset %>%
    select(-ends_with("_original"), -ends_with("_imputed"))
  
  cat("Imputation completed for", dataset_name, ".\n")
  
  return(merged_dataset)
}
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


# Performing moderator imputations
moderators <- c("tree_type", "crop_type", "age_system", "season", 
                "soil_texture", "no_tree_per_m", "tree_height", "alley_width")

# Impute and merge for non-imputed dataset
non_imp_dataset_imputed <- impute_and_merge(non_imp_dataset, moderators, "Non-Imputed Dataset")

# Impute and merge for imputed dataset
imp_dataset_imputed <- impute_and_merge(imp_dataset, moderators, "Imputed Dataset")


############################################################################################################################
# Helper function to convert numeric imputed values to categorical factors
convert_to_factors <- function(data) {
  # Convert 'no_tree_per_m' to character factors (Low, High)
  data <- data %>%
    mutate(
      no_tree_per_m = case_when(
        no_tree_per_m %in% c(1, "1") ~ "Low",
        no_tree_per_m %in% c(2, "2") ~ "High",
        TRUE ~ as.character(no_tree_per_m)
      ) %>% as.factor()
    )
  
  # Convert 'tree_height' to character factors (Short, Tall)
  data <- data %>%
    mutate(
      tree_height = case_when(
        tree_height %in% c(1, "1") ~ "Short",
        tree_height %in% c(2, "2") ~ "Tall",
        TRUE ~ as.character(tree_height)
      ) %>% as.factor()
    )
  
  # Convert 'alley_width' to character factors (Narrow, Wide)
  data <- data %>%
    mutate(
      alley_width = case_when(
        alley_width %in% c(1, "1") ~ "Narrow",
        alley_width %in% c(2, "2") ~ "Wide",
        TRUE ~ as.character(alley_width)
      ) %>% as.factor()
    )
  
  # Convert 'age_system' to character factors (Narrow, Wide)
  data <- data %>%
    mutate(
      age_system = case_when(
        age_system %in% c(1, "1") ~ "Young",
        age_system %in% c(2, "2") ~ "Medium",
        age_system %in% c(3, "3") ~ "Mature",
        TRUE ~ as.character(age_system)
      ) %>% as.factor()
    )
  
  # Convert 'season' to character factors (Narrow, Wide)
  data <- data %>%
    mutate(
      season = case_when(
        season %in% c(1, "1") ~ "Summer",
        season %in% c(2, "2") ~ "Winter",
        season %in% c(3, "3") ~ "WinterSummer",
        TRUE ~ as.character(season)
      ) %>% as.factor()
    )
  
  return(data)
}

# Apply the conversion function to both datasets
non_imp_dataset_imputed <- convert_to_factors(non_imp_dataset_imputed) |> 
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Effect size measure
    yi, vi,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )


imp_dataset_imputed <- convert_to_factors(imp_dataset_imputed) |> 
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Effect size measure
    yi, vi,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )



############################################################################################################################


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (16/11-24)
# Starting imputation for Non-Imputed Dataset ...
# Advarsel: Number of logged events: 1Imputation completed for Non-Imputed Dataset .
# Starting imputation for Imputed Dataset ...
# Advarsel: Number of logged events: 1Imputation completed for Imputed Dataset .
# Time difference of 1.3963 mins

# Check the structure of the datasets
# str(non_imp_dataset_imputed)
# str(imp_dataset_imputed)
```

Assessing imputation of moderators again

```{r}
# Assessing Moderator missingness

moderators <- c("tree_type", "crop_type", "age_system", "season", 
                "soil_texture", "no_tree_per_m", "tree_height", "alley_width")

# Assess missing data for non-imputed dataset
assess_missing_data(non_imp_dataset_imputed, moderators, "Non-Imputed Dataset")

# Assess missing data for imputed dataset
assess_missing_data(imp_dataset_imputed, moderators, "Imputed Dataset")
```

Additional assessment of the moderator imputation

```{r}
# Function to calculate missing data proportions
calculate_missing_proportions <- function(data, moderators) {
  data %>%
    pivot_longer(cols = all_of(moderators), names_to = "moderator", values_to = "value") %>%
    group_by(response_variable, moderator) %>%
    summarise(
      missing_proportion = mean(is.na(value), na.rm = TRUE)
    )
}

# Function to plot missing data proportions per response variable
plot_missing_proportions <- function(original_data, imputed_data, moderators, dataset_name) {
  cat("\nStarting plot creation for", dataset_name, "...\n")
  
  # Calculate missing proportions for original and imputed datasets
  missing_original <- calculate_missing_proportions(original_data, moderators) %>%
    mutate(data_source = "Original")
  
  missing_imputed <- calculate_missing_proportions(imputed_data, moderators) %>%
    mutate(data_source = "Imputed")
  
  # Combine the results
  combined_missing <- bind_rows(missing_original, missing_imputed)
  
  # Create the plot
  plot <- ggplot(combined_missing, aes(x = response_variable, y = missing_proportion, fill = data_source)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ moderator, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = paste("Proportion of Missing Data per Response Variable -", dataset_name),
      x = "Response Variable",
      y = "Missing Proportion"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  
  cat("\nPlot creation completed for", dataset_name, ".\n")
  
  return(plot)
}

# List of moderators
moderators <- c("tree_type", "crop_type", "age_system", "season", 
                "soil_texture", "no_tree_per_m", "tree_height", "alley_width")

# Create plots for Non-Imputed and Imputed datasets
plot_non_imp <- plot_missing_proportions(non_imp_dataset, non_imp_dataset_imputed, moderators, "Non-Imputed Dataset")
plot_imp <- plot_missing_proportions(imp_dataset, imp_dataset_imputed, moderators, "Imputed Dataset")

# Display the plots side by side
plot_non_imp + plot_imp
```



```{r}
# Function to fit models for each response variable using precomputed v_matrices
fit_response_variable_model <- function(data_subset, response_variable, v_matrix, moderators = NULL) {
  cat("\nFitting model for response variable:", response_variable, "...\n")
  
  # Define the moderator formula
  moderator_formula <- if (!is.null(moderators)) {
    as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  } else {
    as.formula("yi ~ 1")  # Intercept-only model
  }
  
  # Fit a multivariate meta-analytic model using `rma.mv` within a try-catch block
  model <- tryCatch({
    # Fit a random-effects multivariate meta-analysis model
    rma.mv(
      # Dependent variable: effect size (yi)
      yi = yi,
      # Variance-covariance matrix (V): accounts for within-study sampling variance and potential correlations
      V = v_matrix,
      # Moderators: a formula specifying the covariates to include in the model
      mods = moderator_formula,
      # Random-effects structure: defines how the random effects are modeled hierarchically
      random = list(
        ~ 1 | id_article/response_variable,         # Nested random intercept for each response variable within articles
        ~ 1 | exp_id                                # Random intercept for individual experiments
      ),
      # Data: the dataset used for the analysis
      data = data_subset,
      # Method: optimization method for model fitting
      method = "ML",                                # Maximum likelihood estimation
      # Control settings: additional options for the optimization algorithm
      control = list(
        optimizer = "optim",                        # Specify the optimization function to use
        optim.method = "BFGS",                      # Use the Broyden–Fletcher–Goldfarb–Shanno algorithm for optimization
        iter.max = 1000,                            # Maximum number of iterations allowed
        rel.tol = 1e-8                              # Convergence tolerance (stopping criterion for optimization)
      )
    )
  }, error = function(e) {                            # Catch any errors during model fitting
    cat("Error in model fitting:", e$message, "\n") # Print the error message
    return(NULL)                                    # Return NULL if an error occurs
  })
  
  
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}
```




```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################

# Start time tracking
start.time <- Sys.time()

##########################################################################
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################

# Start time tracking
start.time <- Sys.time()

##########################################################################
# Step 1: Check and enforce correct data types
col_for_impute <- database_clean_sd |> 
  as.data.frame() |> 
  select(-geometry) |> 
  select(
    # Columns that need to be imputed
    silvo_se, control_se, 
    # Columns that are used by mice to impute values
    tree_age, crop_type, tree_type, bioclim_sub_regions, experiment_year, alley_width, silvo_n, control_n,
    # IDs that are used to back-link imputed values to the dataset
    id_article, id_obs, treat_id, exp_id
  ) |> 
  mutate(
    silvo_se = as.numeric(silvo_se),
    control_se = as.numeric(control_se),
    silvo_n = as.numeric(silvo_n),
    control_n = as.numeric(control_n),
    tree_age = as.numeric(tree_age),
    crop_type = as.factor(crop_type),
    tree_type = as.factor(tree_type),
    bioclim_sub_regions = as.factor(bioclim_sub_regions),
    alley_width = as.factor(alley_width),
    id_article = as.numeric(id_article),
    id_obs = as.numeric(id_obs),
    treat_id = as.numeric(treat_id),
    exp_id = as.numeric(exp_id)
  )

##########################################################################
# Step 2: Define the function for each imputation method
impute_data <- function(data, method_name) {
  if (method_name == "pmm") {
    # Predictive Mean Matching
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0
    
    # Define imputation method for PMM
    method <- c(
      "silvo_se" = "pmm",
      "control_se" = "pmm",
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",         # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    # Perform imputation using mice
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    # Return the raw mids object for PMM
    return(imputed_mids)
    
  } else if (method_name == "upper_quartile") {
    # Upper Quartile Imputation for Variance
    upper_quartile_variance <- data %>%
      # The 75th percentile represents a value higher than the median, ensuring that the imputed variances 
      # are not unrealistically small while 
      # still grounded in observed data. This helps maintain a conservative weighting in the meta-analysis.
      summarise(across(c(silvo_se, control_se), ~ quantile(.^2, 0.75, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "upper_quartile")
    
    # Impute missing variance with the upper quartile
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), sqrt(upper_quartile_variance$upper_quartile[1]), silvo_se),
        control_se = ifelse(is.na(control_se), sqrt(upper_quartile_variance$upper_quartile[2]), control_se)
      )
    return(data)
    
  } else if (method_name == "mean_imputation") {
    # Example: Mean Imputation
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), mean(silvo_se, na.rm = TRUE), silvo_se),
        control_se = ifelse(is.na(control_se), mean(control_se, na.rm = TRUE), control_se),
        silvo_n = ifelse(is.na(silvo_n), mean(silvo_n, na.rm = TRUE), silvo_n),
        control_n = ifelse(is.na(control_n), mean(control_n, na.rm = TRUE), control_n)
      )
    return(data)
  } else {
    stop("Invalid method name.")
  }
}

##########################################################################
# Step 3: Apply each imputation method
imputation_methods <- c("pmm", "upper_quartile", "mean_imputation")
imputed_datasets <- list()

# Separate storage for the raw mids object from PMM
imputed_mids_pmm <- NULL

for (method_name in imputation_methods) {
  cat("Applying", method_name, "imputation...\n")
  if (method_name == "pmm") {
    # Save the mids object for PMM
    imputed_mids_pmm <- impute_data(col_for_impute, method_name)
    # Convert to a completed dataset for combined list
    imputed_datasets[[method_name]] <- mice::complete(imputed_mids_pmm)
  } else {
    # Directly store the completed dataset for other methods
    imputed_datasets[[method_name]] <- impute_data(col_for_impute, method_name)
  }
}

##########################################################################
# Step 4: Compare results
for (method_name in imputation_methods) {
  cat("\nSummary of Imputed Dataset -", method_name, ":\n")
  print(summary(imputed_datasets[[method_name]]))
}

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")

##########################################################################
# imputed_mids_pmm is the raw mids object for PMM
# imputed_datasets contains completed datasets for all methods

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")
##########################################################################
# Last run (28/11-24)
# Total time taken: 11.16786 sec
# Total time taken: 13.66807 

# Last run (01/01-25)
# Total time taken: 10.50317 
```










```{r}
# Visualize Model Comparisons
# Create visualizations for the AIC and likelihood ratio test (LRT) results from `model_comparisons`.

# Extract AIC values and LRT results
comparison_results <- lapply(names(model_comparisons), function(response) {
  comparison <- model_comparisons[[response]]
  if (!is.null(comparison)) {
    tibble(
      Response = response,
      AIC_Original = comparison$AIC[["Original"]],
      AIC_Alternative = comparison$AIC[["Alternative"]],
      AIC_Minimal = comparison$AIC[["Minimal"]],
      LRT_Original_vs_Alternative = comparison$LR_Original_vs_Alternative$pval,
      LRT_Original_vs_Minimal = comparison$LR_Original_vs_Minimal$pval
    )
  } else {
    tibble(
      Response = response,
      AIC_Original = NA,
      AIC_Alternative = NA,
      AIC_Minimal = NA,
      LRT_Original_vs_Alternative = NA,
      LRT_Original_vs_Minimal = NA
    )
  }
}) %>% bind_rows()

comparison_results |> 
  
  # Melt the data for AIC visualization
  aic_data <- comparison_results %>%
  select(Response, 
         AIC_Original, 
         AIC_Alternative, 
         AIC_Minimal) %>%
  pivot_longer(cols = starts_with("AIC"), names_to = "Model", values_to = "AIC") %>%
  mutate(Model = factor(Model, levels = c("AIC_Original", "AIC_Alternative", "AIC_Minimal"),
                        labels = c("Original", "Alternative", "Minimal")))

# Plot AIC values
plot_aic <- ggplot(aic_data, aes(x = Response, y = AIC, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "AIC Comparisons Across Models",
       x = "Response Variable",
       y = "AIC",
       fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prepare LRT data
lrt_data <- comparison_results %>%
  select(Response, LRT_Original_vs_Alternative, LRT_Original_vs_Minimal) %>%
  pivot_longer(cols = starts_with("LRT"), names_to = "Comparison", values_to = "p_value") %>%
  mutate(Comparison = factor(Comparison, 
                             levels = c("LRT_Original_vs_Alternative", "LRT_Original_vs_Minimal"),
                             labels = c("Original vs. Alternative", "Original vs. Minimal")))

# Plot LRT p-values
plot_lrt <- ggplot(lrt_data, aes(x = Response, y = p_value, color = Comparison)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "LRT P-Values for Model Comparisons",
       x = "Response Variable",
       y = "P-Value",
       color = "Comparison") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plots
print(plot_aic)
print(plot_lrt)

```






























Original Model
This model incorporates two levels of random effects: one nested within articles and response variables, and another at the experiment level.

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################


# Function to fit models for each response variable using precomputed v_matrices
fit_response_variable_model <- function(data_subset, response_variable, v_matrix, moderators = NULL) {
  cat("\nFitting model for response variable:", response_variable, "...\n")
  
  # Define the moderator formula
  moderator_formula <- if (!is.null(moderators)) {
    as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  } else {
    as.formula("yi ~ 1")  # Intercept-only model
  }
  
  # Ensure all moderators are treated as factors
  data_subset <- data_subset %>%
    mutate(across(all_of(moderators), as.factor)) %>%
    as.data.frame()
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      # Dependent variable: effect size
      yi = yi,
      # Variance-covariance matrix for within-study variability
      V = v_matrix,
      # Moderator formula: relationship between the effect size and moderators
      mods = moderator_formula,
      # Random effects structure
      random = list(
        ~ 1 | id_article/response_variable,         # Nested random intercept for each response variable within articles
        ~ 1 | exp_id                                # Random effect: accounts for variability at the experiment level
      ),
      # Data used for model fitting
      data = data_subset,
      # Method for model fitting
      method = "ML",                          # Maximum Likelihood (ML) for parameter estimation
      # Optimization settings
      control = list(
        optimizer = "optim",                  # Optimizer function to use for fitting
        optim.method = "BFGS",                # Broyden–Fletcher–Goldfarb–Shanno algorithm for optimization
        iter.max = 1000,                      # Maximum number of iterations allowed
        rel.tol = 1e-8                        # Convergence tolerance for optimization
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  # Return the model or NULL if fitting failed
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

# Fit models for each response variable using the precomputed v_matrices
model_results <- list()

for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Retrieve the precomputed v_matrix
  v_matrix <- v_matrices[[response]]
  
  # Define moderators
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  # Fit the model for the current response variable
  model <- fit_response_variable_model(data_subset, response, v_matrix, moderators)
  
  # Save the model result
  model_results[[response]] <- model
}

# Save the fitted models to a file
saveRDS(model_results, file = file.path(output_dir, "fitted_models_by_response_variable.rds"))
cat("\nAll models fitted and saved to:", output_dir, "\n")


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go (02/12-2024)
# Time difference of 11.96204 secs

# Last go (01/01-2025)
# Time difference of 8.034523 secs
# Processing response variable: Biodiversity 
# Fitting model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting model for response variable: Greenhouse gas emission ...
# Error in model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting model for response variable: Water quality ...
# Error in model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# All models fitted and saved to: C:/Users/au759124/OneDrive - Aarhus universitet/Documents/Temp_SAF_meta_analysis/DATA/OUTPUT_FROM_R/SAVED_OBJECTS_FROM_R 
# Time difference of 8.034523 secs

# Last go (02/12-2024)
# Processing response variable: Biodiversity 
# Fitting model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting model for response variable: Greenhouse gas emission ...
# Error in model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting model for response variable: Water quality ...
# Error in model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# All models fitted and saved to: C:/Users/au759124/OneDrive - Aarhus universitet/Documents/Temp_SAF_meta_analysis/DATA/OUTPUT_FROM_R/SAVED_OBJECTS_FROM_R 
# Time difference of 9.78518 secs
```

Alternative Model 1: Simplified Random Effects
This model simplifies the random effects structure, focusing only on variability at the experiment level.
```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################


# Function to fit models for each response variable using precomputed v_matrices
fit_response_variable_model <- function(data_subset, response_variable, v_matrix, moderators = NULL) {
  cat("\nFitting model for response variable:", response_variable, "...\n")
  
  # Define the moderator formula
  moderator_formula <- if (!is.null(moderators)) {
    as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  } else {
    as.formula("yi ~ 1")  # Intercept-only model
  }
  
  # Ensure all moderators are treated as factors
  data_subset <- data_subset %>%
    mutate(across(all_of(moderators), as.factor)) %>%
    as.data.frame()
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      # Dependent variable: effect size
      yi = yi,
      # Variance-covariance matrix for within-study variability
      V = v_matrix,
      # Moderator formula: relationship between the effect size and moderators
      mods = moderator_formula,
      # Random effects structure
      random = ~ 1 | exp_id,                 # Random effect: accounts for variability at the experiment level
      # Data used for model fitting
      data = data_subset,
      # Method for model fitting
      method = "ML",                          # Maximum Likelihood (ML) for parameter estimation
      # Optimization settings
      control = list(
        optimizer = "optim",                  # Optimizer function to use for fitting
        optim.method = "BFGS",                # Broyden–Fletcher–Goldfarb–Shanno algorithm for optimization
        iter.max = 1000,                      # Maximum number of iterations allowed
        rel.tol = 1e-8                        # Convergence tolerance for optimization
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  # Return the model or NULL if fitting failed
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

# Fit models for each response variable using the precomputed v_matrices
model_results <- list()

for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Retrieve the precomputed v_matrix
  v_matrix <- v_matrices[[response]]
  
  # Define moderators
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  # Fit the model for the current response variable
  model <- fit_response_variable_model(data_subset, response, v_matrix, moderators)
  
  # Save the model result
  model_results[[response]] <- model
}

# Save the fitted models to a file
saveRDS(model_results, file = file.path(output_dir, "fitted_models_by_response_variable_simplified.rds"))
cat("\nAll models fitted and saved to:", output_dir, "\n")


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go (02/12-2024)
# Time difference of 4.593977 secs
# Processing response variable: Biodiversity 
# Fitting model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting model for response variable: Greenhouse gas emission ...
# Error in model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting model for response variable: Water quality ...
# Error in model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# All models fitted and saved to: C:/Users/au759124/OneDrive - Aarhus universitet/Documents/Temp_SAF_meta_analysis/DATA/OUTPUT_FROM_R/SAVED_OBJECTS_FROM_R 
# Time difference of 4.593977 secs

# Last go (02/12-2024)
# Time difference of 4.643164 secs
# Processing response variable: Biodiversity 
# Fitting model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting model for response variable: Greenhouse gas emission ...
# Error in model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting model for response variable: Water quality ...
# Error in model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# All models fitted and saved to: C:/Users/au759124/OneDrive - Aarhus universitet/Documents/Temp_SAF_meta_analysis/DATA/OUTPUT_FROM_R/SAVED_OBJECTS_FROM_R 
# Time difference of 4.643164 secs
```

Minimally Reduced Model:
  This model further simplifies the random effects structure by removing all nested random effects,
focusing only on the experiment level and an intercept-only model.

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################

# Start time tracking
start.time <- Sys.time()

##########################################################################
# Define the function to fit minimally reduced models for each response variable
fit_minimal_model <- function(data_subset, response_variable, v_matrix, moderators = NULL) {
  cat("\nFitting minimally reduced model for response variable:", response_variable, "...\n")
  
  # Define the intercept-only formula
  minimal_formula <- if (!is.null(moderators)) {
    as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  } else {
    as.formula("yi ~ 1")
  }
  
  # Ensure all moderators are treated as factors
  if (!is.null(moderators)) {
    data_subset <- data_subset %>%
      mutate(across(all_of(moderators), as.factor)) %>%
      as.data.frame()
  }
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      # Dependent variable: effect size
      yi = yi,
      # Variance-covariance matrix for within-study variability
      V = v_matrix,
      # Moderator formula: Intercept-only model
      mods = minimal_formula,
      # Random effects structure
      random = ~ 1 | exp_id,                 # Random effect: accounts for variability at the experiment level
      # Data used for model fitting
      data = data_subset,
      # Method for model fitting
      method = "ML",                          # Maximum Likelihood (ML) for parameter estimation
      # Optimization settings
      control = list(
        optimizer = "optim",                  # Optimizer function to use for fitting
        optim.method = "BFGS",                # Broyden–Fletcher–Goldfarb–Shanno algorithm for optimization
        iter.max = 1000,                      # Maximum number of iterations allowed
        rel.tol = 1e-8                        # Convergence tolerance for optimization
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  # Return the model or NULL if fitting failed
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

##########################################################################
# Fit minimally reduced models for each response variable using the precomputed v_matrices
minimal_model_results <- list()

for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Retrieve the precomputed v_matrix
  v_matrix <- v_matrices[[response]]
  
  # Define moderators (if applicable)
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  # Fit the minimally reduced model for the current response variable
  model <- fit_minimal_model(data_subset, response, v_matrix, moderators)
  
  # Save the model result
  minimal_model_results[[response]] <- model
}

##########################################################################
# Save the fitted minimally reduced models to a file
saveRDS(minimal_model_results, file = file.path(output_dir, "fitted_models_by_response_variable_minimal.rds"))
cat("\nAll minimally reduced models fitted and saved to:", output_dir, "\n")

##########################################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")
##########################################################################
# Last go (02/12-2024)
# Total time taken: 4.232735 
# Processing response variable: Biodiversity 
# Fitting minimally reduced model for response variable: Biodiversity ...
# Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting minimally reduced model for response variable: Greenhouse gas emission ...
# Model fitting completed for response variable: Greenhouse gas emission .
# Processing response variable: Product quality 
# Fitting minimally reduced model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting minimally reduced model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting minimally reduced model for response variable: Pest and Disease ...
# Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting minimally reduced model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting minimally reduced model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Water quality .
# All minimally reduced models fitted and saved to: C:/Users/au759124/OneDrive - Aarhus universitet/Documents/Temp_SAF_meta_analysis/DATA/OUTPUT_FROM_R/SAVED_OBJECTS_FROM_R 
# Total time taken: 4.232735 
```







```{r}
# Compare models for each response variable
model_comparisons <- list()

for (response in names(original_model_results)) {
  cat("\nComparing models for response variable:", response, "\n")
  
  original_model <- original_model_results[[response]]
  alternative_model <- simplified_model_results[[response]]
  minimal_model <- minimal_model_results[[response]]
  
  if (!is.null(original_model) & !is.null(alternative_model) & !is.null(minimal_model)) {
    # Extract AIC for each model, handle NULL cases
    aic_values <- c(
      "Original" = if (!is.null(original_model)) original_model$aic else NA,
      "Alternative" = if (!is.null(alternative_model)) alternative_model$aic else NA,
      "Minimal" = if (!is.null(minimal_model)) minimal_model$aic else NA
    )
    
    # Perform likelihood ratio tests, handle potential errors
    lr_original_vs_alternative <- tryCatch(
      anova(original_model, alternative_model),
      error = function(e) { cat("LR test failed for Original vs. Alternative:", e$message, "\n"); NULL }
    )
    lr_original_vs_minimal <- tryCatch(
      anova(original_model, minimal_model),
      error = function(e) { cat("LR test failed for Original vs. Minimal:", e$message, "\n"); NULL }
    )
    
    # Store results
    model_comparisons[[response]] <- list(
      AIC = aic_values,
      LR_Original_vs_Alternative = lr_original_vs_alternative,
      LR_Original_vs_Minimal = lr_original_vs_minimal
    )
    
    # Print AIC values
    cat("AIC Values:\n", aic_values, "\n")
  } else {
    cat("Model fitting failed for one or more models for", response, "\n")
  }
}

# Display results for all response variables
model_comparisons |> glimpse() |> head()
```

```{r}
model_comparisons |> str()
```

```{r}
# Inspect log-likelihood values and model structures
inspect_model_fit <- function(model, model_name) {
  if (!is.null(model)) {
    cat("\nModel:", model_name, "\n")
    cat("Log-Likelihood:", logLik(model), "\n")
    cat("Fixed Effects:\n")
    print(model$beta)
    cat("Random Effects:\n")
    print(model$sigma2)
  } else {
    cat("\nModel:", model_name, "is NULL\n")
  }
}

##################################################################################
# Replace with the specific response variable to inspect
response_variable <- "Crop yield"  

inspect_model_fit(original_model_results[[response_variable]], "Original")
inspect_model_fit(simplified_model_results[[response_variable]], "Alternative")
inspect_model_fit(minimal_model_results[[response_variable]], "Minimal")
```


```{r}
##########################################################################
# Compare models systematically for each response variable
##########################################################################

# Initialize an empty list to store model comparison results
meta_analysis_model_comparisons <- list()

# Loop through each response variable
for (response in names(full_model_results)) {
  cat("\nComparing models for response variable:", response, "\n")
  
  # Retrieve models for the current response variable
  full_model <- full_model_results[[response]]
  simplified_model <- simplified_model_results[[response]]
  minimal_model <- minimal_model_results[[response]]
  fixed_effects_model <- fixed_effects_model_results[[response]]
  
  if (!is.null(full_model) & !is.null(simplified_model) & !is.null(minimal_model) & !is.null(fixed_effects_model)) {
    
    # Extract AIC for each model, handle NULL cases
    aic_values <- c(
      "Full" = if (!is.null(full_model)) full_model$aic else NA,
      "Simplified" = if (!is.null(simplified_model)) simplified_model$aic else NA,
      "Minimal" = if (!is.null(minimal_model)) minimal_model$aic else NA,
      "Fixed Effects Only" = if (!is.null(fixed_effects_model)) fixed_effects_model$aic else NA
    )
    
    # Perform likelihood ratio tests, handle potential errors
    lr_full_vs_simplified <- tryCatch(
      anova(full_model, simplified_model),
      error = function(e) { cat("LR test failed for Full vs. Simplified:", e$message, "\n"); NULL }
    )
    lr_full_vs_minimal <- tryCatch(
      anova(full_model, minimal_model),
      error = function(e) { cat("LR test failed for Full vs. Minimal:", e$message, "\n"); NULL }
    )
    lr_full_vs_fixed <- tryCatch(
      anova(full_model, fixed_effects_model),
      error = function(e) { cat("LR test failed for Full vs. Fixed Effects Only:", e$message, "\n"); NULL }
    )
    
    # Store results
    meta_analysis_model_comparisons[[response]] <- list(
      AIC = aic_values,
      LR_Full_vs_Simplified = lr_full_vs_simplified,
      LR_Full_vs_Minimal = lr_full_vs_minimal,
      LR_Full_vs_Fixed = lr_full_vs_fixed
    )
    
    # Print AIC values
    cat("AIC Values:\n", aic_values, "\n")
  } else {
    cat("Model fitting failed for one or more models for", response, "\n")
  }
}

##########################################################################
# Display comparison results summary
##########################################################################
# Print a glimpse of the comparison results
meta_analysis_model_comparisons |> glimpse()

##########################################################################
# Save comparison results to file for further analysis
##########################################################################
saveRDS(meta_analysis_model_comparisons, file = "meta_analysis_model_comparison_results.rds")
cat("Model comparison results saved to model_comparison_results.rds\n")
```


```{r}
##########################################################################
# Visualizations for Model Comparisons
##########################################################################

# Load necessary libraries
library(ggplot2)

##########################################################################
# 1. AIC Comparison Plot
##########################################################################
# Combine AIC values into a data frame for visualization
aic_df <- do.call(rbind, lapply(meta_analysis_model_comparisons, function(x) {
  if (!is.null(x$AIC)) {
    data.frame(
      Response = names(meta_analysis_model_comparisons),
      Model = names(x$AIC),
      AIC = x$AIC
    )
  }
}))

# Plot AIC values
ggplot(aic_df, aes(x = Response, y = AIC, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "AIC Comparison Across Models", x = "Response Variable", y = "AIC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################
# 2. Likelihood Ratio Test (LRT) Results
##########################################################################
# Extract p-values from LRT results
lrt_pvalues <- do.call(rbind, lapply(meta_analysis_model_comparisons, function(x) {
  if (!is.null(x$LR_Full_vs_Simplified) && !is.null(x$LR_Full_vs_Minimal)) {
    data.frame(
      Response = names(meta_analysis_model_comparisons),
      Comparison = c("Full vs Simplified", "Full vs Minimal"),
      PValue = c(x$LR_Full_vs_Simplified$pval, x$LR_Full_vs_Minimal$pval)
    )
  }
}))

# Plot p-values
ggplot(lrt_pvalues, aes(x = Response, y = -log10(PValue), color = Comparison)) +
  geom_point(size = 3) +
  labs(title = "Likelihood Ratio Test P-Values", x = "Response Variable", y = "-log10(P-Value)") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################
# 3. Residual Variance (QE) Comparison
##########################################################################
# Extract QE values
qe_df <- do.call(rbind, lapply(meta_analysis_model_comparisons, function(x) {
  if (!is.null(x$LR_Full_vs_Simplified)) {
    data.frame(
      Response = names(meta_analysis_model_comparisons),
      Model = c("Full", "Simplified", "Minimal", "Fixed Effects"),
      QE = c(x$LR_Full_vs_Simplified$QE.f, x$LR_Full_vs_Simplified$QE.r,
             x$LR_Full_vs_Minimal$QE.r, x$LR_Full_vs_Fixed$QE.r)
    )
  }
}))

# Plot QE values
ggplot(qe_df, aes(x = Response, y = QE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Residual Variance (QE) Comparison", x = "Response Variable", y = "Residual Variance (QE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################################################################
# 4. R-Squared Values
##########################################################################
# Extract R-squared values
r2_df <- do.call(rbind, lapply(meta_analysis_model_comparisons, function(x) {
  if (!is.null(x$LR_Full_vs_Simplified)) {
    data.frame(
      Response = names(meta_analysis_model_comparisons),
      Model = c("Full", "Simplified", "Minimal", "Fixed Effects"),
      R2 = c(x$LR_Full_vs_Simplified$R2, NA, NA, NA) # Add R2 if available
    )
  }
}))

# Plot R-squared values
ggplot(r2_df, aes(x = Response, y = R2, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "R-Squared Comparison Across Models", x = "Response Variable", y = "R-Squared") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

```{r}
### Visual Evaluation Protocol: AIC and BIC Comparison

# Function to create a performance comparison table for individual response variables
compare_models <- function(original_model_list, simplified_model_list, response_variable) {
  original_model <- original_model_list[[response_variable]]
  simplified_model <- simplified_model_list[[response_variable]]
  
  if (is.null(original_model) || is.null(simplified_model)) {
    cat("Model missing for response variable:", response_variable, "\n")
    return(NULL)
  }
  
  performance_metrics <- tibble(
    ResponseVariable = response_variable,
    Model = c("Original", "Simplified"),
    AIC = c(AIC(original_model), AIC(simplified_model)),
    BIC = c(BIC(original_model), BIC(simplified_model))
  )
  return(performance_metrics)
}

# Visualize AIC and BIC side-by-side for all response variables
plot_model_comparison_by_response <- function(performance_comparison) {
  performance_long <- performance_comparison %>%
    pivot_longer(cols = c(AIC, BIC), names_to = "Metric", values_to = "Value") %>%
    mutate(ResponseVariable = as.factor(ResponseVariable))
  
  ggplot(performance_long, aes(x = Model, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ResponseVariable, scales = "free_y") +
    labs(
      title = "Model Performance Comparison by Response Variable",
      x = "Model",
      y = "Value",
      fill = "Metric"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
}

# Example Usage
# Assuming `original_model_results` and `simplified_model_results` are lists of model objects by response variable
response_variables <- names(original_model_results)

# Generate performance metrics for all response variables
all_performance_metrics <- lapply(response_variables, function(rv) {
  compare_models(original_model_results, simplified_model_results, rv)
})

# Combine all metrics into a single data frame
all_performance_metrics <- bind_rows(all_performance_metrics)

# Print the performance metrics
glimpse(all_performance_metrics)
```
```{r}

# Generate the visualization
plot_model_comparison_by_response(all_performance_metrics)
```

```{r}
summary(original_model)$random
summary(simplified_model)$random
```










```{r}
##########################################################################
# Systematic Comparison of Models for Each Response Variable
##########################################################################
# Perform likelihood ratio tests for each response variable with additional validation
lrt_results <- lapply(names(full_model_results), function(response) {
  tryCatch({
    full_model <- full_model_results[[response]]
    simplified_model <- simplified_model_results[[response]]
    minimal_model <- minimal_model_results[[response]]
    fixed_model <- fixed_effects_model_results[[response]]
    
    # Debug: Check if models are non-NULL
    if (is.null(full_model) || is.null(simplified_model) || is.null(minimal_model) || is.null(fixed_model)) {
      stop("One or more models are NULL for response: ", response)
    }
    
    # Debug: Check data consistency
    if (!identical(length(full_model$yi), length(simplified_model$yi)) || 
        !identical(length(full_model$vi), length(simplified_model$vi))) {
      stop("Mismatch in data lengths for full vs. simplified model for response: ", response)
    }
    if (!identical(length(full_model$yi), length(minimal_model$yi)) || 
        !identical(length(full_model$vi), length(minimal_model$vi))) {
      stop("Mismatch in data lengths for full vs. minimal model for response: ", response)
    }
    if (!identical(length(full_model$yi), length(fixed_model$yi)) || 
        !identical(length(full_model$vi), length(fixed_model$vi))) {
      stop("Mismatch in data lengths for full vs. fixed model for response: ", response)
    }
    
    # Run ANOVA
    simplified_lrt <- anova(full_model, simplified_model) $`Pr(>Chi)`[2]
    minimal_lrt <- anova(full_model, minimal_model)$`Pr(>Chi)`[2]
    fixed_lrt <- anova(full_model, fixed_model)$`Pr(>Chi)`[2]
    
    data.frame(
      Response = response,
      LRT_Full_vs_Simplified = simplified_lrt,
      LRT_Full_vs_Minimal = minimal_lrt,
      LRT_Full_vs_Fixed = fixed_lrt
    )
  }, error = function(e) {
    message("Error processing response variable: ", response, " - ", e$message)
    NULL
  })
})

# Filter out NULL elements and validate results
lrt_results <- lrt_results[!sapply(lrt_results, is.null)]
if (length(lrt_results) > 0) {
  lrt_results <- do.call(rbind, lrt_results)
  lrt_results <- lrt_results %>% mutate(across(starts_with("LRT"), ~ -log10(.)))
} else {
  message("No valid results to process.")
  lrt_results <- data.frame()
}
```

```{r}
full_ids <- full_model_results[["Biodiversity"]]$data$id_obs
minimal_ids <- minimal_model_results[["Biodiversity"]]$data$id_obs

extra_ids_in_minimal <- setdiff(minimal_ids, full_ids)
missing_ids_in_minimal <- setdiff(full_ids, minimal_ids)

message("Extra IDs in Minimal Model: ", length(extra_ids_in_minimal))
message("Missing IDs in Minimal Model: ", length(missing_ids_in_minimal))

# Inspect the discrepancies
extra_ids_in_minimal
missing_ids_in_minimal
```
```{r}
minimal_model_results[["Biodiversity"]] <- update(
  minimal_model_results[["Biodiversity"]],
  data = full_model_results[["Biodiversity"]]$data
)
```


#############
# STEP 5
##########################################################################################################################################
KEY INFLUENCE DIAGNOSTICS ON EACH SUBSET - SIMPLIFIED MODEL FITTING 
##########################################################################################################################################


```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

# Fit models for each response variable
model_results_infdia <- list()

for (response in unique(meta_data$response_variable)) {
  cat("\nProcessing response variable:", response, "...\n")
  
  # Subset data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Fit the model with random effects for `id_article`
  res <- tryCatch({
    rma(yi = yi, 
        vi = vi, 
        # Add study-level random effect
        random = ~ 1 | id_article,  
        data = data_subset, 
        # Restricted ML
        method = "REML")
  }, error = function(e) {
    cat("Model fitting failed for", response, ":", e$message, "\n")
    return(NULL)
  })
  
  # Save the fitted model
  model_results_infdia[[response]] <- res
}

# Recompute influence diagnostics
influence_diagnostics <- list()

for (response in names(model_results_infdia)) {
  cat("\nComputing influence diagnostics for:", response, "...\n")
  
  model <- model_results_infdia[[response]]
  
  if (!is.null(model)) {
    inf <- tryCatch({
      influence(model)
    }, error = function(e) {
      cat("Influence diagnostics failed for:", response, ":", e$message, "\n")
      return(NULL)
    })
    influence_diagnostics[[response]] <- inf
  } else {
    cat("Skipping influence diagnostics for:", response, "due to missing model.\n")
  }
}


# Perform leave-one-out analysis
leave1out_results <- list()

for (response in names(model_results_infdia)) {
  cat("\nRunning Leave-One-Out for:", response, "...\n")
  
  model <- model_results_infdia[[response]]
  
  if (!is.null(model)) {
    leave1out_results[[response]] <- leave1out(model)
  } else {
    cat("Skipping Leave-One-Out for:", response, "due to missing model.\n")
  }
}


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################

# Last go (02/12-2024)
# Time difference of 1.117442 mins
# Time difference of 3.097009 mins
# Time difference of 2.744736 mins

# Last go (01/01-2025)
# Time difference of 2.148423 mins
# Processing response variable: Biodiversity ...
# Advarsel: Extra argument ('random') disregarded.
# Processing response variable: Crop yield ...
# Advarsel: Extra argument ('random') disregarded.Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.
# Processing response variable: Water quality ...
# Advarsel: Extra argument ('random') disregarded.Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.
# Processing response variable: Pest and Disease ...
# Advarsel: Extra argument ('random') disregarded.
# Processing response variable: Soil quality ...
# Advarsel: Extra argument ('random') disregarded.Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.
# Processing response variable: Greenhouse gas emission ...
# Advarsel: Extra argument ('random') disregarded.
# Processing response variable: Product quality ...
# Advarsel: Extra argument ('random') disregarded.
# Computing influence diagnostics for: Biodiversity ...
# Computing influence diagnostics for: Crop yield ...
# Computing influence diagnostics for: Water quality ...
# Computing influence diagnostics for: Pest and Disease ...
# Computing influence diagnostics for: Soil quality ...
# Computing influence diagnostics for: Greenhouse gas emission ...
# Computing influence diagnostics for: Product quality ...
# Running Leave-One-Out for: Biodiversity ...
# Running Leave-One-Out for: Crop yield ...
# Running Leave-One-Out for: Water quality ...
# Running Leave-One-Out for: Pest and Disease ...
# Running Leave-One-Out for: Soil quality ...
# Running Leave-One-Out for: Greenhouse gas emission ...
# Running Leave-One-Out for: Product quality ...
# Time difference of 2.148423 mins
```


=======
  # Last go: (17/11-24)
  # Time difference of 25.3915 secs
  # 
  # Calculating Bootstrap Confidence Intervals for Non-Imputed Dataset ...
  # Bootstrap Confidence Intervals Calculation Complete for Non-Imputed Dataset .
  # 
  # Forest Plot for Non-Imputed Dataset :
  # 
  # Calculating Bootstrap Confidence Intervals for Imputed Dataset ...
  # Bootstrap Confidence Intervals Calculation Complete for Imputed Dataset .
  # 
  # Forest Plot for Imputed Dataset :
  # Advarsel: longer object length is not a multiple of shorter object length
  # Calculating Bootstrap Confidence Intervals for Non-Imputed Imputed Dataset ...
  # Bootstrap Confidence Intervals Calculation Complete for Non-Imputed Imputed Dataset .
  # 
  # Forest Plot for Non-Imputed Imputed Dataset :
  # 
  # Calculating Bootstrap Confidence Intervals for Imputed Imputed Dataset ...
  # Bootstrap Confidence Intervals Calculation Complete for Imputed Imputed Dataset .
  # 
  # Forest Plot for Imputed Imputed Dataset :
  
  Saving plots


Fit the simplified multivariate random-effects model for diagnostics

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################

# Helper function to run the multivariate random-effects model
run_meta_analysis <- function(data, dataset_name) {
  cat("Running analysis for", dataset_name, "\n")
  
  # Step 1: Calculate the variance-covariance matrix if needed
  V_matrix <- as.matrix(data$vi) # Example assumes `vi` is already correct
  
  # Step 2: Fit the simplified multivariate random-effects model
  model_meta_diagnostics <- tryCatch({
    rma(
      yi = data$yi,
      vi = data$vi,
      random = list(
        ~ 1 | id_article,
        ~ 1 | id_article/response_variable,
        ~ 1 | exp_id
      ),
      data = data,
      method = "ML"
    )
  }, error = function(e) {
    stop("Error in model fitting: ", e$message)
  })
  
  # Step 3: Compute influence diagnostics
  inf <- influence(model_meta_diagnostics)
  
  # Return the model and diagnostics
  return(list(model = model_meta_diagnostics, influence = inf))
}

# Step 1: Apply meta-analysis function to each dataset
results <- lapply(names(datasets), function(dataset_name) {
  tryCatch({
    run_meta_analysis(datasets[[dataset_name]], dataset_name)
  }, error = function(e) {
    cat("Error in dataset", dataset_name, ":", e$message, "\n")
    return(NULL)
  })
})

# Step 2: Name the results list
names(results) <- names(datasets)


##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (27/11-24)
# Time difference of 2.942727 hours
```


```{r}
saveRDS(results, file = "meta_analysis_diagnosis_on_datasets_results.rds")
```


```{r}
# Step 3: Plot influence diagnostics
par(mfrow = c(8, 1), oma = c(2, 2, 2, 2)) # Set up plotting area
for (dataset_name in names(results)) {
  if (!is.null(results[[dataset_name]])) {
    plot(results[[dataset_name]]$influence) # Default plot
    mtext(paste("Influence Diagnostics for", dataset_name), side = 3, line = 0.5, outer = FALSE)
  }
}


# Optional: Inspect the results
# str(results)
```

```{r}
datasets_effect_sizes$non_imp_dataset %>% glimpse()
```












##########################################################################################################################################
SAVING DATASETS AND MODEL OBJECTS
##########################################################################################################################################

```{r}
# Save the results
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
saveRDS(model_results_infdia, file = file.path(output_dir, "simplified_fitted_models.rds"))
saveRDS(influence_diagnostics, file = file.path(output_dir, "simplified_influence_diagnostics.rds"))
cat("\nModels and influence diagnostics saved to:", output_dir, "\n")
```

```{r}
influence_diagnostics |> str()
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

# Generate plots for influence diagnostics
plot_dir <- file.path(output_dir, "Influence_Diagnostics_Plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

for (response in names(influence_diagnostics)) {
  cat("\nGenerating influence diagnostic plots for:", response, "...\n")
  
  inf <- influence_diagnostics[[response]]
  
  if (!is.null(inf)) {
    tryCatch({
      par(mfrow = c(8, 1))
      plot(inf)
      dev.copy(jpeg, file = file.path(plot_dir, paste0("influence_plot_", gsub(" ", "_", response), ".jpg")))
      dev.off()
    }, error = function(e) {
      cat("Plotting failed for", response, ":", e$message, "\n")
    })
  } else {
    cat("No influence diagnostics available for:", response, "\n")
  }
}
cat("\nInfluence diagnostic plots saved to:", plot_dir, "\n")

##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go (02/12-2024)
# Time difference of 49.21399 secs
```
```{r}
model_results |> str()
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



# Custom Leave-One-Out for rma.mv models
leave1out_mv <- function(model, data, study_id_col) {
  unique_studies <- unique(data[[study_id_col]])
  results <- list()
  
  for (study in unique_studies) {
    cat("Leaving out study:", study, "...\n")
    
    # Subset data to exclude the current study
    data_subset <- data[data[[study_id_col]] != study, ]
    
    # Refit the model without the excluded study
    tryCatch({
      refit_model <- rma.mv(yi = model$yi,
                            V = model$V,
                            mods = model$X,
                            random = model$s.names,
                            data = data_subset,
                            method = "ML")
      
      # Store refit model's coefficients
      results[[as.character(study)]] <- list(
        coefficients = refit_model$b,
        fit_stats = refit_model$fit.stats
      )
    }, error = function(e) {
      cat("Error with study:", study, "->", e$message, "\n")
      results[[as.character(study)]] <- NULL
    })
  }
  
  return(results)
}

# Perform Leave-One-Out for all response variables
leave1out_results <- list()

for (response in names(model_results)) {
  cat("\nPerforming Leave-One-Out for:", response, "...\n")
  
  model <- model_results[[response]]
  
  if (!is.null(model)) {
    leave1out_results[[response]] <- leave1out_mv(
      model = model,
      data = model$data,
      study_id_col = "id_article"
    )
  } else {
    cat("Skipping Leave-One-Out for:", response, "due to missing model.\n")
  }
}




##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go (01/01-2025)
# Time difference of  mins
```

```{r}
# Function to extract influence diagnostic data
extract_diagnostics <- function(inf_obj, response_variable) {
  if (is.null(inf_obj)) {
    cat("No influence diagnostics available for", response_variable, "\n")
    return(NULL)
  }
  
  # Extract diagnostics safely and pad missing elements with NA
  tryCatch({
    n_studies <- length(inf_obj$inf$rstudent)  # Total number of studies
    
    diagnostics <- data.frame(
      Study = if (!is.null(rownames(inf_obj$inf$rstudent))) rownames(inf_obj$inf$rstudent) else seq_len(n_studies),
      rstudent = inf_obj$inf$rstudent,
      dffits = inf_obj$inf$dffits,
      cook.d = inf_obj$inf$cook.d,
      cov.r = inf_obj$inf$cov.r,
      tau2.del = inf_obj$inf$tau2.del,
      QE.del = inf_obj$inf$QE.del,
      hat = inf_obj$inf$hat,
      weight = inf_obj$inf$weight,
      ResponseVariable = response_variable
    )
    
    return(diagnostics)
  }, error = function(e) {
    cat("Error extracting diagnostics for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
}
```

```{r}
# Combine diagnostics into a single data frame
# Combine diagnostics into a single data frame
diagnostics_list <- lapply(names(influence_diagnostics), function(response) {
  inf <- influence_diagnostics[[response]]
  extract_diagnostics(inf, response)
})

# Filter out NULL entries
diagnostics_data <- do.call(rbind, diagnostics_list[!sapply(diagnostics_list, is.null)])

# Check the resulting data frame
diagnostics_data |> glimpse()
```

```{r}
# Save diagnostics dataset
# diagnostics_list |> str()

# Define the output directory
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

# Save the diagnostics list as an RDS file
saveRDS(diagnostics_list, file = file.path(output_dir, "diagnostics_list.rds"))
cat("Diagnostics list saved as RDS file to:", file.path(output_dir, "diagnostics_list.rds"), "\n")
```


```{r}
# Check the distribution of diagnostics by response variable
table(diagnostics_data$ResponseVariable)

# Summarize key diagnostics for each response variable
summary_stats <- diagnostics_data %>%
  group_by(ResponseVariable) %>%
  summarise(
    Mean_rstudent = mean(rstudent, na.rm = TRUE),
    Max_rstudent = max(rstudent, na.rm = TRUE),
    Mean_dffits = mean(dffits, na.rm = TRUE),
    Max_dffits = max(dffits, na.rm = TRUE),
    Mean_cook.d = mean(cook.d, na.rm = TRUE),
    Max_cook.d = max(cook.d, na.rm = TRUE),
    .groups = "drop"
  )

summary_stats
```
```{r}
ggplot(diagnostics_data, aes(x = Study, y = rstudent, color = ResponseVariable)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  facet_wrap(~ ResponseVariable, scales = "free") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Standardized Residuals by Study",
    x = "Study ID",
    y = "Standardized Residuals"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )
```
```{r}
ggplot(diagnostics_data, aes(x = Study, y = cook.d, color = ResponseVariable)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  facet_wrap(~ ResponseVariable, scales = "free") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cook's Distance by Study",
    x = "Study ID",
    y = "Cook's Distance"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )
```

```{r}
# Define thresholds
thresholds <- diagnostics_data %>%
  mutate(
    IsInfluential = (abs(rstudent) > 2) | (cook.d > 0.5)
  )

# Check how many studies are flagged as influential
table(thresholds$ResponseVariable, thresholds$IsInfluential)

# Save flagged data for review
output_dir <- here::here("DATA", "OUTPUT_FROM_R")
write.csv(thresholds, file.path(output_dir, "flagged_influential_studies.csv"), row.names = FALSE)
```






```{r}
# Define the output directory an file path for the plot
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "FIGURES")
output_file <- file.path(output_dir, "leave_one_out_effect_sizes.png")

# Save the plot to the output directory
ggsave(output_file, plot = loo_plot, width = 12, height = 8, dpi = 300)
cat("Plot saved to:", output_file, "\n")
```


```{r}
# Inspect the diagnostics data structure for a single response variable
str(diagnostics_list[[1]])
```


```{r}
# Define a threshold for Cook's Distance
cooks_threshold <- 0.8  # Adjust as necessary

# Identify influential studies
influential_studies <- diagnostics_data %>%
  filter(cook.d > cooks_threshold) %>%
  distinct(Study, ResponseVariable)

# Map back to `meta_data`
mapped_influential <- meta_data %>%
  semi_join(influential_studies, by = c("id_article" = "Study"))

# View the result
print(mapped_influential)
```



##########################################################################################################################################
FOR ALL RESPONSE VARIABLES
##########################################################################################################################################

```{r}
# Step 1: Validate Model Components
validate_model_components <- function(model) {
  data <- model$data
  v_matrix <- model$V
  
  if (is.null(data)) stop("Data is NULL for the model.")
  if (is.null(v_matrix)) stop("V matrix is NULL for the model.")
  
  if (nrow(v_matrix) != nrow(data) || ncol(v_matrix) != nrow(data)) {
    stop("Dimension mismatch: V matrix does not align with data for model.")
  }
}

# Step 2: Filter and Recreate Models
filter_and_recreate_model <- function(model, common_ids) {
  data <- model$data
  v_matrix <- model$V
  
  # Filter data and V matrix
  filtered_data <- data[data$id_obs %in% common_ids, ]
  filtered_v <- v_matrix[data$id_obs %in% common_ids, data$id_obs %in% common_ids, drop = FALSE]
  
  # Validate filtered components
  if (nrow(filtered_data) != nrow(filtered_v)) {
    stop("Mismatch between filtered data and V matrix dimensions.")
  }
  
  # Recreate model
  rma.mv(
    yi = filtered_data$yi,
    V = filtered_v,
    mods = formula(model$mods),
    random = model$random,
    method = "ML",
    data = filtered_data
  )
}

# Step 3: Process ANOVA Per Response
process_anova_per_response <- function(full_models, other_models) {
  aic_differences <- lapply(names(full_models), function(response) {
    tryCatch({
      # Extract and validate models
      full_model <- full_models[[response]]
      reduced_model <- other_models[[response]]
      
      validate_model_components(full_model)
      validate_model_components(reduced_model)
      
      # Find common IDs
      full_data <- full_model$data
      reduced_data <- reduced_model$data
      common_ids <- intersect(full_data$id_obs, reduced_data$id_obs)
      
      message("Response: ", response, " - Common IDs: ", length(common_ids))
      
      if (length(common_ids) == 0) stop("No matching observations for ", response)
      
      # Recreate models with filtered data
      updated_full_model <- filter_and_recreate_model(full_model, common_ids)
      updated_reduced_model <- filter_and_recreate_model(reduced_model, common_ids)
      
      # Perform ANOVA
      return(process_anova(updated_full_model, updated_reduced_model))
    }, error = function(e) {
      message("Error for response ", response, ": ", e$message)
      return(NA)
    })
  })
  
  names(aic_differences) <- names(full_models)
  return(aic_differences)
}

# Step 4: Validate Components for Full and Simplified Models
lapply(full_model_results, function(model) {
  tryCatch({
    validate_model_components(model)
  }, error = function(e) {
    message("Validation failed: ", e$message)
  })
})

lapply(simplified_model_results, function(model) {
  tryCatch({
    validate_model_components(model)
  }, error = function(e) {
    message("Validation failed: ", e$message)
  })
})

# Step 5: Process AIC Differences
simplified_aic_diffs <- process_anova_per_response(full_model_results, simplified_model_results)
minimal_aic_diffs <- process_anova_per_response(full_model_results, minimal_model_results)
fixed_aic_diffs <- process_anova_per_response(full_model_results, fixed_effects_model_results)

# Step 6: Combine and Display Results
aic_differences_df <- data.frame(
  Response = names(simplified_aic_diffs),
  Simplified = unlist(simplified_aic_diffs),
  Minimal = unlist(minimal_aic_diffs),
  Fixed = unlist(fixed_aic_diffs)
)

# Step 7: Log Summary of Results
if (any(is.na(aic_differences_df))) {
  message("Some models failed during processing. Review logs for details.")
}

print(aic_differences_df)

```


```{r}
# Function to extract key diagnostics from a fitted model
extract_model_diagnostics <- function(model, response_variable) {
  if (is.null(model)) {
    return(data.frame(
      ResponseVariable = response_variable,
      AIC = NA,
      BIC = NA,
      LogLikelihood = NA,
      Tau2 = NA,
      I2 = NA,
      QM = NA,
      QMp = NA
    ))
  }
  
  # Extract diagnostics
  aic <- AIC(model)
  bic <- BIC(model)
  log_likelihood <- as.numeric(logLik(model))
  tau2 <- sum(model$sigma2)
  i2 <- round((tau2 / (tau2 + mean(model$vi))) * 100, 1)
  qm <- model$QM
  qmp <- model$QMp
  
  data.frame(
    ResponseVariable = response_variable,
    AIC = aic,
    BIC = bic,
    LogLikelihood = log_likelihood,
    Tau2 = tau2,
    I2 = i2,
    QM = qm,
    QMp = qmp
  )
}
```

```{r}
# Extract diagnostics for all models
model_diagnostics_full_model <- bind_rows(
  lapply(names(full_model_results), function(response) {
    extract_model_diagnostics(full_model_results[[response]], response)
  })
)

model_diagnostics_simplified_model <- bind_rows(
  lapply(names(simplified_model_results), function(response) {
    extract_model_diagnostics(simplified_model_results[[response]], response)
  })
)
```

```{r}
# Save diagnostics table
write.csv(model_diagnostics, file.path(output_dir, "model_diagnostics_summary.csv"), row.names = FALSE)
```

```{r}
# Visualize AIC, BIC, and Log-Likelihood
diagnostics_plot <- model_diagnostics %>%
  pivot_longer(cols = c(AIC, BIC, LogLikelihood), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = ResponseVariable, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Model Fit Comparison",
    x = "Response Variable",
    y = "Metric Value",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(diagnostics_plot)
```



Visualization 2: Fixed Effects Estimates Comparison

```{r}
# Plot Fixed Effects Estimates with Confidence Intervals
coef_plot <- ggplot(fixed_effects_data, aes(x = Term, y = Estimate, color = Dataset)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(title = "Comparison of Fixed Effects Estimates Across Models",
       x = "Fixed Effect Term",
       y = "Estimate",
       color = "Dataset") +
  theme_minimal()

print(coef_plot)
```




```{r}
# Pivot the data
pivoted_diagnostics <- all_meta_analysis_model_diagnostics %>%
  pivot_wider(
    names_from = ModelType,
    values_from = c(AIC, BIC, LogLikelihood),
    names_prefix = "Model_"
  )

# Debug: Check the resulting structure
pivoted_diagnostics |> str()
pivoted_diagnostics |> glimpse()

# Debugging: Check for NAs in pivoted_diagnostics
pivoted_diagnostics %>%
  summarise(across(starts_with("AIC_"), ~ sum(is.na(.)), .names = "NA_count_{col}")) %>%
  print()

# Compute relative differences
relative_diagnostics <- pivoted_diagnostics %>%
  mutate(
    # AIC relative differences
    AIC_Relative_Difference_To_Full_Model_Simplified = (AIC_Model_Simplified / AIC_Model_Full) * 100,
    AIC_Relative_Difference_To_Full_Model_Minimal = (AIC_Model_Minimal / AIC_Model_Full) * 100,
    AIC_Relative_Difference_To_Full_Model_Fixed = (AIC_Model_Fixed / AIC_Model_Full) * 100,
    
    # BIC relative differences
    BIC_Relative_Difference_To_Full_Model_Simplified = (BIC_Model_Simplified / BIC_Model_Full) * 100,
    BIC_Relative_Difference_To_Full_Model_Minimal = (BIC_Model_Minimal / BIC_Model_Full) * 100,
    BIC_Relative_Difference_To_Full_Model_Fixed = (BIC_Model_Fixed / BIC_Model_Full) * 100,
    
    # LogLikelihood relative differences
    LogLikelihood_Relative_Difference_To_Full_Model_Simplified = (LogLikelihood_Model_Simplified / LogLikelihood_Model_Full) * 100,
    LogLikelihood_Relative_Difference_To_Full_Model_Minimal = (LogLikelihood_Model_Minimal / LogLikelihood_Model_Full) * 100,
    LogLikelihood_Relative_Difference_To_Full_Model_Fixed = (LogLikelihood_Model_Fixed / LogLikelihood_Model_Full) * 100
  ) %>%
  # Select relevant columns for comparison
  select(
    ResponseVariable, starts_with("AIC_Relative"), starts_with("BIC_Relative"), starts_with("LogLikelihood_Relative")
  ) %>%
  # Pivot to long format for visualization or export
  pivot_longer(
    cols = -ResponseVariable,
    names_to = c("Metric", "Comparison"),
    names_sep = "_Relative_Difference_To_Full_Model_",
    values_to = "RelativeValue"
  )

relative_diagnostics |> str()

```



```{r}

# Preprocessing step: Align and fill missing values with `NA` to ensure comparability
# Step 1: Preprocess pivoted_diagnostics
preprocessed_diagnostics <- pivoted_diagnostics %>%
  rowwise() %>%
  mutate(
    AIC_Full = ifelse(is.na(AIC_Full), max(c(AIC_Simplified, AIC_Minimal, AIC_Fixed), na.rm = TRUE), AIC_Full),
    BIC_Full = ifelse(is.na(BIC_Full), max(c(BIC_Simplified, BIC_Minimal, BIC_Fixed), na.rm = TRUE), BIC_Full),
    LogLikelihood_Full = ifelse(is.na(LogLikelihood_Full), min(c(LogLikelihood_Simplified, LogLikelihood_Minimal, LogLikelihood_Fixed), na.rm = TRUE), LogLikelihood_Full)
  ) %>%
  ungroup()

preprocessed_diagnostics |> str()
preprocessed_diagnostics |>  glimpse()

# Perform relative calculations based on the Full model
relative_diagnostics <- preprocessed_diagnostics %>%
  mutate(
    AIC_Relative_Simplified = AIC_Simplified - AIC_Full,
    AIC_Relative_Minimal = AIC_Minimal - AIC_Full,
    AIC_Relative_Fixed = AIC_Fixed - AIC_Full,
    BIC_Relative_Simplified = BIC_Simplified - BIC_Full,
    BIC_Relative_Minimal = BIC_Minimal - BIC_Full,
    BIC_Relative_Fixed = BIC_Fixed - BIC_Full,
    LogLikelihood_Relative_Simplified = LogLikelihood_Simplified - LogLikelihood_Full,
    LogLikelihood_Relative_Minimal = LogLikelihood_Minimal - LogLikelihood_Full,
    LogLikelihood_Relative_Fixed = LogLikelihood_Fixed - LogLikelihood_Full
  ) %>%
  select(ResponseVariable, starts_with("AIC_Relative"), starts_with("BIC_Relative"), starts_with("LogLikelihood_Relative")) %>%
  pivot_longer(
    cols = -ResponseVariable,
    names_to = c("Metric", "ModelType"),
    names_sep = "_Relative_",
    values_to = "Value"
  )

# Debug: Check the resulting relative diagnostics
relative_diagnostics |> glimpse()

# Aggregate performance metrics across all response variables
aggregated_relative_diagnostics <- relative_diagnostics %>%
  group_by(ModelType, Metric) %>%
  summarise(MeanRelativeValue = mean(Value, na.rm = TRUE), .groups = "drop")

# Debug: Check the aggregated results
aggregated_relative_diagnostics |> str()

relative_diagnostics |> str()
aggregated_diagnostics |> str()
```


```{r}
# Heterogeneity Comparison Plot
I2_plot <- ggplot(model_summaries, aes(x = Dataset, y = I2, fill = Dataset)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(I2, 2)), vjust = -0.5) +
  labs(title = "Comparison of I² (Heterogeneity) Across Models",
       x = "Dataset",
       y = "I² (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(I2_plot)

```

Comparison table of key model statistics

```{r}
colnames(model_summaries)
```

```{r}
# Visualize Variance Components (Tau2) and Heterogeneity (I²)
variance_plot <- model_diagnostics %>%
  ggplot(aes(x = ResponseVariable)) +
  geom_bar(aes(y = Tau2, fill = "Tau2 (Variance Components)"), stat = "identity", position = "dodge") +
  geom_point(aes(y = I2 / 100, color = "I² (Heterogeneity)"), size = 4) +
  scale_y_continuous(
    name = "Variance Components (Tau2)",
    sec.axis = sec_axis(~.*100, name = "Heterogeneity (I² %)"),
    limits = c(0, 0.02, na.rm = TRUE)
  ) +
  labs(
    title = "Variance Components and Heterogeneity",
    x = "Response Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

variance_plot
```


















```{r}
# Combine results from all response variables into a single data frame
forest_plot_data <- bind_rows(
  lapply(names(model_results), function(response) {
    model <- model_results[[response]]
    
    if (!is.null(model) && !is.null(model$data)) {
      n_effects <- length(model$yi)
      n_studies <- nrow(model$data)
      
      # Ensure the lengths match or skip inconsistent data
      if (n_effects == n_studies) {
        data.frame(
          Study = model$data$id_article,                 # Study IDs
          EffectSize = model$yi,                        # Effect sizes
          CI_Lower = model$yi - 1.96 * sqrt(model$vi),  # Lower CI
          CI_Upper = model$yi + 1.96 * sqrt(model$vi),  # Upper CI
          ResponseVariable = response                   # Response variable
        )
      } else {
        warning(sprintf(
          "Skipping response variable '%s': Mismatched lengths between model$yi (%d) and model$data (%d).",
          response, n_effects, n_studies
        ))
        NULL
      }
    } else {
      warning(sprintf("Skipping response variable '%s': Missing model or data.", response))
      NULL
    }
  })
)

# Check the prepared data
forest_plot_data |> glimpse()
```
```{r}
# Prepare Aggregated Data
aggregated_data <- forest_plot_data %>%
  group_by(ResponseVariable) %>%
  summarise(
    overall_effect = mean(EffectSize, na.rm = TRUE),
    lower_ci = mean(CI_Lower, na.rm = TRUE),
    upper_ci = mean(CI_Upper, na.rm = TRUE),
    num_observations = n(),
    num_studies = n_distinct(Study), # Assuming 'Study' represents unique studies
    size_category = case_when(
      num_studies <= 2 ~ "1-2",
      num_studies <= 4 ~ "3-4",
      num_studies > 4 ~ "5+"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    size_category = factor(size_category, levels = c("1-2", "3-4", "5+")),
    response_rank = rank(overall_effect)
  )

aggregated_data
```



```{r}
# Create the forest plot with custom colors
forest_plot <- aggregated_data |> 
  ggplot(aes(x = overall_effect, y = reorder(ResponseVariable, response_rank))) +
  # Add points for effect sizes
  geom_point(aes(size = size_category, color = ResponseVariable)) +
  # Add horizontal error bars for confidence intervals
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, color = ResponseVariable), height = 0.2) +
  # Add vertical line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # Customize point size scale
  scale_size_manual(
    values = c("1-2" = 3, "3-4" = 5, "5+" = 7),
    name = "Number of Studies"
  ) +
  # Customize color scale
  scale_color_manual(
    values = custom_colors,
    name = "Response Variable"
  ) +
  # Customize plot labels and appearance
  labs(
    title = "Forest Plot of Response Variables with Custom Colors",
    x = "Effect Size (Overall)",
    y = "Response Variable",
    size = "Number of Studies"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Display the plot
forest_plot
```






```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################
# Start time tracking
start.time <- Sys.time()
##########################################################################
# Protocol to Fit and Save Four Suggested Models for Meta-Analysis

##########################################################################
# Model 1: Comprehensive (Full) Model
# Incorporates the most complex structure with nested random effects and all moderators.
##########################################################################

# Define the function to fit the full model for each response variable
fit_full_model <- function(data_subset, response_variable, v_matrix, moderators) {
  cat("\nFitting full model for response variable:", response_variable, "...\n")
  
  # Ensure all moderators are treated as factors
  data_subset <- data_subset %>%
    mutate(across(all_of(moderators), as.factor)) %>%
    as.data.frame()
  
  # Define the moderator formula
  moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      yi = yi,                              # Dependent variable: effect size
      V = v_matrix,                         # Variance-covariance matrix for within-study variability
      mods = moderator_formula,             # Moderator formula: relationship between the effect size and moderators
      random = list(
        ~ 1 | id_article/response_variable, # Nested random intercept for response variable nested within articles
        ~ 1 | exp_id                        # Random effect: accounts for variability at the experiment level
      ),
      data = data_subset,                   # Data used for model fitting
      method = "REML",                      # REML instead of ML to estimate variance components more robustly.
      control = list(
        optimizer = "optim",                # Optimizer function to use for fitting
        optim.method = "BFGS",              # Optimization algorithm
        iter.max = 1000,                    # Maximum number of iterations allowed
        rel.tol = 1e-8                      # Convergence tolerance for optimization
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  # Return the model or NULL if fitting failed
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

# Fit full models for each response variable
full_model_results <- list()
for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Retrieve the precomputed v_matrix
  v_matrix <- v_matrices[[response]]
  
  # Define moderators
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  # Fit the full model for the current response variable
  model <- fit_full_model(data_subset, response, v_matrix, moderators)
  
  # Save the model result
  full_model_results[[response]] <- model
}

##########################################################################
# Model 2: Moderately Simplified Model
# Simplifies random effects and removes interaction terms.
##########################################################################

# Define the function to fit simplified models for each response variable
fit_simplified_model <- function(data_subset, response_variable, v_matrix, moderators) {
  cat("\nFitting simplified model for response variable:", response_variable, "...\n")
  
  # Ensure all moderators are treated as factors
  data_subset <- data_subset %>%
    mutate(across(all_of(moderators), as.factor)) %>%
    as.data.frame()
  
  # Define the moderator formula
  moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      yi = yi,                           # Dependent variable: effect size
      V = v_matrix,                      # Variance-covariance matrix for within-study variability
      mods = moderator_formula,          # Moderator formula: relationship between the effect size and moderators
      random = ~ 1 | exp_id,             # Simplified random effect: variability at the experiment level only
      data = data_subset,                # Data used for model fitting
      method = "REML",                   # REML instead of ML to estimate variance components more robustly.
      control = list(
        optimizer = "optim",             # Optimizer function to use for fitting
        optim.method = "BFGS",           # Optimization algorithm
        iter.max = 1000,                 # Maximum number of iterations allowed
        rel.tol = 1e-8                   # Convergence tolerance for optimization
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  # Return the model or NULL if fitting failed
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

# Fit simplified models for each response variable
simplified_model_results <- list()
for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Retrieve the precomputed v_matrix
  v_matrix <- v_matrices[[response]]
  
  # Define moderators
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  # Fit the simplified model for the current response variable
  model <- fit_simplified_model(data_subset, response, v_matrix, moderators)
  
  # Save the model result
  simplified_model_results[[response]] <- model
}

##########################################################################
# Model 3: Minimal Random Effects Model
# Focuses on intercept-only model with minimal random effects.
##########################################################################

# Define the function to fit minimally reduced models for each response variable
fit_minimal_model <- function(data_subset, response_variable, v_matrix) {
  cat("\nFitting minimal model for response variable:", response_variable, "...\n")
  
  # Define the intercept-only formula
  minimal_formula <- as.formula("yi ~ 1")
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      yi = yi,                           # Dependent variable: effect size
      V = v_matrix,                      # Variance-covariance matrix for within-study variability
      mods = minimal_formula,            # Intercept-only model
      random = ~ 1 | exp_id,             # Random effect: accounts for variability at the experiment level
      data = data_subset,                # Data used for model fitting
      method = "REML",                   # REML instead of ML to estimate variance components more robustly.
      control = list(
        optimizer = "optim",             # Optimizer function to use for fitting
        optim.method = "BFGS",           # Optimization algorithm
        iter.max = 1000,                 # Maximum number of iterations allowed
        rel.tol = 1e-8                   # Convergence tolerance for optimization
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  # Return the model or NULL if fitting failed
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

# Fit minimal models for each response variable
minimal_model_results <- list()
for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Retrieve the precomputed v_matrix
  v_matrix <- v_matrices[[response]]
  
  # Fit the minimal model for the current response variable
  model <- fit_minimal_model(data_subset, response, v_matrix)
  
  # Save the model result
  minimal_model_results[[response]] <- model
}

##########################################################################
# Model 4: Fixed Effects Only Model
# Removes random effects entirely and focuses solely on fixed effects.
##########################################################################

# Define the function to fit fixed effects only models for each response variable
fit_fixed_effects_model <- function(data_subset, response_variable, v_matrix, moderators) {
  cat("\nFitting fixed effects only model for response variable:", response_variable, "...\n")
  
  # Ensure all moderators are treated as factors
  data_subset <- data_subset %>%
    mutate(across(all_of(moderators), as.factor)) %>%
    as.data.frame()
  
  # Define the moderator formula
  moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  
  # Fit the model
  model <- tryCatch({
    rma.mv(
      yi = yi,                           # Dependent variable: effect size
      V = v_matrix,                      # Variance-covariance matrix for within-study variability
      mods = moderator_formula,          # Moderator formula: relationship between the effect size and moderators
      data = data_subset,                # Data used for model fitting
      method = "REML",                   # REML instead of ML to estimate variance components more robustly.
      control = list(
        optimizer = "optim",             # Optimizer function to use for fitting
        optim.method = "BFGS",           # Optimization algorithm
        iter.max = 1000,                 # Maximum number of iterations allowed
        rel.tol = 1e-8                   # Convergence tolerance for optimization
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  # Return the model or NULL if fitting failed
  if (!is.null(model)) {
    cat("Model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

# Fit fixed effects only models for each response variable
fixed_effects_model_results <- list()
for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Subset the data for the current response variable
  data_subset <- meta_data[meta_data$response_variable == response, ]
  
  # Retrieve the precomputed v_matrix
  v_matrix <- v_matrices[[response]]
  
  # Define moderators
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  # Fit the fixed effects only model for the current response variable
  model <- fit_fixed_effects_model(data_subset, response, v_matrix, moderators)
  
  # Save the model result
  fixed_effects_model_results[[response]] <- model
}

##########################################################################
# Save All Fitted Models
##########################################################################

output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

saveRDS(full_model_results, file = file.path(output_dir, "fitted_models_full.rds"))
saveRDS(simplified_model_results, file = file.path(output_dir, "fitted_models_simplified.rds"))
saveRDS(minimal_model_results, file = file.path(output_dir, "fitted_models_minimal.rds"))
saveRDS(fixed_effects_model_results, file = file.path(output_dir, "fitted_models_fixed_effects.rds"))

cat("\nAll models have been saved successfully!\n")

##########################################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n" )
##########################################################################
# Last go (02/12-2024)
# Total time taken: 18.05626 secs

# Processing response variable: Biodiversity 
# Fitting full model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting full model for response variable: Greenhouse gas emission ...
# Error in model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting full model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting full model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting full model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting full model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting full model for response variable: Water quality ...
# Error in model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Biodiversity 
# 
# Fitting simplified model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting simplified model for response variable: Greenhouse gas emission ...
# Error in model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting simplified model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting simplified model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting simplified model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting simplified model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting simplified model for response variable: Water quality ...
# Error in model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# 
# Processing response variable: Biodiversity 
# Fitting minimal model for response variable: Biodiversity ...
# Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting minimal model for response variable: Greenhouse gas emission ...
# Model fitting completed for response variable: Greenhouse gas emission .
# Processing response variable: Product quality 
# Fitting minimal model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting minimal model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting minimal model for response variable: Pest and Disease ...
# Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting minimal model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting minimal model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Water quality .
# 
# Processing response variable: Biodiversity 
# Fitting fixed effects only model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting fixed effects only model for response variable: Greenhouse gas emission ...
# Error in model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting fixed effects only model for response variable: Product quality ...
# Model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting fixed effects only model for response variable: Crop yield ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting fixed effects only model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting fixed effects only model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting fixed effects only model for response variable: Water quality ...
# Error in model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# 
# All models have been saved successfully!
# 
# Total time taken: 18.05626 
```


The results show varying success in fitting models across different response variables and levels of model complexity. For *biodiversity*, all models were fitted successfully, though there was a warning about some rows with missing data being omitted. This indicates that the data for this variable may be incomplete but still sufficient to run and compare models effectively. The fitted models for *biodiversity* can move forward to the evaluation phase without major concerns.

For *greenhouse gas emissions*, only the minimal model was fitted successfully, with other models failing due to an error related to the levels of categorical variables. This suggests that some moderators do not have sufficient variability within this response variables dataset. Addressing this issue by preprocessing the data, such as removing unused factor levels, could enable the fitting of more complex models.

The *product quality* response variable performed well across all model types, with no critical issues reported during fitting. This indicates that the data for this variable is robust and suitable for detailed comparisons across models.

For *crop yield*, all models were fitted, but there were warnings about large variance ratios, which suggest potential instability in the results. This likely reflects heterogeneity in the dataset, and further diagnostics could clarify whether certain studies or data points are driving this variability.

The *pest and disease* response variable also saw successful fitting across models, though there were instances of redundant predictors being dropped. This points to the value of simplified models in handling such issues efficiently. For *soil quality*, similar warnings about variance ratios and redundant predictors were observed, meaning these results also require further evaluation to ensure reliability.

*Water quality*, like *greenhouse gas emissions*, presented challenges in fitting models beyond the minimal level. This again indicates the need to refine the dataset by addressing issues related to categorical variables with insufficient levels. Overall, these results highlight both strengths and areas needing refinement in the data, allowing for targeted improvements before moving forward with model comparisons and interpretations.












```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################
# Start time tracking
start.time <- Sys.time()

#######################################################################################
# Step 1: Check and enforce correct data types
col_for_impute <- database_clean_sd |> 
  as.data.frame() |> 
  select(-geometry) |> 
  select(
    # Columns that need to be imputed
    silvo_se, control_se, 
    # Columns that are used by mice to impute values
    tree_age, crop_type, tree_type, bioclim_sub_regions, experiment_year, alley_width, silvo_n, control_n,
    # IDs that are used to back-link imputed values to the dataset
    id_article, id_obs, treat_id, exp_id
  ) |> 
  mutate(
    silvo_se = as.numeric(silvo_se),
    control_se = as.numeric(control_se),
    silvo_n = as.numeric(silvo_n),
    control_n = as.numeric(control_n),
    tree_age = as.numeric(tree_age),
    crop_type = as.factor(crop_type),
    tree_type = as.factor(tree_type),
    bioclim_sub_regions = as.factor(bioclim_sub_regions),
    alley_width = as.factor(alley_width),
    id_article = as.numeric(id_article),
    id_obs = as.numeric(id_obs),
    treat_id = as.numeric(treat_id),
    exp_id = as.numeric(exp_id)
  )

#######################################################################################
# Step 2: Define the function for each imputation method
impute_data <- function(data, method_name) {
  if (method_name == "pmm") {
    # Predictive Mean Matching
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0

    ##########################################################################
    # Define imputation method for PMM
    method <- c(
      "silvo_se" = "pmm",         # Imputed using predictive mean matching
      "control_se" = "pmm",       # Imputed using predictive mean matching
      #"silvo_n" = "",            # Not imputed
      #"control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = ""          # Not imputed
      #"id_obs" = "",             # Not imputed
      #"treat_id" = "",           # Not imputed
      #"exp_id" = ""              # Not imputed
    )
    
    # Perform imputation using mice
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else if (method_name == "upper_quartile") {
    ##########################################################################
    # Upper Quartile Imputation for Variance
    upper_quartile_variance <- data %>%
      # The 75th percentile represents a value higher than the median, ensuring that the imputed variances 
      # are not unrealistically small while 
      # still grounded in observed data. This helps maintain a conservative weighting in the meta-analysis.
      summarise(across(c(silvo_se, control_se), ~ quantile(.^2, 0.75, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "upper_quartile")

    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), sqrt(upper_quartile_variance$upper_quartile[1]), silvo_se),
        control_se = ifelse(is.na(control_se), sqrt(upper_quartile_variance$upper_quartile[2]), control_se)
      )
    return(data)
    
  } else if (method_name == "mean_imputation") {
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), mean(silvo_se, na.rm = TRUE), silvo_se),
        control_se = ifelse(is.na(control_se), mean(control_se, na.rm = TRUE), control_se),
        silvo_n = ifelse(is.na(silvo_n), mean(silvo_n, na.rm = TRUE), silvo_n),
        control_n = ifelse(is.na(control_n), mean(control_n, na.rm = TRUE), control_n)
      )
    return(data)

  } else if (method_name == "linear_imputation") {
    ##########################################################################
    # Linear Regression Imputation (norm.predict)
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0

    method <- c(
      "silvo_se" = "norm.predict",   # Imputed using linear regression
      "control_se" = "norm.predict", # Imputed using linear regression
       #"silvo_n" = "",            # Not imputed
      #"control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = ""          # Not imputed
      #"id_obs" = "",             # Not imputed
      #"treat_id" = "",           # Not imputed
      #"exp_id" = ""              # Not imputed
    )

    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)

  } else {
    stop("Invalid method name.")
  }
}

#######################################################################################
# Step 3: Apply each imputation method
imputation_methods <- c("pmm", "upper_quartile", "mean_imputation", "linear_imputation")
imputed_datasets <- list()

# Separate storage for the raw mids objects
imputed_mids_pmm <- NULL
imputed_mids_linear <- NULL

for (method_name in imputation_methods) {
  cat("Applying", method_name, "imputation...\n")
  if (method_name == "pmm") {
    imputed_mids_pmm <- impute_data(col_for_impute, method_name)
    imputed_datasets[[method_name]] <- mice::complete(imputed_mids_pmm)
  } else if (method_name == "linear_imputation") {
    imputed_mids_linear <- impute_data(col_for_impute, method_name)
    imputed_datasets[[method_name]] <- mice::complete(imputed_mids_linear)
  } else {
    imputed_datasets[[method_name]] <- impute_data(col_for_impute, method_name)
  }
}

##########################################################################
# Step 4: Compare results
for (method_name in imputation_methods) {
  cat("\nSummary of Imputed Dataset -", method_name, ":\n")
  print(summary(imputed_datasets[[method_name]]))
}

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")

##########################################################################
# imputed_mids_pmm and imputed_mids_linear are the raw mids objects for PMM and linear regression respectively
# imputed_datasets contains completed datasets for all methods

##########################################################################
# Last run (02/01-25)
# Total time taken: 21.27449 
```




```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################
# Start time tracking
start.time <- Sys.time()

#######################################################################################
# Step 1: Check and enforce correct data types
col_for_impute <- database_clean_sd |> 
  as.data.frame() |> 
  select(-geometry) |> 
  select(
    # Columns that need to be imputed
    silvo_se, control_se, 
    # Columns that are used by mice to impute values
    tree_age, crop_type, tree_type, bioclim_sub_regions, experiment_year, alley_width, silvo_n, control_n,
    # IDs that are used to back-link imputed values to the dataset
    id_article, id_obs, treat_id, exp_id
  ) |> 
  mutate(
    silvo_se = as.numeric(silvo_se),
    control_se = as.numeric(control_se),
    silvo_n = as.numeric(silvo_n),
    control_n = as.numeric(control_n),
    tree_age = as.numeric(tree_age),
    crop_type = as.factor(crop_type),
    tree_type = as.factor(tree_type),
    bioclim_sub_regions = as.factor(bioclim_sub_regions),
    alley_width = as.factor(alley_width),
    id_article = as.numeric(id_article),
    id_obs = as.numeric(id_obs),
    treat_id = as.numeric(treat_id),
    exp_id = as.numeric(exp_id)
  ) |> 
  # Removing column variables not used in the imputation
  select(-c(silvo_n, control_n,
            id_obs, treat_id, exp_id))

#######################################################################################
# Step 2: Define the function for each imputation method
impute_data <- function(data, method_name) {
  if (method_name == "pmm") {
    # Predictive Mean Matching
    pred_matrix <- mice::make.predictorMatrix(data)
    # Exclude these columns from imputation: "id_obs", "treat_id", "exp_id"
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", "id_article")] <- 0 

    ##########################################################################
    # Define imputation method for PMM
    method <- c(
      "silvo_se" = "pmm",         # Imputed using predictive mean matching
      "control_se" = "pmm",       # Imputed using predictive mean matching
      #"silvo_n" = "",            # Not imputed
      #"control_n" = "",          # Not imputed
      "tree_age" = "",            # Not imputed
      "crop_type" = "",           # Not imputed
      "tree_type" = "",           # Not imputed
      "bioclim_sub_regions" = "", # Not imputed
      "experiment_year" = "",     # Not imputed
      "alley_width" = "",         # Not imputed
      "id_article" = ""           # Not imputed
      #"id_obs" = "",             # Not imputed
      #"treat_id" = "",           # Not imputed
      #"exp_id" = ""              # Not imputed
    )

    # Perform imputation using mice
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)

  } else if (method_name == "upper_quartile") {
    ##########################################################################
    # Upper Quartile Imputation for Variance
    upper_quartile_variance <- data %>%
      summarise(across(c(silvo_se, control_se), ~ quantile(.^2, 0.75, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "upper_quartile")

    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), sqrt(upper_quartile_variance$upper_quartile[1]), silvo_se),
        control_se = ifelse(is.na(control_se), sqrt(upper_quartile_variance$upper_quartile[2]), control_se)
      )
    return(data)

  } else if (method_name == "mean_imputation") {
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), mean(silvo_se, na.rm = TRUE), silvo_se),
        control_se = ifelse(is.na(control_se), mean(control_se, na.rm = TRUE), control_se),
        silvo_n = ifelse(is.na(silvo_n), mean(silvo_n, na.rm = TRUE), silvo_n),
        control_n = ifelse(is.na(control_n), mean(control_n, na.rm = TRUE), control_n)
      )
    return(data)

  } else if (method_name == "linear_imputation") {
    ##########################################################################
    # Linear Regression Imputation (norm.predict)
    pred_matrix <- mice::make.predictorMatrix(data)
     # Exclude these columns from imputation: "id_obs", "treat_id", "exp_id"
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", "id_article")] <- 0 

    method <- c(
      "silvo_se" = "norm.predict",   # Imputed using linear regression
      "control_se" = "norm.predict", # Imputed using linear regression
       #"silvo_n" = "",            # Not imputed
      #"control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = ""          # Not imputed
      #"id_obs" = "",             # Not imputed
      #"treat_id" = "",           # Not imputed
      #"exp_id" = ""              # Not imputed
    )

    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)

  } else {
    stop("Invalid method name.")
  }
}

#######################################################################################
# Step 3: Apply each imputation method
imputation_methods <- c("pmm", "upper_quartile", "mean_imputation", "linear_imputation")
imputed_datasets <- list()

# Separate storage for the raw mids objects
imputed_mids_pmm <- NULL
imputed_mids_linear <- NULL

for (method_name in imputation_methods) {
  cat("Applying", method_name, "imputation...\n")
  if (method_name == "pmm") {
    imputed_mids_pmm <- impute_data(col_for_impute, method_name)
    imputed_datasets[[method_name]] <- mice::complete(imputed_mids_pmm)
  } else if (method_name == "linear_imputation") {
    imputed_mids_linear <- impute_data(col_for_impute, method_name)
    imputed_datasets[[method_name]] <- mice::complete(imputed_mids_linear)
  } else {
    imputed_datasets[[method_name]] <- impute_data(col_for_impute, method_name)
  }
}

##########################################################################
# Step 4: Cap Imputed Values
cap_values <- function(data, lower_quantile = 0.05, upper_quantile = 0.95) {
  quantiles <- data |> summarise(
    silvo_se_lower = quantile(silvo_se, lower_quantile, na.rm = TRUE),
    silvo_se_upper = quantile(silvo_se, upper_quantile, na.rm = TRUE),
    control_se_lower = quantile(control_se, lower_quantile, na.rm = TRUE),
    control_se_upper = quantile(control_se, upper_quantile, na.rm = TRUE)
  )

  data <- data |> mutate(
    silvo_se = pmin(pmax(silvo_se, quantiles$silvo_se_lower), quantiles$silvo_se_upper),
    control_se = pmin(pmax(control_se, quantiles$control_se_lower), quantiles$control_se_upper)
  )

  return(data)
}

# Apply capping to each imputed dataset
for (method_name in names(imputed_datasets)) {
  cat("Capping values for", method_name, "dataset...\n")
  imputed_datasets[[method_name]] <- cap_values(imputed_datasets[[method_name]])
}

##########################################################################
# Step 5: Compare results
for (method_name in imputation_methods) {
  cat("\nSummary of Imputed and Capped Dataset -", method_name, ":\n")
  print(summary(imputed_datasets[[method_name]]))
}

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")

##########################################################################
# imputed_mids_pmm and imputed_mids_linear are the raw mids objects for PMM and linear regression respectively
# imputed_datasets contains completed and capped datasets


##########################################################################
# Last run (02/01-25)
```

```{r}
# Recreate the violin plot with better scaling
ggplot() +
  # Violin plot for bootstrapped data
  geom_violin(data = bootstrapped_data, aes(y = response_variable, x = exp(bootstrapped_rr), fill = response_variable), 
              alpha = 0.5, scale = "area") + # Use 'area' scaling for better proportional representation
  # Overlay mean and confidence intervals
  geom_point(data = summary_data, aes(y = response_variable, x = exp(WeightedMeanRR)), color = "black", size = 3) +
  geom_errorbarh(data = summary_data, aes(y = response_variable, xmin = exp(LowerCI), xmax = exp(UpperCI)), 
                height = 0.2, color = "black") +
  # Add annotations for proportions and study counts
  geom_text(data = summary_data, aes(
    y = response_variable, x = 1.1, 
    label = paste0("RR<1: ", round(RR_Less_1), "%\nRR>1: ", round(RR_Greater_1), "%\n[N=", Studies, ", NO=", Observations, "]")
  ), size = 3, hjust = 0.5) +
  # Apply custom colors
  scale_fill_manual(values = custom_colors) +
  # Customize plot
  scale_x_continuous(trans = "log", breaks = scales::log_breaks(base = 10)) +
  labs(
    title = "Weighted Mean Response Ratio",
    subtitle = "Agroforestry vs. Non-Agroforestry Effects by Response Variable",
    x = "Response Ratio (Log Scale)",
    y = "Response Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )

```


##########################################################################
# Step 5: Cap Imputed Values
cap_values <- function(data, lower_quantile = 0.05, upper_quantile = 0.95) {
  quantiles <- data |> summarise(
    silvo_se_lower = quantile(silvo_se, lower_quantile, na.rm = TRUE),
    silvo_se_upper = quantile(silvo_se, upper_quantile, na.rm = TRUE),
    control_se_lower = quantile(control_se, lower_quantile, na.rm = TRUE),
    control_se_upper = quantile(control_se, upper_quantile, na.rm = TRUE)
  )
  
  data <- data |> mutate(
    silvo_se = pmin(pmax(silvo_se, quantiles$silvo_se_lower), quantiles$silvo_se_upper),
    control_se = pmin(pmax(control_se, quantiles$control_se_lower), quantiles$control_se_upper)
  )
  
  return(data)
}

# Apply capping to each imputed dataset
for (method_name in names(imputed_datasets)) {
  cat("Capping values for", method_name, "dataset...\n")
  imputed_datasets[[method_name]] <- cap_values(imputed_datasets[[method_name]])
}



```{r}
# Function to create pre- and post-capping datasets for visualization
compare_capping <- function(original_data, capped_data, variable_name) {
  bind_rows(
    original_data |> select(!!sym(variable_name)) |> mutate(stage = "Original"),
    capped_data |> select(!!sym(variable_name)) |> mutate(stage = "Capped")
  )
}

# Visualize pre- and post-capping for a single variable
visualize_capping <- function(data, variable_name) {
  ggplot(data, aes(x = stage, y = !!sym(variable_name), fill = stage)) +
    geom_boxplot(outlier.color = "red", alpha = 0.6) +
    labs(
      title = paste("Capping Effect on", variable_name),
      x = "Stage",
      y = variable_name
    ) +
    theme_minimal()
}

# Example for silvo_se and control_se
for (method_name in names(imputed_datasets)) {
  original_data <- col_for_impute
  capped_data <- imputed_datasets[[method_name]]
  
  # Create comparison datasets
  silvo_se_data <- compare_capping(original_data, capped_data, "silvo_se")
  control_se_data <- compare_capping(original_data, capped_data, "control_se")
  
  # Plot silvo_se
  cat("Visualizing capping for", method_name, "- silvo_se\n")
  print(visualize_capping(silvo_se_data, "silvo_se"))
  
  # Plot control_se
  cat("Visualizing capping for", method_name, "- control_se\n")
  print(visualize_capping(control_se_data, "control_se"))
}

```
```{r}
# Function to identify capped observations


```


```{r}
# Create a formatted table using `gt`
comparison_gt <- prepared_data_gt %>%
  # Reorder the columns in the data frame before passing to gt
  select(
    metric, category, original, 
    linear_imputation,
    linear_imputation_relative,
    mean_imputation, 
    mean_imputation_relative,
    upper_quartile, 
    upper_quartile_relative, 
    bayesian,
    bayesian_relative,
    pmm, 
    pmm_relative,  
    pmm_best,  
    pmm_best_relative,
    rf,
    rf_relative
  ) |> 
  # Set `metric` as row names
  gt(rowname_col = "metric") %>%  
  tab_header(
    title = "Comparison of Imputation Methods Across Metrics",
    subtitle = "Including Original Data and Relative Differences"
  ) %>%
  # Rename columns for clarity
  cols_label(
    original = "Original",
    linear_imputation = "Linear Imputation",
    linear_imputation_relative = "Linear Imputation Relative",
    mean_imputation = "Mean Imputation",
    mean_imputation_relative = "Mean Imputation Relative",
    upper_quartile = "Upper Quartile",
    upper_quartile_relative = "Upper Quartile Relative",
    bayesian = "Bayesian",
    bayesian_relative = "Bayesian Relative",
    pmm = "PMM",
    pmm_relative = "PMM Relative",
    pmm_best = "PMM Best",
    pmm_best_relative = "PMM Best Relative",
    rf = "Random Forest",
    rf_relative = "Random Forest Relative"
  ) %>%
  # Format numeric columns to two decimal places
  fmt_number(
    columns = c(pmm, upper_quartile, mean_imputation, pmm_best, original),
    decimals = 2
  ) %>%
  fmt_number(
    columns = ends_with("_relative"),  # Format relative difference columns
    decimals = 3
  ) %>%
  # Replace 0 values in the relative columns with "NA"
  fmt_missing(
    columns = ends_with("_relative"),
    missing_text = "NA"
  ) %>%
  # Add horizontal and vertical lines for clarity
  tab_style(
    style = list(
      cell_fill(color = "#f9f9f9"),
      cell_borders(sides = "all", color = "gray", weight = px(1))
    ),
    locations = cells_body()
  ) %>%
  # Adjust table options
  tab_options(
    table.font.size = "small",
    table.border.top.color = "gray",
    table.border.bottom.color = "gray",
    column_labels.font.size = "medium",
    row_group.font.size = "small"
  ) %>%
  # Add footnotes for explanation of relative differences
  tab_footnote(
    footnote = "Relative differences are calculated as |(imputed - original) / original|.",
    locations = cells_column_labels(ends_with("_relative"))
  )

# Optionally, export the table
output_folder <- here("DATA", "OUTPUT_FROM_R")
gtsave(comparison_gt, file.path(output_folder, "comparison_table.html"))
gtsave(comparison_gt, file.path(output_folder, "comparison_table.pdf"))

# Display the table
comparison_gt

```



```{r}
custom_colors <- c(
  "Biodiversity" = "#FF9999",
  "Greenhouse gas emission" = "#66C266",
  "Product quality" = "#FFC000",
  "Crop yield" = "#FF9933",
  "Pest and Disease" = "#33CCCC",
  "Soil quality" = "#9966CC",
  "Water quality" = "#9999FF"
)

# Step 1: Extract individual datasets for each response variable
simplified_model_data_crop_yield <- simplified_model_results$`Crop yield`$data
simplified_model_data_biodiversity <- simplified_model_results$Biodiversity$data
simplified_model_data_product_quality <- simplified_model_results$`Product quality`$data
simplified_model_data_soil_quality <- simplified_model_results$`Soil quality`$data
simplified_model_data_water_quality <- simplified_model_results$`Water quality`$data

# Combine all response variable datasets into one for the simplified model
simplified_model_data <- do.call(
  rbind,
  list(
    simplified_model_data_crop_yield,
    simplified_model_data_biodiversity,
    simplified_model_data_product_quality,
    simplified_model_data_soil_quality,
    simplified_model_data_water_quality
  )
)

# Ensure the merged dataset is structured properly
simplified_model_data <- as.data.frame(simplified_model_data)

# Step 2: Repeat for the full model
full_model_data_crop_yield <- full_model_results$`Crop yield`$data
full_model_data_biodiversity <- full_model_results$Biodiversity$data
full_model_data_product_quality <- full_model_results$`Product quality`$data
full_model_data_soil_quality <- full_model_results$`Soil quality`$data
full_model_data_water_quality <- full_model_results$`Water quality`$data

# Combine all response variable datasets into one for the full model
full_model_data <- do.call(
  rbind,
  list(
    full_model_data_crop_yield,
    full_model_data_biodiversity,
    full_model_data_product_quality,
    full_model_data_soil_quality,
    full_model_data_water_quality
  )
)

# Ensure the merged dataset is structured properly
full_model_data <- as.data.frame(full_model_data)
```
```{r}

# Check the structure of the final datasets
str(simplified_model_data)
str(full_model_data)



# Refit the "simplified" model using ML
simplified_ml <- rma.mv(
  yi = yi,
  V = simplified_model_data$vi,  # Replace with the correct variance structure
  mods = ~ tree_type + crop_type + age_system + season + soil_texture,  # Simplified model moderators
  random = ~ 1 | exp_id,          # Random effects structure
  data = simplified_model_data,   # Dataset for simplified model
  method = "ML"                   # Maximum Likelihood method
)


# Refit the "full" model using ML
full_ml <- rma.mv(
  yi = yi,
  V = full_model_data$vi,         # Replace with the correct variance structure
  mods = ~ tree_type * crop_type * age_system * season * soil_texture,  # Full model with interactions
  random = ~ 1 | exp_id,          # Random effects structure
  data = full_model_data,         # Dataset for full model
  method = "ML"                   # Maximum Likelihood method
)

# Perform a likelihood ratio test to compare the models
lrt <- anova(simplified_ml, full_ml)
print(lrt)

# Interpret the results
if (lrt$pval < 0.05) {
  cat("The full model significantly improves the fit compared to the simplified model (p =", lrt$pval, ").\n")
} else {
  cat("The simplified model is sufficient; the full model does not significantly improve the fit (p =", lrt$pval, ").\n")
}

```










```{r}
##########################################################################
# Extract AIC Values for Each Response Variable (Handling NULLs)
##########################################################################

# Initialize a data frame to store AIC results
aic_results <- data.frame(
  Response = character(),
  Null = numeric(),
  Minimal = numeric(),
  Fixed = numeric(),
  Simplified = numeric(),
  Full = numeric(),
  BestModel = character(),
  stringsAsFactors = FALSE
)

# Loop through each response variable
for (response in names(model_results)) {
  cat("\nExtracting AIC for response variable:", response, "\n")
  
  # Retrieve models for the current response variable
  models <- model_results[[response]]
  null <- models$null
  minimal <- models$minimal
  fixed <- models$fixed
  simplified <- models$simplified
  full <- models$full
  
  # Extract AIC values, replacing NULL with NA
  aic_values <- c(
    Null = if (!is.null(null)) null$aic else NA,
    Minimal = if (!is.null(minimal)) minimal$aic else NA,
    Fixed = if (!is.null(fixed)) fixed$aic else NA,
    Simplified = if (!is.null(simplified)) simplified$aic else NA,
    Full = if (!is.null(full)) full$aic else NA
  )
  
  # Check if all AIC values are NA; skip this response variable if true
  if (all(is.na(aic_values))) {
    cat("All models are missing for response variable:", response, "\n")
    next
  }
  
  # Determine the best model based on the lowest AIC
  best_model <- if (!all(is.na(aic_values))) {
    names(aic_values)[which.min(aic_values)]
  } else {
    NA
  }
  
  # Append results to the data frame
  aic_results <- rbind(
    aic_results,
    data.frame(
      Response = response,
      Null = aic_values["Null"],
      Minimal = aic_values["Minimal"],
      Fixed = aic_values["Fixed"],
      Simplified = aic_values["Simplified"],
      Full = aic_values["Full"],
      BestModel = best_model,
      stringsAsFactors = FALSE
    )
  )
}

##########################################################################
# Save AIC Results to File
##########################################################################
write.csv(aic_results, "aic_results.csv", row.names = FALSE)
cat("AIC results saved to 'aic_results.csv'\n")

##########################################################################
# Print AIC Results
##########################################################################
print(aic_results)


```





```{r}
# meta_analysis_model_comparisons |> str()
# meta_analysis_model_comparisons$Biodiversity$LR_Full_vs_Fixed |> str()

aic_results
```










```{r}
# Check if LRT statistics are present in each response variable's model object
lrt_presence <- data.frame(
  Response = character(),
  LRT_Statistic_Present = logical(),
  LRT_pval_Present = logical(),
  stringsAsFactors = FALSE
)

for (response in names(ml_comparison_results)) {
  result <- ml_comparison_results[[response]]
  
  if (!is.null(result) && !is.null(result$lrt)) {
    lrt_stat_present <- !is.null(result$lrt$statistic)
    lrt_pval_present <- !is.null(result$lrt$pval)
  } else {
    lrt_stat_present <- FALSE
    lrt_pval_present <- FALSE
  }
  
  lrt_presence <- rbind(
    lrt_presence,
    data.frame(
      Response = response,
      LRT_Statistic_Present = lrt_stat_present,
      LRT_pval_Present = lrt_pval_present,
      stringsAsFactors = FALSE
    )
  )
}

# Print results
print(lrt_presence)























##########################################################################################################################################
FOR INDIVIDUAL MODELS PER RESPONSE VARIABLE
##########################################################################################################################################
null = fit_null_model(data_subset, response),
minimal = fit_minimal_model(data_subset, response, v_matrix),
fixed = fit_fixed_effects_model(data_subset, response, v_matrix, moderators),
simplified = fit_simplified_model(data_subset, response, v_matrix, moderators),
full = fit_full_model(data_subset, response, v_matrix, moderators)



```{r}
##########################################################################################################################################
# Compare Models for Individual Response Variables (Extract AIC Values)
##########################################################################################################################################

# Initialize a data frame to store AIC results
aic_results <- data.frame(
  Response = character(),
  Null = numeric(),
  Minimal = numeric(),
  Fixed = numeric(),
  Simplified = numeric(),
  Full = numeric(),
  BestModel = character(),
  stringsAsFactors = FALSE
)

# Loop through each response variable to extract and compare AIC values
for (response in names(model_results)) {
  cat("\nProcessing response variable:", response, "\n")
  
  # Retrieve models for the current response variable
  models <- model_results[[response]]
  null_model <- models$null
  minimal_model <- models$minimal
  fixed_model <- models$fixed
  simplified_model <- models$simplified
  full_model <- models$full
  
  # Extract AIC values, replacing NULL with NA for missing models
  aic_values <- c(
    Null = if (!is.null(null_model)) null_model$aic else NA,
    Minimal = if (!is.null(minimal_model)) minimal_model$aic else NA,
    Fixed = if (!is.null(fixed_model)) fixed_model$aic else NA,
    Simplified = if (!is.null(simplified_model)) simplified_model$aic else NA,
    Full = if (!is.null(full_model)) full_model$aic else NA
  )
  
  # Skip this response variable if all AIC values are missing
  if (all(is.na(aic_values))) {
    cat("All models are missing for response variable:", response, "\n")
    next
  }
  
  # Determine the best model based on the lowest AIC
  best_model <- names(aic_values)[which.min(aic_values, na.rm = TRUE)]
  
  # Append the extracted AIC values and the best model to the results data frame
  aic_results <- rbind(
    aic_results,
    data.frame(
      Response = response,
      Null = aic_values["Null"],
      Minimal = aic_values["Minimal"],
      Fixed = aic_values["Fixed"],
      Simplified = aic_values["Simplified"],
      Full = aic_values["Full"],
      BestModel = best_model,
      stringsAsFactors = FALSE
    )
  )
}
```


```{r}
##########################################################################
# Systematic Comparison of Models for Each Response Variable
##########################################################################

# Extract model comparison data
comparison_results <- lapply(names(meta_analysis_model_comparisons), function(response) {
  models <- meta_analysis_model_comparisons[[response]]
  
  if (!is.null(models)) {
    list(
      Response = response,
      AIC_Full = if (!is.null(models$AIC)) models$AIC["Full"] else NA,
      AIC_Simplified = if (!is.null(models$AIC)) models$AIC["Simplified"] else NA,
      AIC_Minimal = if (!is.null(models$AIC)) models$AIC["Minimal"] else NA,
      AIC_Fixed = if (!is.null(models$AIC)) models$AIC["Fixed Effects Only"] else NA,
      LRT_Full_vs_Simplified = if (!is.null(models$LR_Full_vs_Simplified)) models$LR_Full_vs_Simplified$pval else NA,
      LRT_Full_vs_Minimal = if (!is.null(models$LR_Full_vs_Minimal)) models$LR_Full_vs_Minimal$pval else NA,
      LRT_Full_vs_Fixed = if (!is.null(models$LR_Full_vs_Fixed)) models$LR_Full_vs_Fixed$pval else NA,
      QE_Full = if (!is.null(models$Full_vs_Fixed_Details)) models$Full_vs_Fixed_Details$ResidualQEFull else NA,
      QE_Fixed = if (!is.null(models$Full_vs_Fixed_Details)) models$Full_vs_Fixed_Details$ResidualQEReduced else NA
    )
  } else {
    NULL
  }
})

# Filter out NULL elements and convert to a data frame
comparison_df <- do.call(rbind, lapply(comparison_results, function(x) {
  if (!is.null(x)) {
    data.frame(x, stringsAsFactors = FALSE)
  }
}))

comparison_df |> str()
```

##########################################################################
# Visualization of Model Comparisons
##########################################################################


```{r}
# Plot AIC Comparison Across Models
aic_plot <- ggplot(comparison_df, aes(x = Response)) +
  geom_bar(aes(y = AIC_Full, fill = "Full Model"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = AIC_Simplified, fill = "Simplified Model"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = AIC_Minimal, fill = "Minimal Model"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = AIC_Fixed, fill = "Fixed Effects Only"), stat = "identity", position = "dodge") +
  labs(title = "AIC Comparison Across Models", x = "Response Variable", y = "AIC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(aic_plot)
```
```{r}
# Plot Residual Variance (QE) Comparison
qe_plot <- ggplot(comparison_df, aes(x = Response)) +
  geom_point(aes(y = QE_Full, color = "Full Model"), size = 3) +
  geom_point(aes(y = QE_Fixed, color = "Fixed Effects Only"), size = 3) +
  labs(title = "Residual Variance (QE) Comparison", x = "Response Variable", y = "Residual Variance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(qe_plot)
```
```{r}
# Plot Likelihood Ratio Test (LRT) P-Values
lrt_plot <- ggplot(comparison_df, aes(x = Response)) +
  geom_point(aes(y = -log10(LRT_Full_vs_Simplified), color = "Full vs Simplified"), size = 3) +
  geom_point(aes(y = -log10(LRT_Full_vs_Minimal), color = "Full vs Minimal"), size = 3) +
  geom_point(aes(y = -log10(LRT_Full_vs_Fixed), color = "Full vs Fixed"), size = 3) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  labs(title = "Likelihood Ratio Test P-Values (Log10 Scale)", x = "Response Variable", y = "-log10(P-Value)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lrt_plot
```

```{r}
##########################################################################
# Interpretation and Next Steps
##########################################################################
# - Use AIC and BIC values to identify the best model for each response variable.
# - Compare QE values to assess residual variance.
# - Analyze LRT p-values to determine significant differences between models.
# - Select the simplest model that performs well based on these metrics.

```






























```{r}
lapply(names(full_model_results), function(response) {
  message("Inspecting response variable: ", response)
  full_model <- full_model_results[[response]]
  simplified_model <- simplified_model_results[[response]]
  minimal_model <- minimal_model_results[[response]]
  fixed_model <- fixed_effects_model_results[[response]]
  
  message("Full model length: ", length(full_model$yi))
  message("Simplified model length: ", length(simplified_model$yi))
  message("Minimal model length: ", length(minimal_model$yi))
  message("Fixed model length: ", length(fixed_model$yi))
})
```

```{r}
full_model <- full_model_results[[response]]
simplified_model <- simplified_model_results[[response]]
minimal_model <- minimal_model_results[[response]]
fixed_model <- fixed_effects_model_results[[response]]

# Run ANOVA
simplified_anova <- anova(full_model, simplified_model)
minimal_anova <- anova(full_model, minimal_model)
fixed_anova <- anova(full_model, fixed_model)

simplified_anova |> str()
minimal_anova |> str()
fixed_anova |> str()

simplified_aic_full <- simplified_anova[[1]]
simplified_aic_reduced <- simplified_anova[[2]]

simplified_aic_full
simplified_aic_reduced

simplified_aic_comparison <- (simplified_aic_full - simplified_aic_reduced)
simplified_aic_comparison_aic <- simplified_aic_comparison["AIC"]

simplified_aic_comparison_aic

# Visualize!
```

```{r}
# Define a function to process ANOVA results and extract AIC differences
process_anova <- function(full_model, other_model) {
  anova_result <- anova(full_model, other_model)
  
  # Extract AIC values for full and reduced models
  aic_full <- anova_result$fit.stats.f["AIC"]
  aic_reduced <- anova_result$fit.stats.r["AIC"]
  
  # Compute the AIC difference
  aic_diff <- aic_full - aic_reduced
  
  # Return the difference
  return(aic_diff)
}

# Apply the function to all models
simplified_aic_diff <- process_anova(full_model, simplified_model)
minimal_aic_diff <- process_anova(full_model, minimal_model)
fixed_aic_diff <- process_anova(full_model, fixed_model)

# Print the results
cat("Simplified AIC Difference:", simplified_aic_diff, "\n")
cat("Minimal AIC Difference:", minimal_aic_diff, "\n")
cat("Fixed AIC Difference:", fixed_aic_diff, "\n")
```

```{r}
# Prepare data for visualization
aic_data <- data.frame(
  Model = c("Simplified", "Minimal", "Fixed"),
  AIC_Difference = c(simplified_aic_diff, minimal_aic_diff, fixed_aic_diff)
)

# Create the bar plot
ggplot(aic_data, aes(x = Model, y = AIC_Difference, fill = Model)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "AIC Differences Between Models",
    x = "Model Comparison",
    y = "AIC Difference"
  ) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none")

```


The AIC differences reveal important insights about how well each model fits the data compared to the full model. 

1. The **simplified model** has a small AIC difference of 3.69 compared to the full model. This indicates that while it simplifies the structure, it retains much of the explanatory power. The minimal reduction in fit suggests the simplified model could serve as a practical alternative, balancing complexity and accuracy.

2. In contrast, the **minimal model** shows a significant AIC difference of -57.58. This indicates a noticeable loss in explanatory power relative to the full model. While the minimal model removes many complexities, it sacrifices too much accuracy, making it unsuitable for capturing the relationships in the data effectively.

3. The **fixed-effects model** has the worst performance, with an AIC difference of -2792.28. This massive gap suggests that removing random effects entirely leads to a model that fails to account for essential variability in the data. The fixed-effects approach is overly simplistic and does not provide an adequate fit for the observed outcomes.

In summary, the **simplified model** offers a strong balance between simplicity and performance, while the minimal and fixed-effects models fail to retain the necessary explanatory power. The simplified model could be recommended when prioritizing model parsimony, but the full model remains the best choice when accuracy is the primary goal. This analysis highlights the trade-offs between model complexity and goodness-of-fit, underscoring the importance of selecting the right level of simplification for the data.



```{r}
meta_data |> glimpse()
```




# Create a faceted bar plot
heterogeneity_facet_plot <- ggplot(long_heterogeneity_data_adjusted, aes(x = ResponseVariable, y = HeterogeneityValue, fill = ModelType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7) +
  scale_y_break(c(10000, 200000), scales = 0.5) +  # Add y-axis breaks
  facet_wrap(~ HeterogeneityType, scales = "free_y", ncol = 1) +  # Facet by Heterogeneity Type
  labs(
    title = "Partitioned Heterogeneity Across Models (Faceted View with Adjustments)",
    x = "Response Variable",
    y = "Heterogeneity Value",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  scale_fill_brewer(palette = "Set2")

# Display the plot
print(heterogeneity_facet_plot)


```{r}
# Create individual plots for each heterogeneity type

################################################################################################################
# Total Heterogeneity
################################################################################################################
total_plot <- ggplot(long_heterogeneity_data_adjusted %>% filter(HeterogeneityType == "TotalHeterogeneity"),
                     aes(x = ResponseVariable, y = HeterogeneityValue, fill = ModelType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7) +
  scale_y_break(c(8000, 200000), scales = 0.5) +  # Add y-axis breaks
  labs(
    title = "Total Heterogeneity",
    x = "Response Variable",
    y = "Heterogeneity Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "none"  # Remove legend from this plot
  ) +
  scale_fill_brewer(palette = "Set2")

################################################################################################################
# Explained Heterogeneity
################################################################################################################
explained_plot <- ggplot(
  long_heterogeneity_data_adjusted %>% filter(HeterogeneityType == "ExplainedHeterogeneity"),
  aes(x = ResponseVariable, y = HeterogeneityValue, fill = ModelType)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7) +
  scale_y_break(c(5000, 1000000), scales = 0.5) +  # Adjusted breakpoints to match data range
  labs(
    title = "Explained Heterogeneity",
    x = "Response Variable",
    y = "Heterogeneity Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "none"  # Remove legend from this plot
  ) +
  scale_fill_brewer(palette = "Set2")

################################################################################################################
# Residual Heterogeneity
################################################################################################################
residual_plot <- ggplot(long_heterogeneity_data_adjusted %>% filter(HeterogeneityType == "ResidualHeterogeneity"),
                        aes(x = ResponseVariable, y = HeterogeneityValue, fill = ModelType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7) +
  scale_y_break(c(5000, 100000), scales = 0.5) +
  labs(
    title = "Residual Heterogeneity",
    x = "Response Variable",
    y = "Heterogeneity Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "none"  # Remove legend from this plot
  ) +
  scale_fill_brewer(palette = "Set2")

################################################################################################################
# Combine the plots into a single layout with a shared legend
################################################################################################################


```


```{r}
# Grouped Bar Chart: Total, Explained, and Residual Heterogeneity
heterogeneity_bar_plot <- ggplot(visualization_data, aes(x = ResponseVariable, fill = ModelType)) +
  geom_bar(aes(y = TotalHeterogeneity), stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7) +
  geom_bar(aes(y = ExplainedHeterogeneity), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  labs(
    title = "Total, Explained, and Residual Heterogeneity Across Models",
    x = "Response Variable",
    y = "Heterogeneity Value",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

heterogeneity_bar_plot
```
























#############
# STEP 6
##########################################################################################################################################
PUBLICATION-READY PLOTS OF EFFECT SIZE IMPACTS ON RESPONSE VARIABLES OF TEMPERATE SAF FOR EACH SUBSET MODEL FITTING 
##########################################################################################################################################

Forest Plot: Visualizes effect sizes and confidence intervals for response variables.
Ridge Plot: Shows the distribution of effect sizes for each response variable.
Variance Plot: Compares variance components (Tau²) and heterogeneity (I²).
Combined Plot: Combines the forest and ridge plots into a single figure for publication.










```{r}
# Load the saved models
dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

# Load models for all complexity levels
null_model_results <- readRDS(file.path(dir, "fitted_models_null.rds"))
minimal_model_results <- readRDS(file.path(dir, "fitted_models_minimal.rds"))
fixed_effects_model_results <- readRDS(file.path(dir, "fitted_models_fixed_effects.rds"))
simplified_model_results <- readRDS(file.path(dir, "fitted_models_simplified.rds"))
full_model_results <- readRDS(file.path(dir, "fitted_models_full.rds"))

simplified_model_results |> str()
simplified_model_results |> glimpse()
```

```{r}
# WORKING ON THE SIMPLIFIED MODEL
mod_res <- simplified_model_results



# Combine results from all response variables into a single data frame
forest_plot_data <- bind_rows(
  lapply(names(mod_res), function(response) {
    model <- mod_res[[response]]
    
    if (!is.null(model) && !is.null(model$data)) {
      n_effects <- length(model$yi)
      n_studies <- nrow(model$data)
      
      # Ensure the lengths match or skip inconsistent data
      if (n_effects == n_studies) {
        data.frame(
          Study = model$data$id_article,                 # Study IDs
          EffectSize = model$yi,                        # Effect sizes
          CI_Lower = model$yi - 1.96 * sqrt(model$vi),  # Lower CI
          CI_Upper = model$yi + 1.96 * sqrt(model$vi),  # Upper CI
          ResponseVariable = response                   # Response variable
        )
      } else {
        warning(sprintf(
          "Skipping response variable '%s': Mismatched lengths between model$yi (%d) and model$data (%d).",
          response, n_effects, n_studies
        ))
        NULL
      }
    } else {
      warning(sprintf("Skipping response variable '%s': Missing model or data.", response))
      NULL
    }
  })
)

# Check the prepared data
forest_plot_data |> glimpse()
```

```{r}
# Prepare Aggregated Data
aggregated_data <- forest_plot_data %>%
  group_by(ResponseVariable) %>%
  summarise(
    overall_effect = mean(EffectSize, na.rm = TRUE),
    lower_ci = mean(CI_Lower, na.rm = TRUE),
    upper_ci = mean(CI_Upper, na.rm = TRUE),
    num_observations = n(),
    num_studies = n_distinct(Study), # Assuming 'Study' represents unique studies
    size_category = case_when(
      num_studies <= 2 ~ "1-2",
      num_studies <= 4 ~ "3-4",
      num_studies > 4 ~ "5+"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    size_category = factor(size_category, levels = c("1-2", "3-4", "5+")),
    response_rank = rank(overall_effect)
  )

aggregated_data |> glimpse()
```

```{r}
# Define custom colors for response variables
custom_colors <- c(
  "Biodiversity" = "#1b9e77",
  "Crop yield" = "#d95f02",
  "Pest and Disease" = "#7570b3",
  "Product quality" = "#e7298a",
  "Soil quality" = "#66a61e",
  "Water quality" = "#e6ab02"
)

# Create the forest plot with custom colors
forest_plot <- aggregated_data |> 
  ggplot(aes(x = overall_effect, y = reorder(ResponseVariable, response_rank))) +
  # Add points for effect sizes
  geom_point(aes(size = size_category, color = ResponseVariable)) +
  # Add horizontal error bars for confidence intervals
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, color = ResponseVariable), height = 0.2) +
  # Add vertical line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # Customize point size scale
  scale_size_manual(
    values = c("1-2" = 3, "3-4" = 5, "5+" = 7),
    name = "Number of Studies"
  ) +
  # Customize color scale
  scale_color_manual(
    values = custom_colors,
    name = "Response Variable"
  ) +
  # Customize plot labels and appearance
  labs(
    title = "Forest Plot of Response Variables with Custom Colors",
    x = "Effect Size (Overall)",
    y = "Response Variable",
    size = "Number of Studies"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Display the plot
forest_plot
```



```{r}
# Create the ridge plot with custom colors
library(ggridges)
ridge_plot <- forest_plot_data %>% 
  ggplot(aes(x = EffectSize, y = ResponseVariable, fill = ResponseVariable)) +
  geom_density_ridges(alpha = 0.7, scale = 0.8, rel_min_height = 0.02) +
  scale_fill_manual(
    values = custom_colors,
    name = "Response Variable"
  ) +
  labs(
    title = "Enhanced Ridge Plot of Effect Sizes Across Response Variables",
    subtitle = "Distribution of effect sizes categorized by response variable",
    x = "Effect Size Estimate, Ratio of Means (ROM)",
    y = "Response Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    legend.position = "none"
  )

# Display the plot
ridge_plot
```

```{r}
simplified_model_results
```

```{r}
# Sample one observation per article per response variable
sampled_data <- imp_dataset %>%
  group_by(id_article, response_variable) %>%
  slice_head(n = 1)

sampled_data
```
```{r}
# Boxplot of effect sizes by response variable (unique articles)
boxplot_effec_size_response_variable <- sampled_data |> 
  ggplot(aes(x = response_variable, 
             y = yi, 
             fill = response_variable)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot of Effect Sizes (yi) by Response Variable (Unique Articles)",
       x = "Response Variable",
       y = "Effect Size (yi)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_effec_size_response_variable
```


```{r}
simplified_model_results |> str()
```


```{r}
# Function to bootstrap effect sizes
bootstrap_effect_sizes <- function(yi, vi, n_bootstrap = 1000) {
  bootstrapped <- replicate(
    n_bootstrap,
    rnorm(length(yi), mean = yi, sd = sqrt(vi)),
    simplify = TRUE
  )
  as.data.frame(t(bootstrapped)) %>% 
    pivot_longer(cols = everything(), names_to = "id", values_to = "yi_boot")
}

# Combine all response variables
bootstrapped_results <- purrr::map_df(
  names(simplified_model_results), 
  function(response_var) {
    model <- simplified_model_results[[response_var]]
    if (!is.null(model)) {
      data.frame(
        response_variable = response_var,
        bootstrap_effect_sizes(
          yi = model$yi,
          vi = model$vi
        )
      )
    }
  }
)

# Summarize bootstrapped results
summary_results <- bootstrapped_results %>%
  group_by(response_variable) %>%
  summarise(
    mean = mean(yi_boot),
    median = median(yi_boot),
    lower_ci = quantile(yi_boot, 0.025),
    upper_ci = quantile(yi_boot, 0.975)
  )


bootstrapped_results |> glimpse()


# Plotting
modelled_res_bootstrapped_plot <- bootstrapped_results |> 
  ggplot(aes(x = response_variable, y = yi_boot, fill = response_variable)) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", shape = 23, size = 2, color = "black") +
  scale_y_log10() + # Apply log scale to the y-axis
  theme_minimal() +
  labs(
    title = "Bootstrapped ROM Effect Sizes by Response Variable (Log Scale)",
    x = "Response Variable",
    y = "Effect Size (yi) - Log Scale"
  ) +
  theme(legend.position = "none")

modelled_res_bootstrapped_plot

```


```{r}
# Load the required library
library(metafor)

# Check data structure (already done in your example)
# str(imp_dataset)

# Define the meta-analysis model
# Here, we assume 'yi' represents the effect size and 'vi' the variance of effect sizes.
# Random effects model with robust variance estimation
robust_model <- rma.mv(
  yi = yi,                # Effect size measure (e.g., Log Response Ratio)
  V = vi,                 # Variance of effect sizes
  random = ~ 1 | exp_id,  # Random effects structure: exp_id
  data = imp_dataset,     # Your dataset
  method = "REML"         # Restricted Maximum Likelihood Estimation
)

# Display a summary of the model
summary(robust_model)

# Conduct tests for heterogeneity
# Q-test for heterogeneity
heterogeneity_test <- anova(robust_model)
print(heterogeneity_test)

# Visualize results: Forest plot
forest(robust_model, 
       slab = paste(imp_dataset$id_article, imp_dataset$response_variable), 
       xlab = "Effect Size", 
       header = "Study (Article) and Response Variable")

# Test for publication bias (e.g., funnel plot asymmetry)
# Funnel plot and publication bias
funnel(robust_model) # Visual inspection for symmetry

# Simplified regression model for publication bias
simpler_model <- rma.uni(yi, 
                         vi, 
                         data = imp_dataset, 
                         method = "REML")

regtest(simpler_model, model = "lm") # Test for asymmetry in simpler model

# Manual influence diagnostics
residuals <- residuals(robust_model, type = "rstandard") # Standardized residuals
leverage <- hatvalues(robust_model)                     # Leverage values
plot(residuals, leverage, main = "Residuals vs Leverage", xlab = "Residuals", ylab = "Leverage")

# Sensitivity analysis
# Define criteria for exclusion (e.g., standardized residuals > 2)
influential_points <- which(abs(residuals) > 2)
sensitivity_model <- update(robust_model, subset = -influential_points)
sensitivity_summary <- summary(sensitivity_model)
print(sensitivity_summary)

# Save outputs
capture.output(summary(sensitivity_model), file = "sensitivity_analysis_summary.txt")

# Leave-One-Out Analysis (Exclude one study at a time)
id_articles <- unique(imp_dataset$id_article) # Get unique study IDs

loo_results <- lapply(id_articles, function(article) {
  temp_data <- subset(imp_dataset, id_article != article) # Exclude one study at a time
  temp_model <- tryCatch(
    rma.mv(
      yi = yi,                # Effect size measure
      V = vi,                 # Variance of effect sizes
      random = ~ 1 | exp_id,  # Adjusted random effects structure
      data = temp_data,       # Subset data excluding the study
      method = "REML"
    ),
    error = function(e) NA
  )
  if (!inherits(temp_model, "try-error") && !is.na(temp_model$beta[1])) {
    data.frame(Study = article, Beta = temp_model$beta[1], SE = temp_model$se[1], pval = temp_model$pval[1])
  } else {
    data.frame(Study = article, Beta = NA, SE = NA, pval = NA)
  }
})

# Combine results into a single data frame
loo_results_df <- do.call(rbind, loo_results)

# Display LOO results
loo_results_df

loo_results_df |> glimpse()

# Save Leave-One-Out results
write.csv(loo_results_df, "leave_one_out_results_quick.csv", row.names = FALSE)

# Visualize LOO results
plot(loo_results_df$Study, loo_results_df$Beta, 
     type = "b", pch = 19, 
     main = "Leave-One-Out Analysis", 
     xlab = "Excluded Study", 
     ylab = "Effect Size", 
     col = "blue")

# Visualize LOO results as a forest plot
forest(loo_results_df$Beta, 
       ci.lb = loo_results_df$Beta - 1.96 * loo_results_df$SE, 
       ci.ub = loo_results_df$Beta + 1.96 * loo_results_df$SE, 
       slab = paste("Study", loo_results_df$Study), 
       xlab = "Effect Size (Hedges's g)", 
       main = "Leave-One-Out Analysis", 
       refline = 0, 
       header = c("Omitted Study", "Effect Size with 95% CI"))
```
```{r}
imp_dataset |> glimpse()
```


```{r}
# Use your dataset (`imp_dataset`) for meta-analysis
# Meta-analysis using the `meta` package

# Load the required library
library(meta)

# Calculate correlation data and add it to the dataset
# Ensure both silvo and control groups are available
imp_dataset$correlation <- with(imp_dataset, {
  # Calculate correlation based on means and standard deviations
  numerator <- (silvo_mean - control_mean)^2
  denominator <- silvo_sd^2 + control_sd^2
  correlation <- numerator / (numerator + denominator)
  # Ensure correlation values are between -1 and 1
  pmax(pmin(correlation, 1), -1)
})

# Meta-analysis using the `meta` package

datcor <- metacor(
  cor = correlation,               # Effect size correlation from calculated column
  n = silvo_n + control_n,          # Total sample size: sum of silvo and control sample sizes
  data = imp_dataset,               # Your dataset
  studlab = paste(id_article),      # Study labels based on `id_article`
  method.tau = "REML",             # Restricted Maximum Likelihood Estimation
  comb.random = TRUE,               # Random-effects model
  comb.fixed = FALSE,               # Do not combine fixed-effects
  sm = "COR"                       # Effect size metric: Correlation
)

# Create forest plot using the `meta` package
meta::forest(datcor, print.I2 = FALSE)

# Display meta-analysis summary
print(datcor)

# Perform a leave-one-out analysis with `metafor`
dat <- escalc(
  measure = "ROM",               # Effect size metric: Ratio of Means
  yi = silvo_mean / control_mean,  # Effect size: ratio of silvo to control means
  vi = (silvo_sd / silvo_mean)^2 + (control_sd / control_mean)^2, # Variance: using delta method for ratios
  data = imp_dataset,              # Your dataset
  slab = paste(id_article, response_variable, sep = ", ")
)


res <- rma(yi, vi, data = dat)    # Random-effects meta-analysis
inf <- influence(res)             # Influence diagnostics

# Save Leave-One-Out results
saveRDS(inf, "influence_diagnostics_quick.rds")

print(inf)

inf |> str()



# Add slab column for influence diagnostics
dat$slab <- paste(dat$id_article, dat$response_variable, sep = ", ")

# Visualize Influence diagnostics 
# Extract diagnostic metrics for plotting
influence_data <- data.frame(
  Study = dat$slab,
  RStudent = inf$inf$rstudent,
  CookD = inf$inf$cook.d,
  Hat = inf$inf$hat
)

# Plot influence diagnostics
influence_plot <- influence_data |> 
  ggplot(aes(x = Study, y = RStudent)) +
  geom_point(aes(size = CookD, color = Hat)) +
  labs(
    title = "Influence Diagnostics for Meta-Analysis",
    x = "Study",
    y = "Studentized Residuals",
    size = "Cook's D",
    color = "Hat Values"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

influence_plot
```






# Influence diagnostics
inf_diag <- influence(robust_model)
print(inf_diag)
plot(inf_diag)

# Save outputs to a file
# Write model summary to a text file
capture.output(summary(robust_model), file = "meta_analysis_summary.txt")

# Optional: Sensitivity analysis
# Exclude influential points and re-run the model if needed
sensitivity_model <- update(robust_model, subset = !inf_diag$is.influential)
sensitivity_summary <- summary(sensitivity_model)
print(sensitivity_summary)

# Save sensitivity analysis summary
capture.output(sensitivity_summary, file = "sensitivity_analysis_summary.txt")

















####################################################################################################################################################
# Fit and Evaluate All Models
####################################################################################################################################################
model_results <- list()
for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  data_subset <- meta_data[meta_data$response_variable == response, ]
  v_matrix <- v_matrices[[response]]
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  model_results[[response]] <- list(
    null = fit_null_model(data_subset, response),
    minimal = fit_minimal_model(data_subset, response, v_matrix),
    fixed = fit_fixed_effects_model(data_subset, response, v_matrix, moderators),
    simplified = fit_simplified_model(data_subset, response, v_matrix, moderators),
    full = fit_full_model(data_subset, response, v_matrix, moderators)
  )
}

##########################################################################
# Save All Fitted Models
##########################################################################
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

saveRDS(model_results, file = file.path(output_dir, "fitted_models_all.rds"))

cat("\nAll models have been saved successfully!\n")

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")
##########################################################################










```{r}
# Load the library
library(orchaRd)
```

```{r}
simplified_model_results |> str()

# Inspect a sample model
model <- simplified_model_results[["Biodiversity"]]
if (!is.null(model)) {
  print(class(model))      # Should return "rma.mv"
  print(model)             # Summary of the model
} else {
  cat("Model for Biodiversity is NULL\n")
}
```
```{r}
# Generate orchard plots for each response variable
# library(orchaRd)

# Function to preprocess model results
preprocess_models <- function(models_list) {
  valid_models <- list()
  
  for (response in names(models_list)) {
    model <- models_list[[response]]
    if (!is.null(model) && inherits(model, "rma.mv")) {
      tryCatch({
        # Test mod_results function to ensure compatibility
        processed_model <- orchaRd::mod_results(model, mod = "1", group = "exp_id")
        valid_models[[response]] <- model
      }, error = function(e) {
        cat("Excluding model for response variable:", response, "- Error:", e$message, "\n")
      })
    } else {
      cat("Excluding model for response variable:", response, "- Invalid or NULL model.\n")
    }
  }
  
  return(valid_models)
}

# Preprocess models
simplified_model_results_clean <- preprocess_models(simplified_model_results)


for (response in names(simplified_model_results_clean)) {
  model <- simplified_model_results_clean[[response]]
  if (!is.null(model) && inherits(model, "rma.mv")) {
    cat("\nGenerating orchard plot for response variable:", response, "\n")
    tryCatch({
      # Process model results for orchard plotting
      processed_model <- orchaRd::mod_results(
        model,
        mod = "1",             # Overall effect size or specific moderator
        group = "exp_id"       # Grouping variable for random effects
      )
      
      # Generate orchard plot
      orchaRd::orchard_plot(
        processed_model,
        xlab = paste("Effect Size for", response),
        group = "exp_id",      # Grouping variable
        angle = 45             # Rotate labels for better readability
      )
    }, error = function(e) {
      cat("Error generating orchard plot for", response, ":", e$message, "\n")
    })
  } else {
    cat("Skipping response variable:", response, "- Invalid or NULL model.\n")
  }
}
```

```{r}
# Specify the response variable you want to process
response <- "Product quality"  # Replace with the response variable you want to plot

# Check if the model exists and is valid
if (!is.null(simplified_model_results_clean[[response]]) &&
    inherits(simplified_model_results_clean[[response]], "rma.mv")) {
  
  # Retrieve the model
  model <- simplified_model_results_clean[[response]]
  
  # Process the model for orchard plotting
  processed_model <- tryCatch({
    orchaRd::mod_results(
      model,
      mod = "1",             # Overall effect size or specific moderator
      group = "exp_id"       # Grouping variable for random effects
    )
  }, error = function(e) {
    cat("Error processing model for response variable:", response, "-", e$message, "\n")
    return(NULL)
  })
  
  # Check if the processed model is valid
  if (!is.null(processed_model)) {
    # Generate and display the orchard plot
    cat("\nGenerating orchard plot for response variable:", response, "\n")
    orchaRd::orchard_plot(
      processed_model,
      xlab = paste("Effect Size for", response),  # Label for the x-axis
      group = "exp_id",                          # Grouping variable
      angle = 45                                 # Rotate labels for better readability
    )
  } else {
    cat("Unable to generate orchard plot for response variable:", response, "\n")
  }
} else {
  cat("Model for response variable:", response, "is invalid or NULL.\n")
}
```

```{r}
# Filter or prepare the dataset if needed
meta_data <- imp_dataset %>%
  filter(!is.na(yi), !is.na(vi)) %>%   # Remove rows with missing effect size or variance
  mutate(response_variable = as.factor(response_variable)) |>  # Ensure it's a factor
  # Check and convert categorical moderators to factors
  mutate(across(
    c(tree_type, crop_type, age_system, season, soil_texture, no_tree_per_m, tree_height, alley_width),
    as.factor
  ))

meta_data
```
```{r}
# Prepare the dataset
meta_data <- imp_dataset %>%
  filter(!is.na(yi), !is.na(vi)) %>%   # Remove rows with missing effect size or variance
  mutate(response_variable = as.factor(response_variable)) |>  # Ensure it's a factor
  mutate(across(
    c(tree_type, crop_type, age_system, season, soil_texture, no_tree_per_m, tree_height, alley_width),
    as.factor
  ))  # Convert categorical variables to factors

# Create a list to store models
models <- list()
response_levels <- unique(meta_data$response_variable)

for (response in response_levels) {
  sub_data <- meta_data %>% filter(response_variable == response)
  
  # Skip subgroups with insufficient levels
  if (any(sapply(sub_data[, c("tree_type", "crop_type", "age_system", "season", 
                              "soil_texture", "no_tree_per_m", "tree_height", "alley_width")], 
                 function(x) length(unique(x))) < 2)) {
    cat("Skipping", response, "- insufficient levels in one or more moderators\n")
    next
  }
  
  # Fit the meta-analytic model
  models[[response]] <- rma.mv(
    yi = yi,
    V = vi,
    mods = ~ tree_type + crop_type,
    random = ~ 1 | exp_id,  # Random effect for studies
    data = sub_data
  )
}


# Generate Orchard Plots
for (response in names(models)) {
  cat("Creating Orchard Plot for:", response, "\n")
  
  # Generate Orchard Plot
  plot <- orchard_plot(
    models[[response]],
    xlab = "Effect Size (Hedges' g)",
    group = "response_variable",
    angle = 45
  ) + ggplot2::ggtitle(paste("Orchard Plot for", response))
  
  # Print the plot
  print(plot)
}
```

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################
# Start time tracking
start.time <- Sys.time()


#######################################################################################
# Step 1: Check and enforce correct data types
#######################################################################################
col_for_impute <- database_clean_sd |> 
  as.data.frame() |> 
  select(-geometry) |> 
  select(
    # Columns that need to be imputed
    silvo_se, control_se, 
    # Columns that are used by mice to impute values
    tree_age, crop_type, tree_type, bioclim_sub_regions, experiment_year, alley_width, silvo_n, control_n,
    # IDs that are used to back-link imputed values to the dataset
    id_article, id_obs, treat_id, exp_id
  ) |> 
  mutate(
    silvo_se = as.numeric(silvo_se),
    control_se = as.numeric(control_se),
    silvo_n = as.numeric(silvo_n),
    control_n = as.numeric(control_n),
    tree_age = as.numeric(tree_age),
    crop_type = as.factor(crop_type),
    tree_type = as.factor(tree_type),
    bioclim_sub_regions = as.factor(bioclim_sub_regions),
    alley_width = as.factor(alley_width),
    id_article = as.numeric(id_article),
    id_obs = as.numeric(id_obs),
    treat_id = as.numeric(treat_id),
    exp_id = as.numeric(exp_id)
  )

#######################################################################################
# Step 2: Define the function for each imputation method
#######################################################################################
impute_data <- function(data, method_name) {
  if (method_name == "pmm") {
    #######################################################################################
    # Predictive Mean Matching (pmm)
    #######################################################################################
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0 
    
    method <- c(
      "silvo_se" = "pmm",   # Imputed using predictive mean matching
      "control_se" = "pmm", # Imputed using predictive mean matching
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",          # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else if (method_name == "upper_quartile") {
    #######################################################################################
    # Upper Quartile Imputation (uq)
    #######################################################################################
    upper_quartile_variance <- data %>%
      summarise(across(c(silvo_se, control_se), ~ quantile(.^2, 0.75, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "upper_quartile")
    
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), sqrt(upper_quartile_variance$upper_quartile[1]), silvo_se),
        control_se = ifelse(is.na(control_se), sqrt(upper_quartile_variance$upper_quartile[2]), control_se)
      )
    return(data)
    
  } else if (method_name == "mean_imputation") {
    #######################################################################################
    # Mean Imputation (mean)
    #######################################################################################
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), mean(silvo_se, na.rm = TRUE), silvo_se),
        control_se = ifelse(is.na(control_se), mean(control_se, na.rm = TRUE), control_se),
        silvo_n = ifelse(is.na(silvo_n), mean(silvo_n, na.rm = TRUE), silvo_n),
        control_n = ifelse(is.na(control_n), mean(control_n, na.rm = TRUE), control_n)
      )
    return(data)
    
  } else if (method_name == "linear_imputation") {
    #######################################################################################
    # Linear Regression Imputation (lr)
    #######################################################################################
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), predict(lm(silvo_se ~ control_se + tree_age + crop_type, data = data, na.action = na.exclude)), silvo_se),
        control_se = ifelse(is.na(control_se), predict(lm(control_se ~ silvo_se + tree_age + crop_type, data = data, na.action = na.exclude)), control_se)
      )
    return(data)
    
  } else if (method_name == "rf") {
    #######################################################################################
    # Random Forest Imputation (rf)
    #######################################################################################
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0 
    
    method <- c(
      "silvo_se" = "rf",   # Imputed using random forest
      "control_se" = "rf",   # Imputed using random forest
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",          # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else if (method_name == "bayesian") {
    #######################################################################################
    # Bayesian Imputation (by)
    #######################################################################################
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0 
    
    method <- c(
      "silvo_se" = "norm.nob",   # Imputed using Bayesian regression
      "control_se" = "norm.nob",   # Imputed using Bayesian regression
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",          # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else {
    stop("Invalid method name.")
  }
}

#######################################################################################
# Step 3: Apply each imputation method
#######################################################################################
imputation_methods <- c("pmm", "upper_quartile", "mean_imputation", "linear_imputation", "rf", "bayesian")
imputed_datasets <- list()

# Iterate through imputation methods
for (method_name in imputation_methods) {
  cat("Applying", method_name, "imputation...\n")
  
  tryCatch({
    if (method_name %in% c("pmm", "rf", "bayesian")) {
      imputed_mids <- impute_data(col_for_impute, method_name)
      
      # Save mids objects for diagnostics
      if (method_name == "pmm") imputed_datasets[["pmm"]] <- mice::complete(imputed_mids)
      if (method_name == "rf") imputed_datasets[["rf"]] <- mice::complete(imputed_mids)
      if (method_name == "bayesian") imputed_datasets[["bayesian"]] <- mice::complete(imputed_mids)
    } else {
      # Direct dataset modification for other methods
      imputed_datasets[[method_name]] <- impute_data(col_for_impute, method_name)
    }
  }, error = function(e) {
    cat("Error applying", method_name, "imputation:", e$message, "\n")
  })
}


#######################################################################################
# Step 4: Compare Results
#######################################################################################
# Summary of results
for (method_name in imputation_methods) {
  cat("\nSummary of Imputed Dataset -", method_name, ":\n")
  if (!is.null(imputed_datasets[[method_name]])) {
    print(summary(imputed_datasets[[method_name]]))
  } else {
    cat("No data available for", method_name, "\n")
  }
}


##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")

##########################################################################
# imputed_mids_pmm and imputed_mids_linear are the raw mids objects for PMM and linear regression respectively
# imputed_datasets contains completed and capped datasets


##########################################################################
# Last run (04/01-25)
# Total time taken: 18.98046 secs

# Last run (05/01-25)
# Total time taken: 2.224936 mins 

# Last run (11/01-25)
# Total time taken: 2.632763 
```




```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################
# Start time tracking
start.time <- Sys.time()

#######################################################################################
# Step 1: Check and enforce correct data types
#######################################################################################
col_for_impute <- database_clean_sd |> 
  as.data.frame() |> 
  select(-geometry) |> 
  select(
    # Columns that need to be imputed
    silvo_se, control_se, 
    # Columns that are used by mice to impute values
    tree_age, crop_type, tree_type, bioclim_sub_regions, experiment_year, alley_width, silvo_n, control_n,
    # IDs that are used to back-link imputed values to the dataset
    id_article, id_obs, treat_id, exp_id
  ) |> 
  mutate(
    silvo_se = as.numeric(silvo_se),
    control_se = as.numeric(control_se),
    silvo_n = as.numeric(silvo_n),
    control_n = as.numeric(control_n),
    tree_age = as.numeric(tree_age),
    crop_type = as.factor(crop_type),
    tree_type = as.factor(tree_type),
    bioclim_sub_regions = as.factor(bioclim_sub_regions),
    alley_width = as.factor(alley_width),
    id_article = as.numeric(id_article),
    id_obs = as.numeric(id_obs),
    treat_id = as.numeric(treat_id),
    exp_id = as.numeric(exp_id)
  )

#######################################################################################
# Step 2: Define the function for each imputation method
#######################################################################################
impute_data <- function(data, method_name) {
  if (method_name == "pmm") {
    #######################################################################################
    # Predictive Mean Matching (pmm)
    #######################################################################################
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0 
    
    method <- c(
      "silvo_se" = "pmm",   # Imputed using predictive mean matching
      "control_se" = "pmm", # Imputed using predictive mean matching
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",          # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else if (method_name == "upper_quartile") {
    #######################################################################################
    # Upper Quartile Imputation (uq)
    #######################################################################################
    upper_quartile_variance <- data %>%
      summarise(across(c(silvo_se, control_se), ~ quantile(.^2, 0.75, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "upper_quartile")
    
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), sqrt(upper_quartile_variance$upper_quartile[1]), silvo_se),
        control_se = ifelse(is.na(control_se), sqrt(upper_quartile_variance$upper_quartile[2]), control_se)
      )
    return(data)
    
  } else if (method_name == "mean_imputation") {
    #######################################################################################
    # Mean Imputation (mean)
    #######################################################################################
    data <- data %>%
      mutate(
        silvo_se = ifelse(is.na(silvo_se), mean(silvo_se, na.rm = TRUE), silvo_se),
        control_se = ifelse(is.na(control_se), mean(control_se, na.rm = TRUE), control_se),
        silvo_n = ifelse(is.na(silvo_n), mean(silvo_n, na.rm = TRUE), silvo_n),
        control_n = ifelse(is.na(control_n), mean(control_n, na.rm = TRUE), control_n)
      )
    return(data)
    
  } else if (method_name == "linear_imputation") {
    #######################################################################################
    # Linear Regression Imputation (lr)
    #######################################################################################
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0 
    
    method <- c(
      "silvo_se" = "norm.nob",   # Imputed using linear regression
      "control_se" = "norm.nob",   # Imputed using linear regression
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",          # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else if (method_name == "rf") {
    #######################################################################################
    # Random Forest Imputation (rf)
    #######################################################################################
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0 
    
    method <- c(
      "silvo_se" = "rf",   # Imputed using random forest
      "control_se" = "rf",   # Imputed using random forest
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",          # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else if (method_name == "bayesian") {
    #######################################################################################
    # Bayesian Imputation (by)
    #######################################################################################
    pred_matrix <- mice::make.predictorMatrix(data)
    pred_matrix[, c("tree_age", "crop_type", "tree_type", "bioclim_sub_regions", "experiment_year", "alley_width", 
                    "id_article", "id_obs", "treat_id", "exp_id")] <- 0 
    
    method <- c(
      "silvo_se" = "norm.nob",   # Imputed using Bayesian regression
      "control_se" = "norm.nob",   # Imputed using Bayesian regression
      "silvo_n" = "",            # Not imputed
      "control_n" = "",          # Not imputed
      "tree_age" = "",           # Not imputed
      "crop_type" = "",          # Not imputed
      "tree_type" = "",          # Not imputed
      "bioclim_sub_regions" = "",# Not imputed
      "experiment_year" = "",    # Not imputed
      "alley_width" = "",        # Not imputed
      "id_article" = "",          # Not imputed
      "id_obs" = "",             # Not imputed
      "treat_id" = "",           # Not imputed
      "exp_id" = ""              # Not imputed
    )
    
    imputed_mids <- mice(
      data,
      m = 20,
      maxit = 100,
      method = method,
      predictorMatrix = pred_matrix,
      seed = 1234,
      printFlag = FALSE
    )
    return(imputed_mids)
    
  } else {
    stop("Invalid method name.")
  }
}

#######################################################################################
# Step 3: Apply each imputation method
#######################################################################################
imputation_methods <- c("pmm", "upper_quartile", "mean_imputation", "linear_imputation", "rf", "bayesian")
imputed_datasets <- list()

# Separate storage for raw mids objects
imputed_mids_pmm <- NULL
imputed_mids_rf <- NULL
imputed_mids_bayesian <- NULL
imputed_mids_linear <- NULL

# Iterate through imputation methods
for (method_name in imputation_methods) {
  cat("Applying", method_name, "imputation...\n")
  
  tryCatch({
    if (method_name %in% c("pmm", "rf", "bayesian")) {
      imputed_mids <- impute_data(col_for_impute, method_name)
      
      # Save mids objects for diagnostics
      if (method_name == "pmm") imputed_mids_pmm <- imputed_mids
      if (method_name == "rf") imputed_mids_rf <- imputed_mids
      if (method_name == "bayesian") imputed_mids_bayesian <- imputed_mids
      if (method_name == "linear_imputation") imputed_mids_linear <- imputed_mids
      
      # Store completed dataset
      imputed_datasets[[method_name]] <- mice::complete(imputed_mids)
    } else {
      # Direct dataset modification for other methods
      imputed_datasets[[method_name]] <- impute_data(col_for_impute, method_name)
    }
  }, error = function(e) {
    cat("Error applying", method_name, "imputation:", e$message, "\n")
  })
}

# Summary of results
for (method_name in imputation_methods) {
  cat("\nSummary of Imputed Dataset -", method_name, ":\n")
  if (!is.null(imputed_datasets[[method_name]])) {
    print(summary(imputed_datasets[[method_name]]))
  } else {
    cat("No data available for", method_name, "\n")
  }
}

#######################################################################################
# Step 4: Compare Results
#######################################################################################
for (method_name in imputation_methods) {
  cat("\nSummary of Imputed Dataset -", method_name, ":\n")
  print(summary(imputed_datasets[[method_name]]))
}

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")

##########################################################################
# imputed_mids_pmm and imputed_mids_linear are the raw mids objects for PMM and linear regression respectively
# imputed_datasets contains completed and capped datasets


##########################################################################
# Last run (04/01-25)
# Total time taken: 18.98046 secs

# Last run (05/01-25)
# Total time taken: 2.224936 mins 

# Last run (11/01-25)
# Total time taken: 3.70053 mins
imputed_mids_linear
```


```{r}
# Define thresholds for high and non-high variance
high_variance_threshold <- stats::quantile(imp_data_rom$vi, 0.95, na.rm = TRUE)
low_variance_threshold <- stats::quantile(imp_data_rom$vi, 0.05, na.rm = TRUE)

# Create separate datasets
high_variance_data <- imp_data_rom %>%
  filter(vi > high_variance_threshold | vi < low_variance_threshold)

non_high_variance_data <- imp_data_rom %>%
  filter(!(vi > high_variance_threshold | vi < low_variance_threshold))

# Plot for high variance
high_variance_plot <- ggplot(high_variance_data, aes(x = id_article, y = vi, fill = response_variable)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  labs(title = "High Variance Observations", x = "Article ID", y = "Variance (vi)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Remove legend for better visualization
  theme(legend.position = "none")

# Plot for non-high variance
non_high_variance_plot <- ggplot(non_high_variance_data, aes(x = id_article, y = vi, fill = response_variable)) +
  geom_boxplot(outlier.color = "blue", alpha = 0.7) +
  labs(title = "Non-High Variance Observations", x = "Article ID", y = "Variance (vi)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots side by side
combined_plot <- high_variance_plot + non_high_variance_plot + 
  plot_layout(ncol = 2)

# Display the combined plot
print(combined_plot)
```

```{r}
# Aggregate data by id_article
aggregated_high_variance <- high_variance_data %>%
  group_by(id_article, response_variable) %>%
  summarize(
    mean_vi = mean(vi, na.rm = TRUE),
    .groups = "drop"
  )

aggregated_non_high_variance <- non_high_variance_data %>%
  group_by(id_article, response_variable) %>%
  summarize(
    mean_vi = mean(vi, na.rm = TRUE),
    .groups = "drop"
  )

# Plot for high variance
high_variance_plot <- ggplot(aggregated_high_variance, aes(x = as.factor(id_article), y = mean_vi, fill = response_variable)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "High Variance Observations (Aggregated by Article)", x = "Article ID", y = "Mean Variance (vi)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Plot for non-high variance
non_high_variance_plot <- ggplot(aggregated_non_high_variance, aes(x = as.factor(id_article), y = mean_vi, fill = response_variable)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Non-High Variance Observations (Aggregated by Article)", x = "Article ID", y = "Mean Variance (vi)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Combine plots side by side
combined_plot <- high_variance_plot + non_high_variance_plot +
  plot_layout(ncol = 2) & # Combine plots with patchwork
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Display the combined plot
print(combined_plot)
```


```{r}
# Define thresholds for high and non-high variance
high_variance_threshold <- stats::quantile(imp_data_rom$vi, 0.95, na.rm = TRUE)
low_variance_threshold <- stats::quantile(imp_data_rom$vi, 0.05, na.rm = TRUE)

# Create separate datasets
high_variance_data <- imp_data_rom %>%
  filter(vi > high_variance_threshold | vi < low_variance_threshold)

non_high_variance_data <- imp_data_rom %>%
  filter(!(vi > high_variance_threshold | vi < low_variance_threshold))

# Plot for high variance
high_variance_plot <- ggplot(high_variance_data, aes(x = as.factor(id_obs), y = vi, fill = response_variable)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7, outlier.size = 2) +
  geom_text_repel(
    aes(label = id_article), size = 3, color = "black", max.overlaps = Inf, show.legend = FALSE
  ) + # Add article labels only for high variance points
  labs(title = "High Variance Observations", x = "Observation ID", y = "Variance (vi)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x-axis labels for better visibility
    legend.position = "none"
  )

# Plot for non-high variance
non_high_variance_plot <- ggplot(non_high_variance_data, aes(x = as.factor(id_obs), y = vi, fill = response_variable)) +
  geom_boxplot(outlier.color = "blue", alpha = 0.7, outlier.size = 1.5) +
  labs(title = "Non-High Variance Observations", x = "Observation ID", y = "Variance (vi)") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x-axis labels for better visibility
    legend.position = "right"
  )

# Combine plots side by side
combined_plot <- high_variance_plot + non_high_variance_plot +
  plot_layout(ncol = 2) & # Combine plots with patchwork
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Display the combined plot
print(combined_plot)
```


```{r}
# Define thresholds for high and non-high variance
high_variance_threshold <- stats::quantile(imp_data_rom$vi, 0.95, na.rm = TRUE)
low_variance_threshold <- stats::quantile(imp_data_rom$vi, 0.05, na.rm = TRUE)

# Create separate datasets
high_variance_data <- imp_data_rom %>%
  filter(vi > high_variance_threshold | vi < low_variance_threshold)

non_high_variance_data <- imp_data_rom %>%
  filter(!(vi > high_variance_threshold | vi < low_variance_threshold))


# Aggregate data at the article level
high_variance_aggregated <- high_variance_data %>%
  group_by(id_article, response_variable) %>%
  summarise(
    vi_median = median(vi, na.rm = TRUE),
    vi_iqr = IQR(vi, na.rm = TRUE),
    .groups = "drop"
  )

non_high_variance_aggregated <- non_high_variance_data %>%
  group_by(id_article, response_variable) %>%
  summarise(
    vi_median = median(vi, na.rm = TRUE),
    vi_iqr = IQR(vi, na.rm = TRUE),
    .groups = "drop"
  )

# Plot for high variance
high_variance_plot <- ggplot(high_variance_aggregated, aes(x = as.factor(id_article), y = vi_median, fill = response_variable)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "High Variance Observations (Aggregated)", x = "Article ID", y = "Median Variance (vi)") +
  scale_fill_viridis_d(option = "C", direction = -1) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Plot for non-high variance
non_high_variance_plot <- ggplot(non_high_variance_aggregated, aes(x = as.factor(id_article), y = vi_median, fill = response_variable)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Non-High Variance Observations (Aggregated)", x = "Article ID", y = "Median Variance (vi)") +
  scale_fill_viridis_d(option = "C", direction = -1) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Combine plots side by side
combined_plot <- high_variance_plot + non_high_variance_plot +
  plot_layout(ncol = 2) &
  theme(
    plot.margin = margin(10, 10, 10, 10)
  )

# Display the combined plot
print(combined_plot)
```

```{r}
# High variance plot for individual observations
high_variance_plot <- ggplot(high_variance_data, aes(x = as.factor(id_article), y = vi, color = response_variable)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(aes(label = id_obs), size = 3, max.overlaps = 10) + # Add labels for observation IDs
  labs(
    title = "High Variance Observations (Individual)",
    x = "Article ID",
    y = "Variance (vi)",
    color = "Response Variable"
  ) +
  scale_color_viridis_d(option = "C", direction = -1) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Non-high variance plot with aggregated boxplots
non_high_variance_plot <- ggplot(non_high_variance_data, aes(x = as.factor(id_article), y = vi, fill = response_variable)) +
  geom_boxplot(outlier.color = "blue", alpha = 0.7) +
  labs(
    title = "Non-High Variance Observations (Boxplots)",
    x = "Article ID",
    y = "Variance (vi)",
    fill = "Response Variable"
  ) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Combine the two plots
combined_plot <- high_variance_plot + non_high_variance_plot +
  plot_layout(ncol = 2) &
  theme(
    plot.margin = margin(10, 10, 10, 10)
  )

# Display the combined plot
print(combined_plot)

```
```{r}
# High variance plot for individual observations with jitter and pseudo-log scale
high_variance_plot <- ggplot(high_variance_data, aes(x = as.factor(id_article), y = vi, color = response_variable)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3, alpha = 0.7) +
  geom_text_repel(
    aes(label = id_obs),
    position = position_jitter(width = 0.2, height = 0),
    size = 3,
    max.overlaps = Inf # Allow more overlaps to ensure all high variance points are labeled
  ) +
  labs(
    title = "High Variance Observations (Individual, Jittered)",
    x = "Article ID",
    y = "Variance (vi)",
    color = "Response Variable"
  ) +
  scale_color_viridis_d(option = "C", direction = -1) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 0.1),
    breaks = c(0, 0.1, 1, 10, 30),
    labels = c("0", "0.1", "1", "10", "30")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Non-high variance plot with aggregated boxplots and pseudo-log scale
non_high_variance_plot <- ggplot(non_high_variance_data, aes(x = as.factor(id_article), y = vi, fill = response_variable)) +
  geom_boxplot(outlier.color = "blue", alpha = 0.7) +
  labs(
    title = "Non-High Variance Observations (Boxplots, Pseudo-Log)",
    x = "Article ID",
    y = "Variance (vi)",
    fill = "Response Variable"
  ) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 0.1),
    breaks = c(0, 0.1, 1, 10, 30),
    labels = c("0", "0.1", "1", "10", "30")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Combine the two plots
combined_plot <- high_variance_plot + non_high_variance_plot +
  plot_layout(ncol = 2) &
  theme(
    plot.margin = margin(10, 10, 10, 10)
  )

# Display the combined plot
print(combined_plot)
```

# Modify the high-variance plot to increase label size
high_variance_plot <- high_variance_data %>%
  ggplot(aes(x = id_article, y = vi, color = response_variable)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.8, size = 3) +  # Jitter for points
  geom_text_repel(
    aes(label = id_obs),
    size = 10,                  # Increase label text size
    max.overlaps = Inf,         # Ensure all labels are shown
    box.padding = 0.5,          # Adjust padding around labels
    point.padding = 0.3
  ) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 0.1),
    breaks = c(0.01, 0.1, 1, 10, 30),
    labels = c("0.01", "0.1", "1", "10", "30")
  ) +
  labs(
    title = "High Variance Observations (Individual, Jittered)",
    x = "Article ID",
    y = "Variance (vi) [pseudo-log transformed]"
  ) +
  scale_color_manual(values = global_palette) +
  theme_minimal(base_size = 50) +
  theme(
    plot.title = element_text(size = 150, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 100, face = "bold"),
    axis.text = element_text(size = 70),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 70),
    legend.text = element_text(size = 60),
    legend.title = element_text(size = 70, face = "bold"),
    legend.key.size = unit(2, "cm"),
    legend.position = "top"
  )

# Combine updated plots
combined_plot <- high_variance_plot + non_high_variance_plot + 
  plot_layout(ncol = 2)

# Save the updated plot
ggsave(
  filename = file.path(output_dir, "combined_plot_with_large_labels.png"),
  plot = combined_plot,
  width = 28, height = 16, dpi = 600,
  bg = "white"
)




##########################################################################################################################################
FITTING MODELS (SUB-GROUP) FOR EACH RESPONSE VARIABLE USING PRECOMPUTED V_MATRICES
##########################################################################################################################################


Protocol with Four Models Fit Meta-Analysis

```{r}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##########################################################################
# Start time tracking
start.time <- Sys.time()
##########################################################################
# Protocol to Fit and Save Five Suggested Models for Meta-Analysis

##########################################################################
# Model 1: Null Model (Intercept-Only, No Random Effects)
##########################################################################
fit_null_model <- function(data_subset, response_variable) {
  cat("\nFitting null model for response variable:", response_variable, "...\n")
  
  model <- tryCatch({
    rma.mv(
      yi = yi,                           # Dependent variable: effect size
      V = diag(data_subset$vi),          # Variance matrix: diagonal from vi
      mods = ~ 1,                        # Intercept-only model
      data = data_subset,                # Data used for model fitting
      method = "REML",                   # Restricted Maximum Likelihood estimation
      control = list(
        optimizer = "optim",             # Optimizer function
        optim.method = "BFGS",           # Optimization algorithm
        iter.max = 1000,                 # Maximum iterations
        rel.tol = 1e-8                   # Convergence tolerance
      )
    )
  }, error = function(e) {
    cat("Error in null model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Null model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

##########################################################################
# Model 2: Minimal Random Effects Model (Intercept-Only)
##########################################################################
fit_minimal_model <- function(data_subset, response_variable, v_matrix) {
  cat("\nFitting minimal model for response variable:", response_variable, "...\n")
  
  model <- tryCatch({
    rma.mv(
      yi = yi,                           # Dependent variable: effect size
      V = v_matrix,                      # Variance-covariance matrix
      mods = ~ 1,                        # Intercept-only model
      random = ~ 1 | exp_id,             # Random effect at the experiment level
      data = data_subset,                # Data used for model fitting
      method = "REML",                   # Restricted Maximum Likelihood estimation
      control = list(
        optimizer = "optim",             # Optimizer function
        optim.method = "BFGS",           # Optimization algorithm
        iter.max = 1000,                 # Maximum iterations
        rel.tol = 1e-8                   # Convergence tolerance
      )
    )
  }, error = function(e) {
    cat("Error in minimal model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Minimal model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

##########################################################################
# Model 3: Fixed Effects Only Model (With Moderators)
##########################################################################
fit_fixed_effects_model <- function(data_subset, response_variable, v_matrix, moderators) {
  cat("\nFitting fixed effects model for response variable:", response_variable, "...\n")
  
  data_subset <- data_subset %>% mutate(across(all_of(moderators), as.factor)) %>% as.data.frame()
  moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  
  model <- tryCatch({
    rma.mv(
      yi = yi,
      V = v_matrix,
      mods = moderator_formula,
      data = data_subset,
      method = "REML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    )
  }, error = function(e) {
    cat("Error in fixed effects model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Fixed effects model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

##########################################################################
# Model 4: Moderately Simplified Model
##########################################################################
fit_simplified_model <- function(data_subset, response_variable, v_matrix, moderators) {
  cat("\nFitting simplified model for response variable:", response_variable, "...\n")
  
  data_subset <- data_subset %>% mutate(across(all_of(moderators), as.factor)) %>% as.data.frame()
  moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  
  model <- tryCatch({
    rma.mv(
      yi = yi,
      V = v_matrix,
      mods = moderator_formula,
      random = ~ 1 | exp_id,
      data = data_subset,
      method = "REML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    )
  }, error = function(e) {
    cat("Error in simplified model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Simplified model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

##########################################################################
# Model 5: Comprehensive (Full) Model
##########################################################################
fit_full_model <- function(data_subset, response_variable, v_matrix, moderators) {
  cat("\nFitting full model for response variable:", response_variable, "...\n")
  
  data_subset <- data_subset %>% mutate(across(all_of(moderators), as.factor)) %>% as.data.frame()
  moderator_formula <- as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
  
  model <- tryCatch({
    rma.mv(
      yi = yi,
      V = v_matrix,
      mods = moderator_formula,
      random = list(
        ~ 1 | id_article/response_variable, 
        ~ 1 | exp_id
      ),
      data = data_subset,
      method = "REML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    )
  }, error = function(e) {
    cat("Error in full model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Full model fitting completed for response variable:", response_variable, ".\n")
    return(model)
  } else {
    return(NULL)
  }
}

####################################################################################################################################################
# Fit and Evaluate All Models
####################################################################################################################################################
model_results <- list()
for (response in names(v_matrices)) {
  cat("\nProcessing response variable:", response, "\n")
  
  data_subset <- meta_data[meta_data$response_variable == response, ]
  v_matrix <- v_matrices[[response]]
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  model_results[[response]] <- list(
    null = fit_null_model(data_subset, response),
    minimal = fit_minimal_model(data_subset, response, v_matrix),
    fixed = fit_fixed_effects_model(data_subset, response, v_matrix, moderators),
    simplified = fit_simplified_model(data_subset, response, v_matrix, moderators),
    full = fit_full_model(data_subset, response, v_matrix, moderators)
  )
}

##########################################################################
# Save All Fitted Models In One File
##########################################################################
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

saveRDS(model_results, file = file.path(output_dir, "fitted_models_all.rds"))

cat("\nAll models have been saved successfully in a single file!\n")

##########################################################################
# Save All Fitted Models In Seperate Files
##########################################################################
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

saveRDS(lapply(model_results, `[[`, "null"), file = file.path(output_dir, "fitted_models_null.rds"))
saveRDS(lapply(model_results, `[[`, "minimal"), file = file.path(output_dir, "fitted_models_minimal.rds"))
saveRDS(lapply(model_results, `[[`, "fixed"), file = file.path(output_dir, "fitted_models_fixed_effects.rds"))
saveRDS(lapply(model_results, `[[`, "simplified"), file = file.path(output_dir, "fitted_models_simplified.rds"))
saveRDS(lapply(model_results, `[[`, "full"), file = file.path(output_dir, "fitted_models_full.rds"))

cat("\nAll models have been saved successfully in seperate files!\n")

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")
##########################################################################
# Last go (04/12-2024)
# Total time taken: 13.93442 secs
# Processing response variable: Biodiversity 
# Fitting null model for response variable: Biodiversity ...
# Null model fitting completed for response variable: Biodiversity .
# Fitting minimal model for response variable: Biodiversity ...
# Minimal model fitting completed for response variable: Biodiversity .
# Fitting fixed effects model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Fixed effects model fitting completed for response variable: Biodiversity .
# Fitting simplified model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Simplified model fitting completed for response variable: Biodiversity .
# Fitting full model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Full model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting null model for response variable: Greenhouse gas emission ...
# Null model fitting completed for response variable: Greenhouse gas emission .
# Fitting minimal model for response variable: Greenhouse gas emission ...
# Minimal model fitting completed for response variable: Greenhouse gas emission .
# Fitting fixed effects model for response variable: Greenhouse gas emission ...
# Error in fixed effects model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Fitting simplified model for response variable: Greenhouse gas emission ...
# Error in simplified model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Fitting full model for response variable: Greenhouse gas emission ...
# Error in full model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting null model for response variable: Product quality ...
# Null model fitting completed for response variable: Product quality .
# Fitting minimal model for response variable: Product quality ...
# Minimal model fitting completed for response variable: Product quality .
# Fitting fixed effects model for response variable: Product quality ...
# Fixed effects model fitting completed for response variable: Product quality .
# Fitting simplified model for response variable: Product quality ...
# Simplified model fitting completed for response variable: Product quality .
# Fitting full model for response variable: Product quality ...
# Full model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting null model for response variable: Crop yield ...
# Null model fitting completed for response variable: Crop yield .
# Fitting minimal model for response variable: Crop yield ...
# Minimal model fitting completed for response variable: Crop yield .
# Fitting fixed effects model for response variable: Crop yield ...
# Fixed effects model fitting completed for response variable: Crop yield .
# Fitting simplified model for response variable: Crop yield ...
# Simplified model fitting completed for response variable: Crop yield .
# Fitting full model for response variable: Crop yield ...
# Full model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting null model for response variable: Pest and Disease ...
# Null model fitting completed for response variable: Pest and Disease .
# Fitting minimal model for response variable: Pest and Disease ...
# Minimal model fitting completed for response variable: Pest and Disease .
# Fitting fixed effects model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Pest and Disease .
# Fitting simplified model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Pest and Disease .
# Fitting full model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting null model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Null model fitting completed for response variable: Soil quality .
# Fitting minimal model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Minimal model fitting completed for response variable: Soil quality .
# Fitting fixed effects model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Soil quality .
# Fitting simplified model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Soil quality .
# Fitting full model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting null model for response variable: Water quality ...
# Null model fitting completed for response variable: Water quality .
# Fitting minimal model for response variable: Water quality ...
# Minimal model fitting completed for response variable: Water quality .
# Fitting fixed effects model for response variable: Water quality ...
# Error in fixed effects model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# Fitting simplified model for response variable: Water quality ...
# Error in simplified model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# Fitting full model for response variable: Water quality ...
# Error in full model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# All models have been saved successfully!
# Total time taken: 13.93442 secs

# Last go (04/12-2024)
# Total time taken: 22.32941 secs
# Processing response variable: Biodiversity 
# Fitting null model for response variable: Biodiversity ...
# Null model fitting completed for response variable: Biodiversity .
# Fitting minimal model for response variable: Biodiversity ...
# Minimal model fitting completed for response variable: Biodiversity .
# Fitting fixed effects model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Fixed effects model fitting completed for response variable: Biodiversity .
# Fitting simplified model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Simplified model fitting completed for response variable: Biodiversity .
# Fitting full model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Full model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting null model for response variable: Greenhouse gas emission ...
# Null model fitting completed for response variable: Greenhouse gas emission .
# Fitting minimal model for response variable: Greenhouse gas emission ...
# Minimal model fitting completed for response variable: Greenhouse gas emission .
# Fitting fixed effects model for response variable: Greenhouse gas emission ...
# Error in fixed effects model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Fitting simplified model for response variable: Greenhouse gas emission ...
# Error in simplified model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Fitting full model for response variable: Greenhouse gas emission ...
# Error in full model fitting for Greenhouse gas emission : contrasts can be applied only to factors with 2 or more levels 
# Processing response variable: Product quality 
# Fitting null model for response variable: Product quality ...
# Null model fitting completed for response variable: Product quality .
# Fitting minimal model for response variable: Product quality ...
# Minimal model fitting completed for response variable: Product quality .
# Fitting fixed effects model for response variable: Product quality ...
# Fixed effects model fitting completed for response variable: Product quality .
# Fitting simplified model for response variable: Product quality ...
# Simplified model fitting completed for response variable: Product quality .
# Fitting full model for response variable: Product quality ...
# Full model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting null model for response variable: Crop yield ...
# Null model fitting completed for response variable: Crop yield .
# Fitting minimal model for response variable: Crop yield ...
# Minimal model fitting completed for response variable: Crop yield .
# Fitting fixed effects model for response variable: Crop yield ...
# Fixed effects model fitting completed for response variable: Crop yield .
# Fitting simplified model for response variable: Crop yield ...
# Simplified model fitting completed for response variable: Crop yield .
# Fitting full model for response variable: Crop yield ...
# Full model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting null model for response variable: Pest and Disease ...
# Null model fitting completed for response variable: Pest and Disease .
# Fitting minimal model for response variable: Pest and Disease ...
# Minimal model fitting completed for response variable: Pest and Disease .
# Fitting fixed effects model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Pest and Disease .
# Fitting simplified model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Pest and Disease .
# Fitting full model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting null model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Null model fitting completed for response variable: Soil quality .
# Fitting minimal model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Minimal model fitting completed for response variable: Soil quality .
# Fitting fixed effects model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Soil quality .
# Fitting simplified model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Soil quality .
# Fitting full model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting null model for response variable: Water quality ...
# Null model fitting completed for response variable: Water quality .
# Fitting minimal model for response variable: Water quality ...
# Minimal model fitting completed for response variable: Water quality .
# Fitting fixed effects model for response variable: Water quality ...
# Error in fixed effects model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# Fitting simplified model for response variable: Water quality ...
# Error in simplified model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# Fitting full model for response variable: Water quality ...
# Error in full model fitting for Water quality : contrasts can be applied only to factors with 2 or more levels 
# All models have been saved successfully in a single file!
# All models have been saved successfully in seperate files!
# Total time taken: 22.32941 secs

# Last go (05/12-2024)
# Processing response variable: Biodiversity 
# Fitting null model for response variable: Biodiversity ...
# Null model fitting completed for response variable: Biodiversity .
# Fitting minimal model for response variable: Biodiversity ...
# Minimal model fitting completed for response variable: Biodiversity .
# Fitting fixed effects model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Biodiversity .
# Fitting simplified model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Biodiversity .
# Fitting full model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting null model for response variable: Greenhouse gas emission ...
# Null model fitting completed for response variable: Greenhouse gas emission .
# Fitting minimal model for response variable: Greenhouse gas emission ...
# Minimal model fitting completed for response variable: Greenhouse gas emission .
# Fitting fixed effects model for response variable: Greenhouse gas emission ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Greenhouse gas emission .
# Fitting simplified model for response variable: Greenhouse gas emission ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Greenhouse gas emission .
# Fitting full model for response variable: Greenhouse gas emission ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Greenhouse gas emission .
# Processing response variable: Product quality 
# Fitting null model for response variable: Product quality ...
# Null model fitting completed for response variable: Product quality .
# Fitting minimal model for response variable: Product quality ...
# Minimal model fitting completed for response variable: Product quality .
# Fitting fixed effects model for response variable: Product quality ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Product quality .
# Fitting simplified model for response variable: Product quality ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Product quality .
# Fitting full model for response variable: Product quality ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting null model for response variable: Crop yield ...
# Null model fitting completed for response variable: Crop yield .
# Fitting minimal model for response variable: Crop yield ...
# Minimal model fitting completed for response variable: Crop yield .
# Fitting fixed effects model for response variable: Crop yield ...
# Fixed effects model fitting completed for response variable: Crop yield .
# Fitting simplified model for response variable: Crop yield ...
# Simplified model fitting completed for response variable: Crop yield .
# Fitting full model for response variable: Crop yield ...
# Full model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting null model for response variable: Pest and Disease ...
# Null model fitting completed for response variable: Pest and Disease .
# Fitting minimal model for response variable: Pest and Disease ...
# Minimal model fitting completed for response variable: Pest and Disease .
# Fitting fixed effects model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Pest and Disease .
# Fitting simplified model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Pest and Disease .
# Fitting full model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting null model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Null model fitting completed for response variable: Soil quality .
# Fitting minimal model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Minimal model fitting completed for response variable: Soil quality .
# Fitting fixed effects model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Soil quality .
# Fitting simplified model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Soil quality .
# Fitting full model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting null model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Null model fitting completed for response variable: Water quality .
# Fitting minimal model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Minimal model fitting completed for response variable: Water quality .
# Fitting fixed effects model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Water quality .
# Fitting simplified model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Water quality
# Fitting full model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Advarsel: Single-level factor(s) found in 'random' argument. Corresponding 'sigma2' value(s) fixed to 0.Full model fitting completed for response variable: Water quality .
# All models have been saved successfully in a single file!
# All models have been saved successfully in seperate files!
# Total time taken: 19.25503 

# Last go (12/01-2025)
# Processing response variable: Biodiversity 
# Fitting null model for response variable: Biodiversity ...
# Null model fitting completed for response variable: Biodiversity .
# Fitting minimal model for response variable: Biodiversity ...
# Minimal model fitting completed for response variable: Biodiversity .
# Fitting fixed effects model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Biodiversity .
# Fitting simplified model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Biodiversity .
# Fitting full model for response variable: Biodiversity ...
# Advarsel: 14 rows with NAs omitted from model fitting.Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Biodiversity .
# Processing response variable: Greenhouse gas emission 
# Fitting null model for response variable: Greenhouse gas emission ...
# Null model fitting completed for response variable: Greenhouse gas emission .
# Fitting minimal model for response variable: Greenhouse gas emission ...
# Minimal model fitting completed for response variable: Greenhouse gas emission .
# Fitting fixed effects model for response variable: Greenhouse gas emission ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Greenhouse gas emission .
# Fitting simplified model for response variable: Greenhouse gas emission ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Greenhouse gas emission .
# Fitting full model for response variable: Greenhouse gas emission ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Greenhouse gas emission .
# Processing response variable: Product quality 
# Fitting null model for response variable: Product quality ...
# Null model fitting completed for response variable: Product quality .
# Fitting minimal model for response variable: Product quality ...
# Minimal model fitting completed for response variable: Product quality .
# Fitting fixed effects model for response variable: Product quality ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Product quality .
# Fitting simplified model for response variable: Product quality ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Product quality .
# Fitting full model for response variable: Product quality ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Product quality .
# Processing response variable: Crop yield 
# Fitting null model for response variable: Crop yield ...
# Null model fitting completed for response variable: Crop yield .
# Fitting minimal model for response variable: Crop yield ...
# Minimal model fitting completed for response variable: Crop yield .
# Fitting fixed effects model for response variable: Crop yield ...
# Fixed effects model fitting completed for response variable: Crop yield .
# Fitting simplified model for response variable: Crop yield ...
# Simplified model fitting completed for response variable: Crop yield .
# Fitting full model for response variable: Crop yield ...
# Full model fitting completed for response variable: Crop yield .
# Processing response variable: Pest and Disease 
# Fitting null model for response variable: Pest and Disease ...
# Null model fitting completed for response variable: Pest and Disease .
# Fitting minimal model for response variable: Pest and Disease ...
# Minimal model fitting completed for response variable: Pest and Disease .
# Fitting fixed effects model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Pest and Disease .
# Fitting simplified model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Pest and Disease .
# Fitting full model for response variable: Pest and Disease ...
# Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Pest and Disease .
# Processing response variable: Soil quality 
# Fitting null model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Null model fitting completed for response variable: Soil quality .
# Fitting minimal model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Minimal model fitting completed for response variable: Soil quality .
# Fitting fixed effects model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Soil quality .
# Fitting simplified model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Soil quality .
# Fitting full model for response variable: Soil quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Full model fitting completed for response variable: Soil quality .
# Processing response variable: Water quality 
# Fitting null model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Null model fitting completed for response variable: Water quality .
# Fitting minimal model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Minimal model fitting completed for response variable: Water quality .
# Fitting fixed effects model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Fixed effects model fitting completed for response variable: Water quality .
# Fitting simplified model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Simplified model fitting completed for response variable: Water quality .
# Fitting full model for response variable: Water quality ...
# Advarsel: Ratio of largest to smallest sampling variance extremely large. May not be able to obtain stable results.Advarsel: Redundant predictors dropped from the model.Advarsel: Single-level factor(s) found in 'random' argument. Corresponding 'sigma2' value(s) fixed to 0.Full model fitting completed for response variable: Water quality .
# All models have been saved successfully in a single file!
# All models have been saved successfully in seperate files!
# Total time taken: 19.71101 
```



```{r}
ggplot(fit_stats_results, aes(x = Response, y = AIC, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(
    data = best_models, 
    aes(x = Response, y = AIC, color = Model), 
    size = 5, shape = 18
  ) +
  facet_wrap(~ Model, scales = "free_y", nrow = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Best Performing Models by AIC",
    x = "Response Variable",
    y = "AIC Value"
  )
```


Calculate standard deviations from standard errors and sample sizes only when _sd is missing

```{r}
# Calculate standard deviations from standard errors and sample sizes only when _sd is missing
database_clean_sd <- database_clean |>
  mutate(
    # Preserve existing silvo_sd and calculate only if missing
    silvo_sd = if_else(is.na(silvo_sd), silvo_se * sqrt(silvo_n), silvo_sd),
    
    # Preserve existing control_sd and calculate only if missing
    control_sd = if_else(is.na(control_sd), control_se * sqrt(control_n), control_sd),
    
    # Create separate columns for calculated standard deviations for better transparency
    silvo_sd_calculated = if_else(is.na(silvo_sd), silvo_se * sqrt(silvo_n), NA_real_),
    control_sd_calculated = if_else(is.na(control_sd), control_se * sqrt(control_n), NA_real_)
  )
```













##########################################################################################################################################
Workflow for Calculating _SD with Comprehensive Conditions
##########################################################################################################################################


# Workflow for Calculating _SD with Comprehensive Conditions













```{r}
# Imputed dataset
imp_pmm_best <- merged_data |> 
  # Modify the imp_pmm_best dataset before saving
  # Remove existing columns
  select(-c(control_se, silvo_se, control_se_original, silvo_se_original)) |>  
  rename(
    # Rename control_se_imputed to control_se
    control_se = control_se_imputed, 
    # Rename silvo_se_imputed to silvo_se
    silvo_se = silvo_se_imputed      
  ) |> 
  as.data.frame()|> 
  # RECALCALCULATE STANDARD DEVIATION FOR IMPUTED DATASET
  mutate(
    # Calculate standard deviation for silvo group
    silvo_sd = silvo_se * sqrt(silvo_n),
    # Calculate standard deviation for control group
    control_sd = control_se * sqrt(control_n)
  ) |> 
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )


# Non-imputed dataset (remove geometry if necessary)
non_imp_dataset <- database_clean_sd |> 
  as.data.frame() |> 
  # RECALCALCULATE STANDARD DEVIATION 
  mutate(
    # Calculate standard deviation for silvo group
    silvo_sd = silvo_se * sqrt(silvo_n),
    # Calculate standard deviation for control group
    control_sd = control_se * sqrt(control_n)
  ) |> 
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative mata-analysis effect size info
    silvo_mean, silvo_se, silvo_sd, silvo_n, control_mean, control_se, control_sd, control_n
  )
```




```{r}
# Workflow for Calculating _SD with Comprehensive Conditions

# Step 1: Define the full workflow to calculate _SD with explicit handling of original _SE, imputed _SE, and weighted average CV.

# Load necessary libraries
library(dplyr)

# Step 2: Calculate the CV for studies that report SDs
calculate_cv <- function(data) {
  data %>%
    mutate(
      # Calculate CV for silvo group (when SD and mean are available)
      silvo_cv = ifelse(!is.na(silvo_sd) & silvo_mean > 0, silvo_sd / silvo_mean, NA),
      
      # Calculate CV for control group (when SD and mean are available)
      control_cv = ifelse(!is.na(control_sd) & control_mean > 0, control_sd / control_mean, NA)
    )
}

# Step 3: Calculate the weighted average (pooled) CV
calculate_pooled_cv <- function(data) {
  pooled_values <- data %>%
    filter(!is.na(silvo_cv) & !is.na(control_cv)) %>%
    summarise(
      # Weighted average CV for silvo group
      pooled_silvo_cv = sqrt(sum((silvo_cv^2) * silvo_n, na.rm = TRUE) / sum(silvo_n, na.rm = TRUE)),
      
      # Weighted average CV for control group
      pooled_control_cv = sqrt(sum((control_cv^2) * control_n, na.rm = TRUE) / sum(control_n, na.rm = TRUE))
    )
  return(pooled_values)
}

# Step 4: Apply conditional logic to calculate SD
calculate_sd <- function(data, pooled_cv) {
  data %>%
    mutate(
      # Calculate SD using original _SE values (if available)
      silvo_sd_from_se_original = ifelse(!is.na(silvo_se_original), silvo_se_original * sqrt(silvo_n), NA),
      control_sd_from_se_original = ifelse(!is.na(control_se_original), control_se_original * sqrt(control_n), NA),
      
      # Calculate SD using imputed _SE values (if available)
      silvo_sd_from_se_imputed = ifelse(is.na(silvo_se_original) & !is.na(silvo_se_imputed), silvo_se_imputed * sqrt(silvo_n), NA),
      control_sd_from_se_imputed = ifelse(is.na(control_se_original) & !is.na(control_se_imputed), control_se_imputed * sqrt(control_n), NA),
      
      # Calculate SD using pooled CV (for missing SDs)
      silvo_sd_from_weight_cv = ifelse(is.na(silvo_se_original) & is.na(silvo_se_imputed), pooled_cv$pooled_silvo_cv * silvo_mean, NA),
      control_sd_from_weight_cv = ifelse(is.na(control_se_original) & is.na(control_se_imputed), pooled_cv$pooled_control_cv * control_mean, NA),
      
      # Warning for completely missing SE values
      silvo_warning = ifelse(is.na(silvo_se_original) & is.na(silvo_se_imputed) & is.na(pooled_cv$pooled_silvo_cv), "Warning: Missing silvo SE values for this observation.", NA),
      control_warning = ifelse(is.na(control_se_original) & is.na(control_se_imputed) & is.na(pooled_cv$pooled_control_cv), "Warning: Missing control SE values for this observation.", NA)
    )
}

# Step 5: Workflow implementation
workflow <- function(data) {
  # Step 5a: Calculate CV
  data <- calculate_cv(data)
  
  # Step 5b: Calculate pooled CV
  pooled_cv <- calculate_pooled_cv(data)
  
  # Step 5c: Calculate SD using the comprehensive conditions
  data <- calculate_sd(data, pooled_cv)
  
  # Return the processed dataset
  return(data)
}

# Example usage
# Assuming `imp_pmm_best` is the dataset with necessary columns
processed_data <- workflow(imp_pmm_best)

# Glimpse the processed dataset
processed_data |> glimpse()

# Save the processed dataset
saveRDS(processed_data, file = "processed_data_with_sd.rds")

```









```{r}
# Modify the `imp_pmm_best` dataset to retain all necessary columns for the Workflow for Calculating _SD with Comprehensive Conditions

# Imputed dataset
imp_pmm_best <- merged_data |> 
  # Retain all required columns including original and imputed SE values
  select(
    id_article, id_obs, treat_id, exp_id,
    response_variable, sub_response_variable, location, final_lat, final_lon, exp_site_loc, experiment_year,
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    silvo_mean, silvo_se_original, silvo_se_imputed, silvo_sd,
    silvo_n, control_mean, control_se_original, control_se_imputed, control_sd, control_n
  ) |>  
  # Ensure column names are consistent and informative
  rename(
    control_se_original = control_se_original,
    control_se_imputed = control_se_imputed,
    silvo_se_original = silvo_se_original,
    silvo_se_imputed = silvo_se_imputed
  ) |> 
  # Convert to a data frame for compatibility
  as.data.frame() |> 
  # Rearrange columns for better readability
  relocate(
    # Overall ID info
    id_article, id_obs, treat_id, exp_id,
    # Response variable info
    response_variable, sub_response_variable,
    # Geographic and temporal info
    location, final_lat, final_lon, exp_site_loc, experiment_year,
    # Moderators info
    tree_type, crop_type, age_system, tree_age, season, soil_texture, no_tree_per_m, tree_height, alley_width,
    # Quantitative meta-analysis effect size info
    silvo_mean, silvo_se_original, silvo_se_imputed, silvo_sd, silvo_n,
    control_mean, control_se_original, control_se_imputed, control_sd, control_n
  )

# Confirm the structure of the modified dataset
imp_pmm_best |> glimpse()

# The dataset now retains original and imputed _SE values, as well as placeholders for _SD calculations,
# ensuring compatibility with the Workflow for Calculating _SD with Comprehensive Conditions.
```

```{r}
# Workflow for Calculating _SD with Comprehensive Conditions

# Step 1: Define the full workflow to calculate _SD with explicit handling of original _SE, imputed _SE, and weighted average CV.

# Step 2: Calculate the CV for studies that report SDs
calculate_cv <- function(data) {
  data %>%
    mutate(
      # Calculate CV for silvo group (when SD and mean are available)
      silvo_cv = ifelse(!is.na(silvo_sd) & silvo_mean > 0, silvo_sd / silvo_mean, NA),
      
      # Calculate CV for control group (when SD and mean are available)
      control_cv = ifelse(!is.na(control_sd) & control_mean > 0, control_sd / control_mean, NA)
    )
}

# Step 3: Calculate the weighted average (pooled) CV
calculate_pooled_cv <- function(data) {
  pooled_values <- data %>%
    filter(!is.na(silvo_cv) & !is.na(control_cv)) %>%
    summarise(
      # Weighted average CV for silvo group
      pooled_silvo_cv = sqrt(sum((silvo_cv^2) * silvo_n, na.rm = TRUE) / sum(silvo_n, na.rm = TRUE)),
      
      # Weighted average CV for control group
      pooled_control_cv = sqrt(sum((control_cv^2) * control_n, na.rm = TRUE) / sum(control_n, na.rm = TRUE))
    )
  return(pooled_values)
}

# Step 4: Apply conditional logic to calculate SD
calculate_sd <- function(data, pooled_cv) {
  data %>%
    mutate(
      # Calculate SD using original _SE values (if available)
      silvo_sd_from_se_original = ifelse(!is.na(silvo_se_original), silvo_se_original * sqrt(silvo_n), NA),
      control_sd_from_se_original = ifelse(!is.na(control_se_original), control_se_original * sqrt(control_n), NA),
      
      # Calculate SD using imputed _SE values (if available)
      silvo_sd_from_se_imputed = ifelse(is.na(silvo_se_original) & !is.na(silvo_se_imputed), silvo_se_imputed * sqrt(silvo_n), NA),
      control_sd_from_se_imputed = ifelse(is.na(control_se_original) & !is.na(control_se_imputed), control_se_imputed * sqrt(control_n), NA),
      
      # Calculate SD using pooled CV (for missing SDs)
      silvo_sd_from_weight_cv = ifelse(is.na(silvo_se_original) & is.na(silvo_se_imputed), pooled_cv$pooled_silvo_cv * silvo_mean, NA),
      control_sd_from_weight_cv = ifelse(is.na(control_se_original) & is.na(control_se_imputed), pooled_cv$pooled_control_cv * control_mean, NA),
      
      # Warning for completely missing SE values
      silvo_warning = ifelse(is.na(silvo_se_original) & is.na(silvo_se_imputed) & is.na(pooled_cv$pooled_silvo_cv), "Warning: Missing silvo SE values for this observation.", NA),
      control_warning = ifelse(is.na(control_se_original) & is.na(control_se_imputed) & is.na(pooled_cv$pooled_control_cv), "Warning: Missing control SE values for this observation.", NA)
    )
}

# Step 5: Workflow implementation
workflow <- function(data) {
  # Step 5a: Calculate CV
  data <- calculate_cv(data)
  
  # Step 5b: Calculate pooled CV
  pooled_cv <- calculate_pooled_cv(data)
  
  # Step 5c: Calculate SD using the comprehensive conditions
  data <- calculate_sd(data, pooled_cv)
  
  # Return the processed dataset
  return(data)
}

# Example usage
# Assuming `imp_pmm_best` is the dataset with necessary columns
processed_data <- workflow(imp_pmm_best)

# Glimpse the processed dataset
processed_data |> glimpse()
```
```{r}
# Define the column groups
se_columns <- c("silvo_se_original", "silvo_se_imputed", "control_se_original", "control_se_imputed")
# Updated SD columns with weight-based CV
sd_columns <- c(
  "silvo_sd", "control_sd", 
  "silvo_sd_from_se_original", "control_sd_from_se_original", 
  "silvo_sd_from_se_imputed", "control_sd_from_se_imputed", 
  "silvo_sd_from_weight_cv", "control_sd_from_weight_cv"  
)

cv_columns <- c("silvo_cv", "control_cv")

# Function to pivot and plot density plots
plot_density <- function(data, columns, title, x_label) {
  data %>%
    select(all_of(columns)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = value, fill = variable)) +
    geom_density(alpha = 0.5) +
    labs(title = title, x = x_label, y = "Density") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_discrete(name = "Variables")
}

# Density plot for SE variables
plot_density(processed_data, se_columns, "Density Plot of SE Variables", "SE Value")

# Density plot for SD variables
plot_density(processed_data, sd_columns, "Density Plot of SD Variables", "SD Value")

# Density plot for CV variables
plot_density(processed_data, cv_columns, "Density Plot of CV Variables", "CV Value")
```
```{r}
# Function to pivot and plot density plots with pseudo log scaling
plot_density_pseudo <- function(data, columns, title, x_label, y_label) {
  data %>%
    select(all_of(columns)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = value, fill = variable)) +
    geom_density(alpha = 0.5) +
    scale_x_continuous(trans = "pseudo_log") +  # Apply pseudo-log scaling to x-axis
    scale_y_continuous(trans = "pseudo_log") +  # Apply pseudo-log scaling to y-axis
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_discrete(name = "Variables")
}

# Density plot for SE variables with pseudo scaling
plot_density_pseudo(processed_data, se_columns, 
                    "Density Plot of SE Variables (Pseudo Log Scale)", 
                    "SE Value", "Density")

# Density plot for SD variables with pseudo scaling
plot_density_pseudo(processed_data, sd_columns, 
                    "Density Plot of SD Variables (Pseudo Log Scale)", 
                    "SD Value", "Density")

# Density plot for CV variables with pseudo scaling
plot_density_pseudo(processed_data, cv_columns, 
                    "Density Plot of CV Variables (Pseudo Log Scale)", 
                    "CV Value", "Density")

```
