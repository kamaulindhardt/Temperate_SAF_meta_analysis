






#############
# STEP 4
##########################################################################################################################################
MODEL COMPARISONS, EVALUATION AND DIAGNOSTICS
##########################################################################################################################################




```{r}
##########################################################################
# FITTING MODELS WITH ML FOR DIRECT COMPARISONS
##########################################################################

# Define `include_interaction` variable to toggle interaction models
include_interaction <- TRUE  # Set to FALSE if interaction terms are not required

##########################################################################
# Set up parallel processing and start time tracking
##########################################################################
plan(multisession, workers = parallel::detectCores() - 1)
start.time <- Sys.time()

##########################################################################
# Function to fit models with hierarchical complexity
##########################################################################
fit_model <- function(data_subset, response_variable, v_matrix, moderators, random_effects = NULL, intercept = TRUE, include_interaction = FALSE) {
  cat("\nFitting model for response variable:", response_variable, "...\n")
  
  # Build moderator formula based on interaction and intercept settings
  moderator_formula <- if (is.null(moderators) || length(moderators) == 0) {
    ~ 1  # Intercept-only model
  } else if (include_interaction) {
    if (intercept) {
      as.formula(paste("yi ~", paste(moderators, collapse = " * ")))
    } else {
      as.formula(paste("yi ~", paste(moderators, collapse = " * "), "- 1"))
    }
  } else {
    if (intercept) {
      as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
    } else {
      as.formula(paste("yi ~", paste(moderators, collapse = " + "), "- 1"))
    }
  }
  
  # Fit the model
  tryCatch({
    rma.mv(
      yi = data_subset$yi,
      V = v_matrix,
      mods = moderator_formula,
      random = random_effects,
      data = data_subset,
      method = "ML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
}

##########################################################################
# Fit Models for Each Response Variable
##########################################################################
model_results_ml <- lapply(names(v_matrices), function(response) {
  cat("\nProcessing response variable:", response, "\n")
  
  data_subset <- meta_data[meta_data$response_variable == response, ]
  v_matrix <- v_matrices[[response]]
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  list(
    A_null = fit_model(data_subset, response, v_matrix, NULL, intercept = TRUE),
    B_minimal_random = fit_model(data_subset, response, v_matrix, NULL, random_effects = ~ 1 | exp_id, intercept = TRUE),
    C_fixed_effects = fit_model(data_subset, response, v_matrix, moderators, intercept = TRUE),
    D_random_effects = fit_model(data_subset, response, v_matrix, moderators, random_effects = ~ 1 | exp_id, intercept = TRUE),
    E_random_effects_interaction = fit_model(data_subset, response, v_matrix, moderators, random_effects = ~ 1 | exp_id, intercept = TRUE, include_interaction = TRUE),
    F_full = fit_model(data_subset, response, v_matrix, moderators, random_effects = list(~ 1 | id_article/response_variable, ~ 1 | exp_id), intercept = TRUE),
    G_full_interaction = fit_model(data_subset, response, v_matrix, moderators, random_effects = list(~ 1 | id_article/response_variable, ~ 1 | exp_id), intercept = TRUE, include_interaction = TRUE)
  )
})

names(model_results_ml) <- names(v_matrices)

##########################################################################
# Save Fitted Models
##########################################################################
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

# Save all models in one file
saveRDS(model_results_ml, file = file.path(output_dir, "fitted_models_ml_all.rds"))

# Save each model type in separate files
model_types <- c("A_null", "B_minimal_random", "C_fixed_effects", "D_random_effects", "E_random_effects_interaction", "F_full", "G_full_interaction")

for (type in model_types) {
  saveRDS(lapply(model_results_ml, `[[`, type), file = file.path(output_dir, paste0("fitted_models_", type, "_ml.rds")))
}

cat("\nAll models fitted with ML have been saved successfully.\n")

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")

# Last go (18/01-2025)
# Total time taken: 28.87752  secs
```



```{r}
##########################################################################
# Combine All Comparison Results into a DataFrame
##########################################################################

# Load the saved models if needed
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
model_results_ml <- readRDS(file.path(output_dir, "fitted_models_ml_all.rds"))

# Initialize a list to store comparison results
comparison_results <- list()

# Iterate over response variables to perform model comparisons
for (response in names(model_results_ml)) {
  cat("\nComparing models for response variable:", response, "\n")
  
  # Extract the models for the current response variable
  models <- model_results_ml[[response]]
  
  # Perform comparisons for multiple models using ANOVA
  comparison <- tryCatch({
    list(
      Full_vs_Random = anova(models$G_full_interaction, models$D_random_effects),
      Random_vs_Fixed = anova(models$D_random_effects, models$C_fixed_effects),
      Full_vs_Null = anova(models$G_full_interaction, models$A_null)
    )
  }, error = function(e) {
    cat("Error in comparison for", response, ":", e$message, "\n")
    return(NULL)
  })
  
  # Store the comparison results
  comparison_results[[response]] <- comparison
}

# Combine all comparisons into a single data frame for each model pair
comparison_df <- do.call(rbind, lapply(names(comparison_results), function(response) {
  result <- comparison_results[[response]]
  if (!is.null(result)) {
    do.call(rbind, lapply(names(result), function(comp) {
      as.data.frame(result[[comp]]) |>
        dplyr::mutate(
          Response = response,
          Comparison = comp
        )
    }))
  } else {
    NULL  # Exclude responses with no comparison results
  }
}))

# View the combined comparison results
# print(comparison_df)



##########################################################################
# Modifying the Resulting DataFrame
##########################################################################

# Standardize model names and add Model_Type column
comparison_df <- comparison_df |>
  dplyr::mutate(
    Model_Type = case_when(
      grepl("Full_vs_Random", Comparison) ~ "Full",
      grepl("Random_vs_Fixed", Comparison) ~ "Random",
      grepl("Full_vs_Null", Comparison) ~ "Null",
      TRUE ~ "Unknown"
    )
  ) |>
  dplyr::relocate(Model_Type, .before = everything())  # Move Model_Type to the first column


# comparison_df |> str()
comparison_df |> str()
```

```{r}
##########################################################################
# Transform the DataFrame for Visualization
##########################################################################
##########################################################################
# Step 1: Update the Model_Type Based on Comparison
##########################################################################
comparison_df_clean <- comparison_df %>%
  dplyr::mutate(
    Actual_Model_Type = dplyr::case_when(
      Comparison == "Full_vs_Random" & row_number() %% 2 == 1 ~ "Full",
      Comparison == "Full_vs_Random" & row_number() %% 2 == 0 ~ "Random",
      Comparison == "Random_vs_Fixed" & row_number() %% 2 == 1 ~ "Random",
      Comparison == "Random_vs_Fixed" & row_number() %% 2 == 0 ~ "Fixed",
      Comparison == "Full_vs_Null" & row_number() %% 2 == 1 ~ "Full",
      Comparison == "Full_vs_Null" & row_number() %% 2 == 0 ~ "Null",
      TRUE ~ Model_Type
    )
  )  |> relocate(Comparison, Actual_Model_Type, Response, AIC, BIC, AICc, logLik, LRT, pval)

##########################################################################
# Step 2: Populate Metric Columns
##########################################################################
# Create a function to populate metrics based on Actual_Model_Type
populate_metrics <- function(data, metric_cols) {
  for (metric in metric_cols) {
    data <- data %>%
      dplyr::mutate(
        !!paste0(metric, "_Full") := ifelse(Actual_Model_Type == "Full", .data[[metric]], NA),
        !!paste0(metric, "_Random") := ifelse(Actual_Model_Type == "Random", .data[[metric]], NA),
        !!paste0(metric, "_Fixed") := ifelse(Actual_Model_Type == "Fixed", .data[[metric]], NA),
        !!paste0(metric, "_Null") := ifelse(Actual_Model_Type == "Null", .data[[metric]], NA)
      )
  }
  return(data)
}

# Apply the function to populate metrics
metrics <- c("AIC", "BIC", "logLik", "pval")
aligned_data <- populate_metrics(comparison_df_clean, metrics)

##########################################################################
# Step 3: Aggregate Metrics by Response and Comparison
##########################################################################
aligned_data_agg <- aligned_data %>%
  group_by(Response, Comparison) %>%
  summarise(across(starts_with("AIC"), ~ max(., na.rm = TRUE)),
            across(starts_with("BIC"), ~ max(., na.rm = TRUE)),
            across(starts_with("logLik"), ~ max(., na.rm = TRUE)),
            across(starts_with("pval"), ~ max(., na.rm = TRUE)),
            .groups = "drop")

##########################################################################
# Step 4: Calculate Differences in Metrics
##########################################################################
aligned_data_pronto <- aligned_data_agg %>%
  mutate(
    # Full vs Random
    AIC_Diff_Full_vs_Random = AIC_Full - AIC_Random,
    BIC_Diff_Full_vs_Random = BIC_Full - BIC_Random,
    logLik_Diff_Full_vs_Random = logLik_Full - logLik_Random,
    pval_Diff_Full_vs_Random = pval_Full - pval_Random,
    
    # Random vs Fixed
    AIC_Diff_Random_vs_Fixed = AIC_Random - AIC_Fixed,
    BIC_Diff_Random_vs_Fixed = BIC_Random - BIC_Fixed,
    logLik_Diff_Random_vs_Fixed = logLik_Random - logLik_Fixed,
    pval_Diff_Random_vs_Fixed = pval_Random - pval_Fixed,
    
    # Full vs Null
    AIC_Diff_Full_vs_Null = AIC_Full - AIC_Null,
    BIC_Diff_Full_vs_Null = BIC_Full - BIC_Null,
    logLik_Diff_Full_vs_Null = logLik_Full - logLik_Null,
    pval_Diff_Full_vs_Null = pval_Full - pval_Null
  )

##########################################################################
# Step 5: Handle Missing Values (Inf, NaN)
##########################################################################
aligned_data_pronto <- aligned_data_pronto %>%
  mutate(across(starts_with("AIC_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)),
         across(starts_with("BIC_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)),
         across(starts_with("logLik_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)),
         across(starts_with("pval_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

##########################################################################
# Step 6: View Results
##########################################################################
aligned_data_pronto %>%
  dplyr::select(Response, starts_with("AIC_Diff"), starts_with("BIC_Diff")) %>%
  print()

aligned_data_pronto |> str()
```

```{r}
# Prepare data for plotting
absolute_metrics_plot_data <- aligned_data %>%
  dplyr::select(Response, Actual_Model_Type, AIC, BIC) %>%
  tidyr::pivot_longer(
    cols = c(AIC, BIC),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::filter(!is.na(Value))

# Create the plot
absolute_metrics_plot <- ggplot(absolute_metrics_plot_data, aes(x = Response, y = Value, fill = Actual_Model_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Absolute AIC and BIC Values by Model Type Across Response Variables",
    x = "Response Variable",
    y = "Metric Value",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top"
  )  +
  ggbreak::scale_y_break(c(5000, 20000), scales = 0.8) +
  ggbreak::scale_y_break(c(50000, 200000), scales = 0.8)

# View the plot
absolute_metrics_plot
```


```{r}
##########################################################################
# Visualization
##########################################################################

# Visualization 1: AIC Differences with ggbreak
aic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, AIC_Diff_Full_vs_Random, AIC_Diff_Random_vs_Fixed, AIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("AIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "AIC_Difference"
  ) %>%
  dplyr::filter(!is.na(AIC_Difference))

aic_plot_break <- ggplot(aic_diff_plot_data, aes(x = Response, y = AIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in AIC Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "AIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

# Visualization 2: BIC Differences with ggbreak
bic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, BIC_Diff_Full_vs_Random, BIC_Diff_Random_vs_Fixed, BIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("BIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "BIC_Difference"
  ) %>%
  dplyr::filter(!is.na(BIC_Difference))

bic_plot_break <- ggplot(bic_diff_plot_data, aes(x = Response, y = BIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in BIC Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "BIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

# Visualization 3: Log-Likelihood Differences with ggbreak
loglik_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, logLik_Diff_Full_vs_Random, logLik_Diff_Random_vs_Fixed, logLik_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("logLik_Diff"),
    names_to = "Comparison_Type",
    values_to = "LogLik_Difference"
  ) %>%
  dplyr::filter(!is.na(LogLik_Difference))

loglik_plot_break <- ggplot(loglik_diff_plot_data, aes(x = Response, y = LogLik_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in Log-Likelihood Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "Log-Likelihood Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(100, 5000), scales = 0.6) +
  ggbreak::scale_y_break(c(10000, 50000), scales = 0.6)

# Combine Plots Using patchwork
combined_plot <- aic_plot_break / bic_plot_break / loglik_plot_break +
  plot_layout(ncol = 1) +
  plot_annotation(title = "Comparison Metrics Across Models")

# Display Combined Plot
print(combined_plot)
```


```{r}
# Visualization 1: Differences in AIC Across Comparison Types
# Prepare data for plotting
aic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, AIC_Diff_Full_vs_Random, AIC_Diff_Random_vs_Fixed, AIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("AIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "AIC_Difference"
  ) %>%
  dplyr::filter(!is.na(AIC_Difference))

# Create the plot
# AIC Differences with ggbreak
aic_plot_break <- aic_diff_plot_data |> 
  ggplot(aes(x = Response, y = AIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous() +
  labs(
    title = "Differences in AIC Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "AIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

# Plot
aic_plot_break
```
Visualization 2: Differences in BIC Across Comparison Types
```{r}
# Prepare data for plotting
bic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, BIC_Diff_Full_vs_Random, BIC_Diff_Random_vs_Fixed, BIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("BIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "BIC_Difference"
  ) %>%
  dplyr::filter(!is.na(BIC_Difference))

# Create the plot
bic_plot_break <-
  bic_diff_plot_data |> 
  ggplot(aes(x = Response, y = BIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in BIC Across Comparison Types",
    x = "Response Variable",
    y = "BIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

bic_plot_break
```
Visualization 3: Log-Likelihood Differences Across Comparison Types
```{r}
# Prepare data for plotting
loglik_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, logLik_Diff_Full_vs_Random, logLik_Diff_Random_vs_Fixed, logLik_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("logLik_Diff"),
    names_to = "Comparison_Type",
    values_to = "LogLik_Difference"
  ) %>%
  dplyr::filter(!is.na(LogLik_Difference))

# Create the plot
loglik_plot_break <-
  loglik_diff_plot_data |> 
  ggplot(aes(x = Response, y = LogLik_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in Log-Likelihood Across Comparison Types",
    x = "Response Variable",
    y = "Log-Likelihood Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(100, 5000), scales = 0.6) +
  ggbreak::scale_y_break(c(10000, 50000), scales = 0.6)

loglik_plot_break
```


The updated AIC and BIC plots provide clearer insights into the model comparisons across response variables. AIC values assess the balance between model fit and complexity, with lower values indicating better trade-offs. The AIC plot highlights stark contrasts in "Full vs Null" comparisons, especially for "Soil Quality," where extreme values suggest a poor fit of the null model relative to the full model. This trend reflects the complexity and variability inherent in "Soil Quality" data. Meanwhile, "Full vs Random" and "Random vs Fixed" comparisons exhibit closer alignment for most response variables, such as "Crop Yield" and "Pest and Disease," indicating minimal improvements in fit despite increased model complexity. For "Product Quality," the consistently low AIC values across all comparisons suggest that simpler models are adequate, potentially due to limited variability in the data.

The BIC plot echoes these observations but imposes a stronger penalty for model complexity. The "Full vs Null" comparison again dominates with substantially higher BIC values, particularly for "Soil Quality." This pattern reinforces the view that the null model fails to capture critical variability, though the full model's complexity may still be excessive. In contrast, "Full vs Random" and "Random vs Fixed" show minimal differences in BIC for most variables, further emphasizing the balance achieved by intermediate models. For simpler variables like "Product Quality," BIC values are notably small, underscoring that more parsimonious models can sufficiently describe these data.

In conclusion, these comparisons underscore the importance of tailoring model complexity to specific response variables. For highly variable metrics like "Soil Quality," intermediate models that reduce complexity while maintaining fit may be optimal. Simpler models suffice for "Product Quality" and similarly straightforward variables. These results emphasize that balancing fit and complexity is central to effective model selection, with "Full vs Random" comparisons often offering the best trade-offs across diverse response variables.



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
```



#############
# STEP 5
##########################################################################################################################################
MODEL DIAGNOSTICS ON EACH SUBSET MODEL FITTING 
##########################################################################################################################################


 
 
##########################################################################################################################################
Visualization 3: Heterogeneity (I²) Comparison
Visualize Variance Components (Tau2) and Heterogeneity (I²)
##########################################################################################################################################

```{r}
# Function to extract heterogeneity and variance components
extract_heterogeneity_metrics <- function(model_set, model_type) {
  bind_rows(
    lapply(names(model_set), function(response) {
      model <- model_set[[response]]
      if (is.null(model)) {
        return(data.frame(
          ResponseVariable = response,
          Tau2 = NA,
          I2 = NA
        ))
      }
      # Extract metrics
      tau2 <- sum(model$sigma2)
      i2 <- round((tau2 / (tau2 + mean(model$vi))) * 100, 1)
      data.frame(
        ResponseVariable = response,
        Tau2 = tau2,
        I2 = i2
      )
    })
  ) %>%
    mutate(ModelType = model_type)
}

# Extract heterogeneity metrics for each model set
heterogeneity_null <- extract_heterogeneity_metrics(null_model_results, "Null")
heterogeneity_minimal_random <- extract_heterogeneity_metrics(minimal_random_results, "Minimal Random")
heterogeneity_fixed <- extract_heterogeneity_metrics(fixed_effects_results, "Fixed Effects")
heterogeneity_random <- extract_heterogeneity_metrics(random_effects_results, "Random Effects")
heterogeneity_random_interaction <- extract_heterogeneity_metrics(random_effects_interaction_results, "Random Effects Interaction")
heterogeneity_full <- extract_heterogeneity_metrics(full_results, "Full")
heterogeneity_full_interaction <- extract_heterogeneity_metrics(full_interaction_results, "Full Interaction")

# Combine all extracted metrics into one data frame
all_heterogeneity_metrics <- bind_rows(
  heterogeneity_null,
  heterogeneity_minimal_random,
  heterogeneity_fixed,
  heterogeneity_random,
  heterogeneity_random_interaction,
  heterogeneity_full,
  heterogeneity_full_interaction
)

# View the combined metrics
all_heterogeneity_metrics |> str()

```
```{r}
# Prepare data for visualization
heterogeneity_plot_data <- all_heterogeneity_metrics %>%
  filter(!is.na(I2) & !is.na(Tau2)) %>%
  mutate(
    I2_transformed = ifelse(I2 == 0, 0.01, I2),   # Handle zero values for log scale
    Tau2_transformed = ifelse(Tau2 == 0, 0.001, Tau2) # Handle zero values for log scale
  )

# Plot for Heterogeneity (I²)
i2_plot <- ggplot(heterogeneity_plot_data, aes(x = ResponseVariable, y = I2_transformed, fill = ModelType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(
    title = "Heterogeneity (I²) Across Models",
    x = "Response Variable",
    y = "I² (Log Scale, %)",
    fill = "Model Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot for Variance Components (Tau²)
tau2_plot <- ggplot(heterogeneity_plot_data, aes(x = ResponseVariable, y = Tau2_transformed, fill = ModelType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(
    title = "Variance Components (Tau²) Across Models",
    x = "Response Variable",
    y = "Tau² (Log Scale)",
    fill = "Model Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine plots into one grid
combined_heterogeneity_plot <- i2_plot / tau2_plot +
  plot_annotation(
    title = "Heterogeneity (I²) and Variance Components (Tau²) Across Models",
    caption = "Logarithmic scales used for better visualization of smaller values."
  )

# Print the combined plot
print(combined_heterogeneity_plot)
```


### Interpretation of Heterogeneity (I²) and Variance Components (Tau²)

The heterogeneity (I²) plot highlights the proportion of variability in effect sizes across studies that is attributable to heterogeneity rather than sampling error. Low I² values signify consistent effect sizes across studies, whereas high I² values point to substantial heterogeneity, which may arise from differences between studies or unmeasured moderators. Tau², on the other hand, quantifies the variance in true effect sizes, offering an absolute measure of between-study variability.

For **Heterogeneity (I²)**, the Full Model shows elevated values for variables like biodiversity and greenhouse gas emissions, indicating substantial heterogeneity that this model captures through its complexity. However, this may reflect overfitting, especially if the model is explaining noise rather than meaningful patterns. This aligns with the fact that highly complex models, while comprehensive, often overstate heterogeneity.

The **Simplified Model**, which includes key moderators without interaction terms, demonstrates moderate I² values for variables like crop yield and product quality. This model strikes a balance between capturing essential variability and avoiding excessive complexity. Its performance suggests it is particularly suited to datasets where heterogeneity is present but not extreme.

The **Minimal Model**, with only an intercept as a random effect, consistently shows very low I² values. While this simplicity is suitable for variables with inherently low heterogeneity, such as product quality, it risks oversimplifying datasets like biodiversity or greenhouse gas emissions, where heterogeneity is critical to understanding variability.

The **Fixed Effects Model**, which excludes random effects, mirrors the Minimal Model in showing low I² values across most variables. While appropriate for datasets with minimal between-study variability, it is unsuitable for datasets with higher complexity, as it fails to account for essential heterogeneity.

The Tau² plot confirms these patterns, showing greater variance components for the Full Model in complex datasets, while the Simplified and Minimal Models exhibit much smaller Tau² values. This reflects the trade-off between comprehensiveness and parsimony.

### Implications for Meta-Analysis

The choice of model profoundly affects the interpretation of heterogeneity and the conclusions of a meta-analysis. The Full Model captures extensive variability but risks overcomplication in datasets with consistent patterns. The Simplified Model offers a more balanced approach, capturing meaningful heterogeneity without overfitting, making it the most robust choice for many variables. In contrast, the Minimal and Fixed Effects Models provide parsimonious alternatives for datasets with minimal heterogeneity but may oversimplify more complex datasets.

### Reporting and Application

When communicating results, it is crucial to pair heterogeneity metrics like I² and Tau² with model fit metrics (e.g., AIC and BIC) to clearly justify model selection. The Simplified Model, with its consistent performance across most variables, supports interpretable and actionable conclusions, while acknowledging that certain response variables may require more refined approaches. Aligning model choice with the data’s complexity ensures the meta-analysis yields robust and meaningful insights.

### Conclusion

The Simplified Model emerges as the optimal choice for balancing complexity and parsimony, particularly for response variables with moderate heterogeneity. For highly heterogeneous variables, however, refinements to the Full Model or the introduction of interaction terms may better capture variability. By aligning model complexity with data characteristics, meta-analysis results can remain both defensible and interpretable, fostering better decision-making and scientific communication.




```{r}
# Function to extract diagnostics from a single model
extract_model_diagnostics <- function(model, response_variable) {
  if (is.null(model)) {
    return(data.frame(
      ResponseVariable = response_variable,
      AIC = NA,
      BIC = NA,
      LogLikelihood = NA,
      Tau2 = NA,
      I2 = NA
    ))
  }

  # Extract diagnostics
  aic <- tryCatch(AIC(model), error = function(e) NA)
  bic <- tryCatch(BIC(model), error = function(e) NA)
  log_likelihood <- tryCatch(as.numeric(logLik(model)), error = function(e) NA)
  tau2 <- tryCatch(sum(model$sigma2), error = function(e) NA)
  i2 <- tryCatch(round((tau2 / (tau2 + mean(model$vi))) * 100, 1), error = function(e) NA)

  data.frame(
    ResponseVariable = response_variable,
    AIC = aic,
    BIC = bic,
    LogLikelihood = log_likelihood,
    Tau2 = tau2,
    I2 = i2
  )
}

# Function to extract diagnostics for all response variables in a model set
extract_diagnostics_for_model_set <- function(model_set, model_type) {
  bind_rows(
    lapply(names(model_set), function(response_variable) {
      extract_model_diagnostics(model_set[[response_variable]], response_variable)
    })
  ) %>%
    mutate(ModelType = model_type)
}

# Extract diagnostics for all model sets
null_model_diagnostics <- extract_diagnostics_for_model_set(null_model_results, "Null")
minimal_random_diagnostics <- extract_diagnostics_for_model_set(minimal_random_results, "Minimal Random")
fixed_effects_diagnostics <- extract_diagnostics_for_model_set(fixed_effects_results, "Fixed Effects")
random_effects_diagnostics <- extract_diagnostics_for_model_set(random_effects_results, "Random Effects")
random_effects_interaction_diagnostics <- extract_diagnostics_for_model_set(random_effects_interaction_results, "Random Effects Interaction")
full_diagnostics <- extract_diagnostics_for_model_set(full_results, "Full")
full_interaction_diagnostics <- extract_diagnostics_for_model_set(full_interaction_results, "Full Interaction")

# Combine all diagnostics into a single data frame
all_meta_analysis_model_diagnostics <- bind_rows(
  null_model_diagnostics,
  minimal_random_diagnostics,
  fixed_effects_diagnostics,
  random_effects_diagnostics,
  random_effects_interaction_diagnostics,
  full_diagnostics,
  full_interaction_diagnostics
)

# Save the extracted diagnostics for future use
saveRDS(all_meta_analysis_model_diagnostics, file.path(output_dir, "all_meta_analysis_model_diagnostics.rds"))

all_meta_analysis_model_diagnostics
```

```{r}
# Summarize model diagnostics into a table
summary_table_meta_data_all_models_data <- all_meta_analysis_model_diagnostics %>%
  select(ResponseVariable, ModelType, AIC, BIC, LogLikelihood, Tau2, I2) %>%
  group_by(ResponseVariable, ModelType) %>%
  summarise(
    AIC = round(mean(AIC, na.rm = TRUE), 2),
    BIC = round(mean(BIC, na.rm = TRUE), 2),
    LogLikelihood = round(mean(LogLikelihood, na.rm = TRUE), 2),
    Tau2 = round(mean(Tau2, na.rm = TRUE), 2),
    I2 = round(mean(I2, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Display the summarized table
print(summary_table_meta_data_all_models_data)
```

```{r}
# Create a formatted summary table
diagnostics_table_meta_data_all_models <- summary_table_meta_data_all_models_data %>%
  gt() %>%
  tab_header(
    title = "Model Diagnostics Summary",
    subtitle = "Comparison of Key Metrics Across Models by Response Variable"
  ) %>%
  cols_label(
    ResponseVariable = "Response Variable",
    ModelType = "Model Type",
    AIC = "AIC",
    BIC = "BIC",
    LogLikelihood = "Log-Likelihood",
    Tau2 = "Tau²",
    I2 = "I² (%)"
  ) %>%
  fmt_number(
    columns = c(AIC, BIC, LogLikelihood, Tau2, I2),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = "small",
    column_labels.font.size = "medium",
    heading.align = "center"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

# Display the table
print(diagnostics_table_meta_data_all_models)
```



##########################################################################
Model Diagnostics: Heterogeneity also called Residual Heterogeneity Partitioning
##########################################################################


```{r}
# Check the names of response variables in each model set
response_variables <- unique(unlist(lapply(list(
  null = null_model_results,
  minimal = minimal_random_results,
  fixed = fixed_effects_results,
  simplified = random_effects_results,
  full = full_results
), names)))

# Function to compute heterogeneity partitioning
compute_heterogeneity_partitioning <- function(model, response_variable, model_type) {
  if (is.null(model)) {
    return(data.frame(
      ResponseVariable = response_variable,
      ModelType = model_type,
      TotalHeterogeneity = NA,
      ExplainedHeterogeneity = NA,
      ResidualHeterogeneity = NA,
      ExplainedProportion = NA,
      ResidualProportion = NA
    ))
  }
  QE <- model$QE
  QM <- model$QM
  if (!is.null(QE) && !is.null(QM)) {
    residual_heterogeneity <- QE - QM
    explained_proportion <- ifelse(QE > 0, (QM / QE) * 100, NA)
    residual_proportion <- ifelse(QE > 0, (residual_heterogeneity / QE) * 100, NA)
    return(data.frame(
      ResponseVariable = response_variable,
      ModelType = model_type,
      TotalHeterogeneity = QE,
      ExplainedHeterogeneity = QM,
      ResidualHeterogeneity = max(residual_heterogeneity, 0),
      ExplainedProportion = explained_proportion,
      ResidualProportion = residual_proportion
    ))
  }
  return(data.frame(
    ResponseVariable = response_variable,
    ModelType = model_type,
    TotalHeterogeneity = NA,
    ExplainedHeterogeneity = NA,
    ResidualHeterogeneity = NA,
    ExplainedProportion = NA,
    ResidualProportion = NA
  ))
}

# Extract heterogeneity for all models
heterogeneity_results <- list(
  null = null_model_results,
  minimal = minimal_random_results,
  fixed = fixed_effects_results,
  simplified = random_effects_results,
  full = full_results
) %>%
  purrr::imap_dfr(~ {
    purrr::map_dfr(response_variables, function(response_variable) {
      if (!is.null(.x[[response_variable]])) {
        compute_heterogeneity_partitioning(.x[[response_variable]], response_variable, .y)
      } else {
        compute_heterogeneity_partitioning(NULL, response_variable, .y)
      }
    })
  })

# View final heterogeneity results
print(heterogeneity_results)
heterogeneity_results |> str()
```

```{r}
# Clean the data
cleaned_heterogeneity_results <- heterogeneity_results %>%
  mutate(
    ResidualProportion = ifelse(ResidualProportion < 0, 0, ResidualProportion),
    ExplainedProportion = ifelse(ExplainedProportion > 100, 100, ExplainedProportion)
  )

# Reshape data for heterogeneity values (Total, Explained, Residual)
heterogeneity_long <- cleaned_heterogeneity_results %>%
  pivot_longer(
    cols = c(TotalHeterogeneity, ExplainedHeterogeneity, ResidualHeterogeneity),
    names_to = "HeterogeneityType",
    values_to = "HeterogeneityValue"
  )

# Plot 1: Heterogeneity values (bar plot)
heterogeneity_plot <- ggplot(heterogeneity_long, aes(
  x = ResponseVariable, 
  y = HeterogeneityValue, 
  fill = HeterogeneityType
)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_y_log10(labels = scales::comma) + # Log scale for better visualization
  labs(
    title = "Partitioned Heterogeneity Across Models",
    x = "Response Variable",
    y = "Heterogeneity Value (log scale)",
    fill = "Heterogeneity Type"
  ) +
  facet_wrap(~ModelType, ncol = 2) + # Separate by model type
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Plot 2: Proportion values (line plot)
proportion_plot <- cleaned_heterogeneity_results %>%
  select(ResponseVariable, ModelType, ExplainedProportion, ResidualProportion) %>%
  pivot_longer(
    cols = c(ExplainedProportion, ResidualProportion),
    names_to = "ProportionType",
    values_to = "ProportionValue"
  ) %>%
  ggplot(aes(
    x = ResponseVariable, 
    y = ProportionValue, 
    group = ProportionType, 
    color = ProportionType
  )) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5), alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Explained vs. Residual Proportion",
    x = "Response Variable",
    y = "Proportion (%)",
    color = "Proportion Type"
  ) +
  facet_wrap(~ModelType, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Combine the plots into one
combined_plot <- heterogeneity_plot / proportion_plot +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Heterogeneity and Proportion Metrics Across Models",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

# Display the combined plot
print(combined_plot)
```


This plot provides a detailed view of the relationship between explained and residual heterogeneity proportions across various meta-analysis models, alongside their performance in partitioning total heterogeneity. Explained heterogeneity measures the variance in effect sizes attributable to predictors or moderators in the model, while residual heterogeneity captures the unexplained variance. These metrics are critical for evaluating how well different models account for patterns in the data while minimizing unaccounted variation.

### Insights from the Plot:
1. **Partitioned Heterogeneity (Top Panel):**  
   The top panel displays the total, explained, and residual heterogeneity across response variables for each model type. The **null model**, as expected, exhibits high residual heterogeneity and minimal explained heterogeneity due to its lack of fixed or random effects. The **minimal model** improves on this by introducing random effects, capturing more explained heterogeneity while reducing residual variability. Models with greater complexity, such as the **fixed**, **simplified**, and **full models**, show varying proportions of explained heterogeneity. For instance, the **full model** often captures substantial explained heterogeneity but risks overfitting, evident in certain response variables with high unexplained residuals.

2. **Explained vs. Residual Proportions (Bottom Panel):**  
   The bottom panel illustrates the trade-off between explained and residual heterogeneity proportions across models. For most response variables, the **fixed model** achieves a strong reduction in residual heterogeneity while increasing the explained proportion, suggesting its effectiveness in leveraging fixed effects to capture variability. The **simplified model** demonstrates balanced performance, striking a middle ground between explained and residual proportions. The **full model**, while often capturing the highest explained heterogeneity, shows inconsistencies, indicating potential challenges with overfitting or the inclusion of unnecessary moderators.

3. **Response Variable-Specific Performance:**  
   Response variables like **soil quality** and **pest and disease** exhibit large unexplained residual heterogeneity in simpler models (e.g., null, minimal), underscoring the need for more complex modeling frameworks. Conversely, response variables like **product quality** or **biodiversity** show relatively stable proportions across most models, suggesting lower inherent heterogeneity.

### Implications for Meta-Analysis:
This plot highlights the trade-offs between model complexity and explanatory power. While simpler models like the null or minimal models are easy to interpret, they fail to capture significant heterogeneity in complex datasets. More complex models, such as the full or simplified models, excel at explaining heterogeneity but may introduce overfitting, reducing their generalizability. The fixed model appears to balance these factors effectively, particularly for response variables with moderate heterogeneity.

### Limitations and Recommendations:
One limitation of this analysis is that it focuses on heterogeneity metrics without linking them explicitly to effect size accuracy or hypothesis testing. Future work should assess how heterogeneity partitioning affects the broader objectives of the meta-analysis, such as robustness in effect size estimation or predictive performance. The choice of model should align with the study's goals, balancing parsimony and interpretability with the ability to explain key patterns in the data.

### Conclusion:
The fixed and simplified models emerge as strong candidates for meta-analytic applications, offering a balance between explained heterogeneity and manageable residuals. However, for response variables with high complexity (e.g., soil quality), full models may provide additional insights at the cost of interpretability. Careful justification of model selection based on these metrics is essential for robust and defensible conclusions in meta-analysis.















#############
# STEP 6
##########################################################################################################################################
KEY INFLUENCE DIAGNOSTICS ON EACH SUBSET - SIMPLIFIED MODEL FITTING 
##########################################################################################################################################




##########################################################################################################################################
MODERATOR ANALYSIS AND INTERPRETATION 
##########################################################################################################################################



##########################################################################################################################################
SAVING DATASETS AND MODEL OBJECTS
##########################################################################################################################################




#############
# STEP 7
##########################################################################################################################################
PUBLICATION-READY PLOTS AND TABLES OF EFFECT SIZE IMPACTS ON RESPONSE VARIABLES OF TEMPERATE SAF FOR EACH SUBSET MODEL FITTING 
##########################################################################################################################################



Forest Plot: Visualizes effect sizes and confidence intervals for response variables.
Ridge Plot: Shows the distribution of effect sizes for each response variable.
Variance Plot: Compares variance components (Tau²) and heterogeneity (I²).
Combined Plot: Combines the forest and ridge plots into a single figure for publication.








```{r}
# Function to extract AIC, BIC, LogLik from each model in the model_summaries object
extract_model_metrics <- function(model, response, model_type) {
  # Check if model is NULL
  if (is.null(model)) {
    return(data.frame(
      Response = response,
      Model_Type = model_type,
      AIC = NA,
      BIC = NA,
      LogLik = NA,
      Coefficients = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Check if model has the expected structure (a typical model object)
  if (!inherits(model, "rma")) {
    return(data.frame(
      Response = response,
      Model_Type = model_type,
      AIC = NA,
      BIC = NA,
      LogLik = NA,
      Coefficients = "Not a model object",
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract AIC, BIC, LogLik (assuming these are in 'fit.stats' data frame)
  aic <- tryCatch(if (!is.null(model$fit.stats)) model$fit.stats[1, "REML"] else NA, error = function(e) NA)
  bic <- tryCatch(if (!is.null(model$fit.stats)) model$fit.stats[2, "REML"] else NA, error = function(e) NA)
  loglik <- tryCatch(if (!is.null(model$fit.stats)) model$fit.stats[3, "REML"] else NA, error = function(e) NA)
  
  # Extract coefficients, check if available
  coefficients <- if (!is.null(model$b) && is.numeric(model$b)) {
    paste0(names(model$b), "=", round(model$b, 4), collapse = "; ")
  } else {
    NA
  }
  
  # Return the extracted metrics
  return(data.frame(
    Response = response,
    Model_Type = model_type,
    AIC = aic,
    BIC = bic,
    LogLik = loglik,
    Coefficients = coefficients,
    stringsAsFactors = FALSE
  ))
}

# Initialize an empty data frame to store the results
results_summary <- data.frame()

# Loop through each response variable and model to extract metrics
for (response in names(model_summaries)) {
  for (model_type in names(model_summaries[[response]])) {
    model_list <- model_summaries[[response]][[model_type]]
    
    # Loop over each model in the model_list
    for (i in seq_along(model_list)) {
      model <- model_list[[i]]
      
      # Extract metrics for each model, check for valid model structure
      if (inherits(model, "rma")) {
        metrics <- extract_model_metrics(model, response, paste0(model_type, "_", i))
        results_summary <- rbind(results_summary, metrics)
      } else {
        warning(paste("Model", response, model_type, i, "is not of class 'rma'. Skipping."))
      }
    }
  }
}

# Display the results summary
results_summary
```

































































################################################################################################################################################################
FIXED EFFECTS AND MODERATORS - MODEL DIAGNOSTICS AND EVALUATIONS
################################################################################################################################################################

```{r}
#######################################################################################################
# Organize Nested Meta-Analysis Results into a Condensed Data Frame
#######################################################################################################

extract_model_metrics <- function(model, response, model_type) {
  if (is.null(model)) {
    return(data.frame(
      Response = response,
      Model_Type = model_type,
      k = NA,
      tau2 = NA,
      QE = NA,
      QEp = NA,
      QM = NA,
      QMp = NA,
      AIC = NA,
      BIC = NA,
      LogLik = NA,
      Coefficients = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Handle cases where 'b' is NULL or non-numeric
  coefficients <- if (!is.null(model$b) && is.numeric(model$b)) {
    paste0(names(model$b), "=", round(model$b, 4), collapse = "; ")
  } else {
    NA
  }
  # Remove the "=" sign from the coefficients
  if (!is.na(coefficients)) {
    coefficients <- gsub("=", "", coefficients)
  }
  
  
  # Extract AIC, BIC, LogLik
  aic <- tryCatch(if (!is.null(model$fit.stats)) model$fit.stats[1, "REML"] else NA, error = function(e) NA)
  bic <- tryCatch(if (!is.null(model$fit.stats)) model$fit.stats[2, "REML"] else NA, error = function(e) NA)
  loglik <- tryCatch(if (!is.null(model$fit.stats)) model$fit.stats[3, "REML"] else NA, error = function(e) NA)
  
  # Extract metrics
  data.frame(
    Response = response,
    Model_Type = model_type,
    k = if (!is.null(model$k)) model$k else NA,
    tau2 = if (!is.null(model$tau2)) model$tau2 else NA,
    QE = if (!is.null(model$QE)) model$QE else NA,
    QEp = if (!is.null(model$QEp)) model$QEp else NA,
    QM = if (!is.null(model$QM)) model$QM else NA,
    QMp = if (!is.null(model$QMp)) model$QMp else NA,
    AIC = aic,
    BIC = bic,
    LogLik = loglik,
    Coefficients = coefficients,
    stringsAsFactors = FALSE
  )
}

# Initialize an empty data frame
results_summary <- data.frame()

# Loop through each response variable and model
for (response in names(model_results)) {
  for (model_type in names(model_results[[response]])) {
    model <- model_results[[response]][[model_type]]
    
    # Check if sub-models exist (e.g., [[1]], [[2]])
    if (is.list(model) && all(sapply(model, function(x) inherits(x, "rma")))) {
      sub_model_metrics <- do.call(rbind, lapply(seq_along(model), function(i) {
        extract_model_metrics(model[[i]], response, paste0(model_type, "_", i))
      }))
      # Aggregate sub-model metrics (e.g., calculate mean tau2, QM, etc.)
      aggregated_metrics <- data.frame(
        Response = response,
        Model_Type = model_type,
        k = mean(sub_model_metrics$k, na.rm = TRUE),
        tau2 = mean(sub_model_metrics$tau2, na.rm = TRUE),
        QE = mean(sub_model_metrics$QE, na.rm = TRUE),
        QEp = mean(sub_model_metrics$QEp, na.rm = TRUE),
        QM = mean(sub_model_metrics$QM, na.rm = TRUE),
        QMp = mean(sub_model_metrics$QMp, na.rm = TRUE),
        AIC = mean(sub_model_metrics$AIC, na.rm = TRUE),
        BIC = mean(sub_model_metrics$BIC, na.rm = TRUE),
        LogLik = mean(sub_model_metrics$LogLik, na.rm = TRUE),
        Coefficients = paste(unique(sub_model_metrics$Coefficients), collapse = " | "),
        stringsAsFactors = FALSE
      )
      results_summary <- rbind(results_summary, aggregated_metrics)
    } else {
      # Extract metrics for single models
      results_summary <- rbind(
        results_summary,
        extract_model_metrics(model, response, model_type)
      )
    }
  }
}

# Filter for models with complete outputs (all required metrics available)
results_summary <- results_summary[complete.cases(results_summary[, c("k", "tau2", "QE", "QEp")]), ]

# Diagnostic Outputs: Flag incomplete or missing results
results_summary$Diagnostic_Flag <- ifelse(is.na(results_summary$k) | is.na(results_summary$tau2) | 
                                            is.na(results_summary$QE) | is.na(results_summary$QEp), "Incomplete", "Complete")

results_summary <- results_summary |>  
  relocate(Model_Type, Response, AIC, BIC, LogLik, k, QE, QEp, QM, QMp)

# View the condensed dataset
print(results_summary)
# results_summary |> glimpse()

# Save the summary
# summary_file <- file.path(output_dir, "meta_analysis_results_summary_with_AIC_BIC_LogLik.csv")
# write.csv(results_summary, summary_file, row.names = FALSE)
# cat("\nSummary saved successfully at:", summary_file, "\n")

results_summary |> str()
results_summary
```

```{r}
# Split the 'Coefficients' column into separate rows by splitting at ' | ' 
coefficients_df <- results_summary %>%
  mutate(Coefficients = strsplit(as.character(Coefficients), " \\| ")) %>%  # Split the coefficients by " | "
  unnest(Coefficients) %>%  # Convert each coefficient into a separate row
  separate(Coefficients, into = paste0("Coef_", 1:10), sep = ";", remove = FALSE) %>%  # Split by ";" to extract individual coefficients
  gather(key = "Coef_#", value = "Coefficient", starts_with("Coef")) %>%  # Reshape the data into long format
  filter(!is.na(Coefficient))  # Remove rows with NA coefficients

# View the first few rows of the detangled coefficients
coefficients_df |> str()
```


```{r}
# model_results$`Crop yield`$B_minimal_random_incremental
# model_results$`Crop yield`$B_minimal_random_incremental |> str()

# model_results$`Crop yield`$D_incremental_random_incremental
# model_results$`Crop yield`$E_intercept_fixed_random_incremental
```
```{r}
results_summary |> str()
```


Models with diagnostics (AIC, BIC, LogLikelihood)

```{r}
# Pivot the data to long format for ggplot
results_summary_long <- results_summary %>%
  pivot_longer(
    cols = c("AIC", "BIC", "LogLik"),
    names_to = "Metric",
    values_to = "Value"
  )

# Create grouped bar chart for AIC, BIC, and LogLik
aic_bic_loglik_plot <- ggplot(results_summary_long, aes(x = Model_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Response, scales = "free_y", nrow = 2) +  # Free y-axis scaling for each response
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Model Fit Statistics by Response Variable and Model Type",
    x = "Model Type",
    y = "Metric Value",
    fill = "Metric"
  )

# Display the plot
print(aic_bic_loglik_plot)

```


The model comparison evaluates the impact of increasing complexity on explaining variability in the dataset, using models ranging from the simplest (`A_null`) to the most complex (`G_full_interaction`). AIC (Akaike Information Criterion) and BIC (Bayesian Information Criterion) are used to balance model fit against complexity, with lower values indicating better performance. Each model adds features incrementally, such as random effects, specified moderators, and interaction terms, to improve explanatory power.

The null model (`A_null`) serves as a baseline and consistently performs poorly. For "Biodiversity," its AIC of 24,170.40 and BIC of 24,173.59 show limited explanatory power. Introducing random effects in the minimal random model (`B_minimal_random`) significantly improves fit, with AIC and BIC dropping to 14,775.98 and 14,782.36, respectively, highlighting the importance of capturing between-experiment variability. Adding specified moderators in the fixed effects model (`C_fixed_effects`) reduces these values further, to 14,129.22 (AIC) and 14,156.73 (BIC), by accounting for known moderators.

The random effects model (`D_random_effects`) slightly outperforms the fixed effects model, achieving an AIC of 14,125.27 and a BIC of 14,155.84, as it captures both within- and between-experiment variability. For "Greenhouse Gas Emission," the fixed effects model reduces AIC from 5,437.01 (`A_null`) to 2,280.87, while the random effects model improves it further to 2,282.87. Incorporating interactions among moderators in the random effects interaction model (`E_random_effects_interaction`) provides modest additional improvement for response variables like "Greenhouse Gas Emission" (AIC = 2,288.14) but shows diminishing returns for simpler datasets like "Water Quality."

The full model (`F_full`), which includes multiple random effects and nested dependencies, balances complexity and fit effectively, particularly for heterogeneous datasets like "Soil Quality." In "Crop Yield," its AIC of 2,342.19 is the lowest, showing its ability to capture the data's complexity. The full interaction model (`G_full_interaction`), while achieving slightly lower AIC and BIC for some variables, often introduces unnecessary complexity and interpretational challenges.

**Overall Conclusion:** The most appropriate model depends on the dataset's complexity. For response variables with high heterogeneity and interaction effects, such as "Crop Yield" and "Greenhouse Gas Emission," the full model (`F_full`) offers the best trade-off between fit and interpretability. For simpler variables like "Water Quality," the random effects model (`D_random_effects`) suffices, as additional complexity does not significantly improve fit. This recommendation ensures robust yet interpretable insights across diverse datasets, aligning model choice with research objectives and data characteristics.




The visualizations and tables presented provide a clear comparative analysis of model performance across multiple response variables in the meta-analysis. The "D_random_effects" model emerges as the most effective overall, consistently demonstrating the lowest average AIC (9,253.15) and BIC (9,277.79) values across response variables, coupled with a superior log-likelihood value (-4,618.15). This highlights its ability to account for both fixed and random effects efficiently, balancing model complexity and explanatory power. 

The "F_full" model performs similarly, with slightly higher AIC (9,255.24) and BIC (9,284.74), indicating that its additional complexity, including nested random effects, does not substantially improve fit for most response variables. The "E_random_effects_interaction" model introduces interactions between moderators, offering better fits in some scenarios but generally displaying diminishing returns compared to simpler models like "D_random_effects."

The "A_null" model, with the highest average AIC (134,882.43) and BIC (134,885.33), fails to capture variability adequately, serving as a stark contrast to more complex models. The "C_fixed_effects" model, with average AIC (9,753.59) and BIC (9,775.39), outperforms the null but is less robust than models incorporating random effects. The "B_minimal_random" model provides a better fit than the fixed effects model, but its higher AIC (9,879.92) and BIC (9,885.72) indicate its limitations in explaining heterogeneity effectively.

For specific response variables, the variability in best-fit models underscores the importance of tailoring the model structure to data characteristics. The "D_random_effects" model excels for "Biodiversity" and "Soil Quality," while "E_random_effects_interaction" occasionally performs better in datasets with more complex interactions, such as "Greenhouse Gas Emission." However, no single model consistently dominates across all variables, reflecting the diversity in dataset structures and underlying processes.

In conclusion, the "D_random_effects" model is the most appropriate overall due to its strong performance across diverse response variables. However, the "F_full" model may be suitable for datasets requiring nested random effects or greater complexity. This analysis emphasizes the need to align model complexity with dataset characteristics to ensure robust, interpretable results in meta-analytical research.


















#############
# STEP 4
##########################################################################################################################################
MODEL COMPARISONS, EVALUATION AND DIAGNOSTICS
##########################################################################################################################################




```{r}
##########################################################################
# FITTING MODELS WITH ML FOR DIRECT COMPARISONS
##########################################################################

# Define `include_interaction` variable to toggle interaction models
include_interaction <- TRUE  # Set to FALSE if interaction terms are not required

##########################################################################
# Set up parallel processing and start time tracking
##########################################################################
plan(multisession, workers = parallel::detectCores() - 1)
start.time <- Sys.time()

##########################################################################
# Function to fit models with hierarchical complexity
##########################################################################
fit_model <- function(data_subset, response_variable, v_matrix, moderators, random_effects = NULL, intercept = TRUE, include_interaction = FALSE) {
  cat("\nFitting model for response variable:", response_variable, "...\n")
  
  # Build moderator formula based on interaction and intercept settings
  moderator_formula <- if (is.null(moderators) || length(moderators) == 0) {
    ~ 1  # Intercept-only model
  } else if (include_interaction) {
    if (intercept) {
      as.formula(paste("yi ~", paste(moderators, collapse = " * ")))
    } else {
      as.formula(paste("yi ~", paste(moderators, collapse = " * "), "- 1"))
    }
  } else {
    if (intercept) {
      as.formula(paste("yi ~", paste(moderators, collapse = " + ")))
    } else {
      as.formula(paste("yi ~", paste(moderators, collapse = " + "), "- 1"))
    }
  }
  
  # Fit the model
  tryCatch({
    rma.mv(
      yi = data_subset$yi,
      V = v_matrix,
      mods = moderator_formula,
      random = random_effects,
      data = data_subset,
      method = "ML",
      control = list(
        optimizer = "optim",
        optim.method = "BFGS",
        iter.max = 1000,
        rel.tol = 1e-8
      )
    )
  }, error = function(e) {
    cat("Error in model fitting for", response_variable, ":", e$message, "\n")
    return(NULL)
  })
}

##########################################################################
# Fit Models for Each Response Variable
##########################################################################
model_results_ml <- lapply(names(v_matrices), function(response) {
  cat("\nProcessing response variable:", response, "\n")
  
  data_subset <- meta_data[meta_data$response_variable == response, ]
  v_matrix <- v_matrices[[response]]
  moderators <- c("tree_type", "crop_type", "age_system", "season", "soil_texture")
  
  list(
    A_null = fit_model(data_subset, response, v_matrix, NULL, intercept = TRUE),
    B_minimal_random = fit_model(data_subset, response, v_matrix, NULL, random_effects = ~ 1 | exp_id, intercept = TRUE),
    C_fixed_effects = fit_model(data_subset, response, v_matrix, moderators, intercept = TRUE),
    D_random_effects = fit_model(data_subset, response, v_matrix, moderators, random_effects = ~ 1 | exp_id, intercept = TRUE),
    E_random_effects_interaction = fit_model(data_subset, response, v_matrix, moderators, random_effects = ~ 1 | exp_id, intercept = TRUE, include_interaction = TRUE),
    F_full = fit_model(data_subset, response, v_matrix, moderators, random_effects = list(~ 1 | id_article/response_variable, ~ 1 | exp_id), intercept = TRUE),
    G_full_interaction = fit_model(data_subset, response, v_matrix, moderators, random_effects = list(~ 1 | id_article/response_variable, ~ 1 | exp_id), intercept = TRUE, include_interaction = TRUE)
  )
})

names(model_results_ml) <- names(v_matrices)

##########################################################################
# Save Fitted Models
##########################################################################
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

# Save all models in one file
saveRDS(model_results_ml, file = file.path(output_dir, "fitted_models_ml_all.rds"))

# Save each model type in separate files
model_types <- c("A_null", "B_minimal_random", "C_fixed_effects", "D_random_effects", "E_random_effects_interaction", "F_full", "G_full_interaction")

for (type in model_types) {
  saveRDS(lapply(model_results_ml, `[[`, type), file = file.path(output_dir, paste0("fitted_models_", type, "_ml.rds")))
}

cat("\nAll models fitted with ML have been saved successfully.\n")

##########################################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("\nTotal time taken:", time.taken, "\n")

# Last go (18/01-2025)
# Total time taken: 28.87752  secs
```



```{r}
##########################################################################
# Combine All Comparison Results into a DataFrame
##########################################################################

# Load the saved models if needed
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
model_results_ml <- readRDS(file.path(output_dir, "fitted_models_ml_all.rds"))

# Initialize a list to store comparison results
comparison_results <- list()

# Iterate over response variables to perform model comparisons
for (response in names(model_results_ml)) {
  cat("\nComparing models for response variable:", response, "\n")
  
  # Extract the models for the current response variable
  models <- model_results_ml[[response]]
  
  # Perform comparisons for multiple models using ANOVA
  comparison <- tryCatch({
    list(
      Full_vs_Random = anova(models$G_full_interaction, models$D_random_effects),
      Random_vs_Fixed = anova(models$D_random_effects, models$C_fixed_effects),
      Full_vs_Null = anova(models$G_full_interaction, models$A_null)
    )
  }, error = function(e) {
    cat("Error in comparison for", response, ":", e$message, "\n")
    return(NULL)
  })
  
  # Store the comparison results
  comparison_results[[response]] <- comparison
}

# Combine all comparisons into a single data frame for each model pair
comparison_df <- do.call(rbind, lapply(names(comparison_results), function(response) {
  result <- comparison_results[[response]]
  if (!is.null(result)) {
    do.call(rbind, lapply(names(result), function(comp) {
      as.data.frame(result[[comp]]) |>
        dplyr::mutate(
          Response = response,
          Comparison = comp
        )
    }))
  } else {
    NULL  # Exclude responses with no comparison results
  }
}))

# View the combined comparison results
# print(comparison_df)



##########################################################################
# Modifying the Resulting DataFrame
##########################################################################

# Standardize model names and add Model_Type column
comparison_df <- comparison_df |>
  dplyr::mutate(
    Model_Type = case_when(
      grepl("Full_vs_Random", Comparison) ~ "Full",
      grepl("Random_vs_Fixed", Comparison) ~ "Random",
      grepl("Full_vs_Null", Comparison) ~ "Null",
      TRUE ~ "Unknown"
    )
  ) |>
  dplyr::relocate(Model_Type, .before = everything())  # Move Model_Type to the first column


# comparison_df |> str()
comparison_df |> str()
```

```{r}
##########################################################################
# Transform the DataFrame for Visualization
##########################################################################
##########################################################################
# Step 1: Update the Model_Type Based on Comparison
##########################################################################
comparison_df_clean <- comparison_df %>%
  dplyr::mutate(
    Actual_Model_Type = dplyr::case_when(
      Comparison == "Full_vs_Random" & row_number() %% 2 == 1 ~ "Full",
      Comparison == "Full_vs_Random" & row_number() %% 2 == 0 ~ "Random",
      Comparison == "Random_vs_Fixed" & row_number() %% 2 == 1 ~ "Random",
      Comparison == "Random_vs_Fixed" & row_number() %% 2 == 0 ~ "Fixed",
      Comparison == "Full_vs_Null" & row_number() %% 2 == 1 ~ "Full",
      Comparison == "Full_vs_Null" & row_number() %% 2 == 0 ~ "Null",
      TRUE ~ Model_Type
    )
  )  |> relocate(Comparison, Actual_Model_Type, Response, AIC, BIC, AICc, logLik, LRT, pval)

##########################################################################
# Step 2: Populate Metric Columns
##########################################################################
# Create a function to populate metrics based on Actual_Model_Type
populate_metrics <- function(data, metric_cols) {
  for (metric in metric_cols) {
    data <- data %>%
      dplyr::mutate(
        !!paste0(metric, "_Full") := ifelse(Actual_Model_Type == "Full", .data[[metric]], NA),
        !!paste0(metric, "_Random") := ifelse(Actual_Model_Type == "Random", .data[[metric]], NA),
        !!paste0(metric, "_Fixed") := ifelse(Actual_Model_Type == "Fixed", .data[[metric]], NA),
        !!paste0(metric, "_Null") := ifelse(Actual_Model_Type == "Null", .data[[metric]], NA)
      )
  }
  return(data)
}

# Apply the function to populate metrics
metrics <- c("AIC", "BIC", "logLik", "pval")
aligned_data <- populate_metrics(comparison_df_clean, metrics)

##########################################################################
# Step 3: Aggregate Metrics by Response and Comparison
##########################################################################
aligned_data_agg <- aligned_data %>%
  group_by(Response, Comparison) %>%
  summarise(across(starts_with("AIC"), ~ max(., na.rm = TRUE)),
            across(starts_with("BIC"), ~ max(., na.rm = TRUE)),
            across(starts_with("logLik"), ~ max(., na.rm = TRUE)),
            across(starts_with("pval"), ~ max(., na.rm = TRUE)),
            .groups = "drop")

##########################################################################
# Step 4: Calculate Differences in Metrics
##########################################################################
aligned_data_pronto <- aligned_data_agg %>%
  mutate(
    # Full vs Random
    AIC_Diff_Full_vs_Random = AIC_Full - AIC_Random,
    BIC_Diff_Full_vs_Random = BIC_Full - BIC_Random,
    logLik_Diff_Full_vs_Random = logLik_Full - logLik_Random,
    pval_Diff_Full_vs_Random = pval_Full - pval_Random,
    
    # Random vs Fixed
    AIC_Diff_Random_vs_Fixed = AIC_Random - AIC_Fixed,
    BIC_Diff_Random_vs_Fixed = BIC_Random - BIC_Fixed,
    logLik_Diff_Random_vs_Fixed = logLik_Random - logLik_Fixed,
    pval_Diff_Random_vs_Fixed = pval_Random - pval_Fixed,
    
    # Full vs Null
    AIC_Diff_Full_vs_Null = AIC_Full - AIC_Null,
    BIC_Diff_Full_vs_Null = BIC_Full - BIC_Null,
    logLik_Diff_Full_vs_Null = logLik_Full - logLik_Null,
    pval_Diff_Full_vs_Null = pval_Full - pval_Null
  )

##########################################################################
# Step 5: Handle Missing Values (Inf, NaN)
##########################################################################
aligned_data_pronto <- aligned_data_pronto %>%
  mutate(across(starts_with("AIC_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)),
         across(starts_with("BIC_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)),
         across(starts_with("logLik_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)),
         across(starts_with("pval_Diff"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

##########################################################################
# Step 6: View Results
##########################################################################
aligned_data_pronto %>%
  dplyr::select(Response, starts_with("AIC_Diff"), starts_with("BIC_Diff")) %>%
  print()

aligned_data_pronto |> str()
```

```{r}
# Prepare data for plotting
absolute_metrics_plot_data <- aligned_data %>%
  dplyr::select(Response, Actual_Model_Type, AIC, BIC) %>%
  tidyr::pivot_longer(
    cols = c(AIC, BIC),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::filter(!is.na(Value))

# Create the plot
absolute_metrics_plot <- ggplot(absolute_metrics_plot_data, aes(x = Response, y = Value, fill = Actual_Model_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Absolute AIC and BIC Values by Model Type Across Response Variables",
    x = "Response Variable",
    y = "Metric Value",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top"
  )  +
  ggbreak::scale_y_break(c(5000, 20000), scales = 0.8) +
  ggbreak::scale_y_break(c(50000, 200000), scales = 0.8)

# View the plot
absolute_metrics_plot
```


```{r}
##########################################################################
# Visualization
##########################################################################

# Visualization 1: AIC Differences with ggbreak
aic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, AIC_Diff_Full_vs_Random, AIC_Diff_Random_vs_Fixed, AIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("AIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "AIC_Difference"
  ) %>%
  dplyr::filter(!is.na(AIC_Difference))

aic_plot_break <- ggplot(aic_diff_plot_data, aes(x = Response, y = AIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in AIC Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "AIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

# Visualization 2: BIC Differences with ggbreak
bic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, BIC_Diff_Full_vs_Random, BIC_Diff_Random_vs_Fixed, BIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("BIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "BIC_Difference"
  ) %>%
  dplyr::filter(!is.na(BIC_Difference))

bic_plot_break <- ggplot(bic_diff_plot_data, aes(x = Response, y = BIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in BIC Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "BIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

# Visualization 3: Log-Likelihood Differences with ggbreak
loglik_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, logLik_Diff_Full_vs_Random, logLik_Diff_Random_vs_Fixed, logLik_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("logLik_Diff"),
    names_to = "Comparison_Type",
    values_to = "LogLik_Difference"
  ) %>%
  dplyr::filter(!is.na(LogLik_Difference))

loglik_plot_break <- ggplot(loglik_diff_plot_data, aes(x = Response, y = LogLik_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in Log-Likelihood Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "Log-Likelihood Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(100, 5000), scales = 0.6) +
  ggbreak::scale_y_break(c(10000, 50000), scales = 0.6)

# Combine Plots Using patchwork
combined_plot <- aic_plot_break / bic_plot_break / loglik_plot_break +
  plot_layout(ncol = 1) +
  plot_annotation(title = "Comparison Metrics Across Models")

# Display Combined Plot
print(combined_plot)
```


```{r}
# Visualization 1: Differences in AIC Across Comparison Types
# Prepare data for plotting
aic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, AIC_Diff_Full_vs_Random, AIC_Diff_Random_vs_Fixed, AIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("AIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "AIC_Difference"
  ) %>%
  dplyr::filter(!is.na(AIC_Difference))

# Create the plot
# AIC Differences with ggbreak
aic_plot_break <- aic_diff_plot_data |> 
  ggplot(aes(x = Response, y = AIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous() +
  labs(
    title = "Differences in AIC Across Comparison Types (Broken Scale)",
    x = "Response Variable",
    y = "AIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

# Plot
aic_plot_break
```
Visualization 2: Differences in BIC Across Comparison Types
```{r}
# Prepare data for plotting
bic_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, BIC_Diff_Full_vs_Random, BIC_Diff_Random_vs_Fixed, BIC_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("BIC_Diff"),
    names_to = "Comparison_Type",
    values_to = "BIC_Difference"
  ) %>%
  dplyr::filter(!is.na(BIC_Difference))

# Create the plot
bic_plot_break <-
  bic_diff_plot_data |> 
  ggplot(aes(x = Response, y = BIC_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in BIC Across Comparison Types",
    x = "Response Variable",
    y = "BIC Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(-100, -5000), scales = 0.6) +
  ggbreak::scale_y_break(c(-10000, -50000), scales = 0.6)

bic_plot_break
```
Visualization 3: Log-Likelihood Differences Across Comparison Types
```{r}
# Prepare data for plotting
loglik_diff_plot_data <- aligned_data_pronto %>%
  dplyr::select(Response, logLik_Diff_Full_vs_Random, logLik_Diff_Random_vs_Fixed, logLik_Diff_Full_vs_Null) %>%
  tidyr::pivot_longer(
    cols = starts_with("logLik_Diff"),
    names_to = "Comparison_Type",
    values_to = "LogLik_Difference"
  ) %>%
  dplyr::filter(!is.na(LogLik_Difference))

# Create the plot
loglik_plot_break <-
  loglik_diff_plot_data |> 
  ggplot(aes(x = Response, y = LogLik_Difference, fill = Comparison_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Differences in Log-Likelihood Across Comparison Types",
    x = "Response Variable",
    y = "Log-Likelihood Difference",
    fill = "Comparison Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggbreak::scale_y_break(c(100, 5000), scales = 0.6) +
  ggbreak::scale_y_break(c(10000, 50000), scales = 0.6)

loglik_plot_break
```


The updated AIC and BIC plots provide clearer insights into the model comparisons across response variables. AIC values assess the balance between model fit and complexity, with lower values indicating better trade-offs. The AIC plot highlights stark contrasts in "Full vs Null" comparisons, especially for "Soil Quality," where extreme values suggest a poor fit of the null model relative to the full model. This trend reflects the complexity and variability inherent in "Soil Quality" data. Meanwhile, "Full vs Random" and "Random vs Fixed" comparisons exhibit closer alignment for most response variables, such as "Crop Yield" and "Pest and Disease," indicating minimal improvements in fit despite increased model complexity. For "Product Quality," the consistently low AIC values across all comparisons suggest that simpler models are adequate, potentially due to limited variability in the data.

The BIC plot echoes these observations but imposes a stronger penalty for model complexity. The "Full vs Null" comparison again dominates with substantially higher BIC values, particularly for "Soil Quality." This pattern reinforces the view that the null model fails to capture critical variability, though the full model's complexity may still be excessive. In contrast, "Full vs Random" and "Random vs Fixed" show minimal differences in BIC for most variables, further emphasizing the balance achieved by intermediate models. For simpler variables like "Product Quality," BIC values are notably small, underscoring that more parsimonious models can sufficiently describe these data.

In conclusion, these comparisons underscore the importance of tailoring model complexity to specific response variables. For highly variable metrics like "Soil Quality," intermediate models that reduce complexity while maintaining fit may be optimal. Simpler models suffice for "Product Quality" and similarly straightforward variables. These results emphasize that balancing fit and complexity is central to effective model selection, with "Full vs Random" comparisons often offering the best trade-offs across diverse response variables.



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
```





```{r}
# Function to extract heterogeneity and variance components
extract_heterogeneity_metrics <- function(model_set, model_type) {
  bind_rows(
    lapply(names(model_set), function(response) {
      model <- model_set[[response]]
      if (is.null(model)) {
        return(data.frame(
          ResponseVariable = response,
          Tau2 = NA,
          I2 = NA
        ))
      }
      # Extract metrics
      tau2 <- sum(model$sigma2)
      i2 <- round((tau2 / (tau2 + mean(model$vi))) * 100, 1)
      data.frame(
        ResponseVariable = response,
        Tau2 = tau2,
        I2 = i2
      )
    })
  ) %>%
    mutate(ModelType = model_type)
}

# Extract heterogeneity metrics for each model set
heterogeneity_null <- extract_heterogeneity_metrics(null_model_results, "Null")
heterogeneity_minimal_random <- extract_heterogeneity_metrics(minimal_random_results, "Minimal Random")
heterogeneity_fixed <- extract_heterogeneity_metrics(fixed_effects_results, "Fixed Effects")
heterogeneity_random <- extract_heterogeneity_metrics(random_effects_results, "Random Effects")
heterogeneity_random_interaction <- extract_heterogeneity_metrics(random_effects_interaction_results, "Random Effects Interaction")
heterogeneity_full <- extract_heterogeneity_metrics(full_results, "Full")
heterogeneity_full_interaction <- extract_heterogeneity_metrics(full_interaction_results, "Full Interaction")

# Combine all extracted metrics into one data frame
all_heterogeneity_metrics <- bind_rows(
  heterogeneity_null,
  heterogeneity_minimal_random,
  heterogeneity_fixed,
  heterogeneity_random,
  heterogeneity_random_interaction,
  heterogeneity_full,
  heterogeneity_full_interaction
)

# View the combined metrics
all_heterogeneity_metrics |> str()

```
```{r}
# Prepare data for visualization
heterogeneity_plot_data <- all_heterogeneity_metrics %>%
  filter(!is.na(I2) & !is.na(Tau2)) %>%
  mutate(
    I2_transformed = ifelse(I2 == 0, 0.01, I2),   # Handle zero values for log scale
    Tau2_transformed = ifelse(Tau2 == 0, 0.001, Tau2) # Handle zero values for log scale
  )

# Plot for Heterogeneity (I²)
i2_plot <- ggplot(heterogeneity_plot_data, aes(x = ResponseVariable, y = I2_transformed, fill = ModelType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(
    title = "Heterogeneity (I²) Across Models",
    x = "Response Variable",
    y = "I² (Log Scale, %)",
    fill = "Model Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot for Variance Components (Tau²)
tau2_plot <- ggplot(heterogeneity_plot_data, aes(x = ResponseVariable, y = Tau2_transformed, fill = ModelType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(
    title = "Variance Components (Tau²) Across Models",
    x = "Response Variable",
    y = "Tau² (Log Scale)",
    fill = "Model Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine plots into one grid
combined_heterogeneity_plot <- i2_plot / tau2_plot +
  plot_annotation(
    title = "Heterogeneity (I²) and Variance Components (Tau²) Across Models",
    caption = "Logarithmic scales used for better visualization of smaller values."
  )

# Print the combined plot
print(combined_heterogeneity_plot)
```


### Interpretation of Heterogeneity (I²) and Variance Components (Tau²)

The heterogeneity (I²) plot highlights the proportion of variability in effect sizes across studies that is attributable to heterogeneity rather than sampling error. Low I² values signify consistent effect sizes across studies, whereas high I² values point to substantial heterogeneity, which may arise from differences between studies or unmeasured moderators. Tau², on the other hand, quantifies the variance in true effect sizes, offering an absolute measure of between-study variability.

For **Heterogeneity (I²)**, the Full Model shows elevated values for variables like biodiversity and greenhouse gas emissions, indicating substantial heterogeneity that this model captures through its complexity. However, this may reflect overfitting, especially if the model is explaining noise rather than meaningful patterns. This aligns with the fact that highly complex models, while comprehensive, often overstate heterogeneity.

The **Simplified Model**, which includes key moderators without interaction terms, demonstrates moderate I² values for variables like crop yield and product quality. This model strikes a balance between capturing essential variability and avoiding excessive complexity. Its performance suggests it is particularly suited to datasets where heterogeneity is present but not extreme.

The **Minimal Model**, with only an intercept as a random effect, consistently shows very low I² values. While this simplicity is suitable for variables with inherently low heterogeneity, such as product quality, it risks oversimplifying datasets like biodiversity or greenhouse gas emissions, where heterogeneity is critical to understanding variability.

The **Fixed Effects Model**, which excludes random effects, mirrors the Minimal Model in showing low I² values across most variables. While appropriate for datasets with minimal between-study variability, it is unsuitable for datasets with higher complexity, as it fails to account for essential heterogeneity.

The Tau² plot confirms these patterns, showing greater variance components for the Full Model in complex datasets, while the Simplified and Minimal Models exhibit much smaller Tau² values. This reflects the trade-off between comprehensiveness and parsimony.

### Implications for Meta-Analysis

The choice of model profoundly affects the interpretation of heterogeneity and the conclusions of a meta-analysis. The Full Model captures extensive variability but risks overcomplication in datasets with consistent patterns. The Simplified Model offers a more balanced approach, capturing meaningful heterogeneity without overfitting, making it the most robust choice for many variables. In contrast, the Minimal and Fixed Effects Models provide parsimonious alternatives for datasets with minimal heterogeneity but may oversimplify more complex datasets.

### Reporting and Application

When communicating results, it is crucial to pair heterogeneity metrics like I² and Tau² with model fit metrics (e.g., AIC and BIC) to clearly justify model selection. The Simplified Model, with its consistent performance across most variables, supports interpretable and actionable conclusions, while acknowledging that certain response variables may require more refined approaches. Aligning model choice with the data’s complexity ensures the meta-analysis yields robust and meaningful insights.

### Conclusion

The Simplified Model emerges as the optimal choice for balancing complexity and parsimony, particularly for response variables with moderate heterogeneity. For highly heterogeneous variables, however, refinements to the Full Model or the introduction of interaction terms may better capture variability. By aligning model complexity with data characteristics, meta-analysis results can remain both defensible and interpretable, fostering better decision-making and scientific communication.




```{r}
# Function to extract diagnostics from a single model
extract_model_diagnostics <- function(model, response_variable) {
  if (is.null(model)) {
    return(data.frame(
      ResponseVariable = response_variable,
      AIC = NA,
      BIC = NA,
      LogLikelihood = NA,
      Tau2 = NA,
      I2 = NA
    ))
  }

  # Extract diagnostics
  aic <- tryCatch(AIC(model), error = function(e) NA)
  bic <- tryCatch(BIC(model), error = function(e) NA)
  log_likelihood <- tryCatch(as.numeric(logLik(model)), error = function(e) NA)
  tau2 <- tryCatch(sum(model$sigma2), error = function(e) NA)
  i2 <- tryCatch(round((tau2 / (tau2 + mean(model$vi))) * 100, 1), error = function(e) NA)

  data.frame(
    ResponseVariable = response_variable,
    AIC = aic,
    BIC = bic,
    LogLikelihood = log_likelihood,
    Tau2 = tau2,
    I2 = i2
  )
}

# Function to extract diagnostics for all response variables in a model set
extract_diagnostics_for_model_set <- function(model_set, model_type) {
  bind_rows(
    lapply(names(model_set), function(response_variable) {
      extract_model_diagnostics(model_set[[response_variable]], response_variable)
    })
  ) %>%
    mutate(ModelType = model_type)
}

# Extract diagnostics for all model sets
null_model_diagnostics <- extract_diagnostics_for_model_set(null_model_results, "Null")
minimal_random_diagnostics <- extract_diagnostics_for_model_set(minimal_random_results, "Minimal Random")
fixed_effects_diagnostics <- extract_diagnostics_for_model_set(fixed_effects_results, "Fixed Effects")
random_effects_diagnostics <- extract_diagnostics_for_model_set(random_effects_results, "Random Effects")
random_effects_interaction_diagnostics <- extract_diagnostics_for_model_set(random_effects_interaction_results, "Random Effects Interaction")
full_diagnostics <- extract_diagnostics_for_model_set(full_results, "Full")
full_interaction_diagnostics <- extract_diagnostics_for_model_set(full_interaction_results, "Full Interaction")

# Combine all diagnostics into a single data frame
all_meta_analysis_model_diagnostics <- bind_rows(
  null_model_diagnostics,
  minimal_random_diagnostics,
  fixed_effects_diagnostics,
  random_effects_diagnostics,
  random_effects_interaction_diagnostics,
  full_diagnostics,
  full_interaction_diagnostics
)

# Save the extracted diagnostics for future use
saveRDS(all_meta_analysis_model_diagnostics, file.path(output_dir, "all_meta_analysis_model_diagnostics.rds"))

all_meta_analysis_model_diagnostics
```

```{r}
# Summarize model diagnostics into a table
summary_table_meta_data_all_models_data <- all_meta_analysis_model_diagnostics %>%
  select(ResponseVariable, ModelType, AIC, BIC, LogLikelihood, Tau2, I2) %>%
  group_by(ResponseVariable, ModelType) %>%
  summarise(
    AIC = round(mean(AIC, na.rm = TRUE), 2),
    BIC = round(mean(BIC, na.rm = TRUE), 2),
    LogLikelihood = round(mean(LogLikelihood, na.rm = TRUE), 2),
    Tau2 = round(mean(Tau2, na.rm = TRUE), 2),
    I2 = round(mean(I2, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Display the summarized table
print(summary_table_meta_data_all_models_data)
```

```{r}
# Create a formatted summary table
diagnostics_table_meta_data_all_models <- summary_table_meta_data_all_models_data %>%
  gt() %>%
  tab_header(
    title = "Model Diagnostics Summary",
    subtitle = "Comparison of Key Metrics Across Models by Response Variable"
  ) %>%
  cols_label(
    ResponseVariable = "Response Variable",
    ModelType = "Model Type",
    AIC = "AIC",
    BIC = "BIC",
    LogLikelihood = "Log-Likelihood",
    Tau2 = "Tau²",
    I2 = "I² (%)"
  ) %>%
  fmt_number(
    columns = c(AIC, BIC, LogLikelihood, Tau2, I2),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = "small",
    column_labels.font.size = "medium",
    heading.align = "center"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

# Display the table
print(diagnostics_table_meta_data_all_models)
```



##########################################################################
Model Diagnostics: Heterogeneity also called Residual Heterogeneity Partitioning
##########################################################################


```{r}
# Check the names of response variables in each model set
response_variables <- unique(unlist(lapply(list(
  null = null_model_results,
  minimal = minimal_random_results,
  fixed = fixed_effects_results,
  simplified = random_effects_results,
  full = full_results
), names)))

# Function to compute heterogeneity partitioning
compute_heterogeneity_partitioning <- function(model, response_variable, model_type) {
  if (is.null(model)) {
    return(data.frame(
      ResponseVariable = response_variable,
      ModelType = model_type,
      TotalHeterogeneity = NA,
      ExplainedHeterogeneity = NA,
      ResidualHeterogeneity = NA,
      ExplainedProportion = NA,
      ResidualProportion = NA
    ))
  }
  QE <- model$QE
  QM <- model$QM
  if (!is.null(QE) && !is.null(QM)) {
    residual_heterogeneity <- QE - QM
    explained_proportion <- ifelse(QE > 0, (QM / QE) * 100, NA)
    residual_proportion <- ifelse(QE > 0, (residual_heterogeneity / QE) * 100, NA)
    return(data.frame(
      ResponseVariable = response_variable,
      ModelType = model_type,
      TotalHeterogeneity = QE,
      ExplainedHeterogeneity = QM,
      ResidualHeterogeneity = max(residual_heterogeneity, 0),
      ExplainedProportion = explained_proportion,
      ResidualProportion = residual_proportion
    ))
  }
  return(data.frame(
    ResponseVariable = response_variable,
    ModelType = model_type,
    TotalHeterogeneity = NA,
    ExplainedHeterogeneity = NA,
    ResidualHeterogeneity = NA,
    ExplainedProportion = NA,
    ResidualProportion = NA
  ))
}

# Extract heterogeneity for all models
heterogeneity_results <- list(
  null = null_model_results,
  minimal = minimal_random_results,
  fixed = fixed_effects_results,
  simplified = random_effects_results,
  full = full_results
) %>%
  purrr::imap_dfr(~ {
    purrr::map_dfr(response_variables, function(response_variable) {
      if (!is.null(.x[[response_variable]])) {
        compute_heterogeneity_partitioning(.x[[response_variable]], response_variable, .y)
      } else {
        compute_heterogeneity_partitioning(NULL, response_variable, .y)
      }
    })
  })

# View final heterogeneity results
print(heterogeneity_results)
heterogeneity_results |> str()
```

```{r}
# Clean the data
cleaned_heterogeneity_results <- heterogeneity_results %>%
  mutate(
    ResidualProportion = ifelse(ResidualProportion < 0, 0, ResidualProportion),
    ExplainedProportion = ifelse(ExplainedProportion > 100, 100, ExplainedProportion)
  )

# Reshape data for heterogeneity values (Total, Explained, Residual)
heterogeneity_long <- cleaned_heterogeneity_results %>%
  pivot_longer(
    cols = c(TotalHeterogeneity, ExplainedHeterogeneity, ResidualHeterogeneity),
    names_to = "HeterogeneityType",
    values_to = "HeterogeneityValue"
  )

# Plot 1: Heterogeneity values (bar plot)
heterogeneity_plot <- ggplot(heterogeneity_long, aes(
  x = ResponseVariable, 
  y = HeterogeneityValue, 
  fill = HeterogeneityType
)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_y_log10(labels = scales::comma) + # Log scale for better visualization
  labs(
    title = "Partitioned Heterogeneity Across Models",
    x = "Response Variable",
    y = "Heterogeneity Value (log scale)",
    fill = "Heterogeneity Type"
  ) +
  facet_wrap(~ModelType, ncol = 2) + # Separate by model type
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Plot 2: Proportion values (line plot)
proportion_plot <- cleaned_heterogeneity_results %>%
  select(ResponseVariable, ModelType, ExplainedProportion, ResidualProportion) %>%
  pivot_longer(
    cols = c(ExplainedProportion, ResidualProportion),
    names_to = "ProportionType",
    values_to = "ProportionValue"
  ) %>%
  ggplot(aes(
    x = ResponseVariable, 
    y = ProportionValue, 
    group = ProportionType, 
    color = ProportionType
  )) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5), alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Explained vs. Residual Proportion",
    x = "Response Variable",
    y = "Proportion (%)",
    color = "Proportion Type"
  ) +
  facet_wrap(~ModelType, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Combine the plots into one
combined_plot <- heterogeneity_plot / proportion_plot +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Heterogeneity and Proportion Metrics Across Models",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

# Display the combined plot
print(combined_plot)
```


This plot provides a detailed view of the relationship between explained and residual heterogeneity proportions across various meta-analysis models, alongside their performance in partitioning total heterogeneity. Explained heterogeneity measures the variance in effect sizes attributable to predictors or moderators in the model, while residual heterogeneity captures the unexplained variance. These metrics are critical for evaluating how well different models account for patterns in the data while minimizing unaccounted variation.

### Insights from the Plot:
1. **Partitioned Heterogeneity (Top Panel):**  
   The top panel displays the total, explained, and residual heterogeneity across response variables for each model type. The **null model**, as expected, exhibits high residual heterogeneity and minimal explained heterogeneity due to its lack of fixed or random effects. The **minimal model** improves on this by introducing random effects, capturing more explained heterogeneity while reducing residual variability. Models with greater complexity, such as the **fixed**, **simplified**, and **full models**, show varying proportions of explained heterogeneity. For instance, the **full model** often captures substantial explained heterogeneity but risks overfitting, evident in certain response variables with high unexplained residuals.

2. **Explained vs. Residual Proportions (Bottom Panel):**  
   The bottom panel illustrates the trade-off between explained and residual heterogeneity proportions across models. For most response variables, the **fixed model** achieves a strong reduction in residual heterogeneity while increasing the explained proportion, suggesting its effectiveness in leveraging fixed effects to capture variability. The **simplified model** demonstrates balanced performance, striking a middle ground between explained and residual proportions. The **full model**, while often capturing the highest explained heterogeneity, shows inconsistencies, indicating potential challenges with overfitting or the inclusion of unnecessary moderators.

3. **Response Variable-Specific Performance:**  
   Response variables like **soil quality** and **pest and disease** exhibit large unexplained residual heterogeneity in simpler models (e.g., null, minimal), underscoring the need for more complex modeling frameworks. Conversely, response variables like **product quality** or **biodiversity** show relatively stable proportions across most models, suggesting lower inherent heterogeneity.

### Implications for Meta-Analysis:
This plot highlights the trade-offs between model complexity and explanatory power. While simpler models like the null or minimal models are easy to interpret, they fail to capture significant heterogeneity in complex datasets. More complex models, such as the full or simplified models, excel at explaining heterogeneity but may introduce overfitting, reducing their generalizability. The fixed model appears to balance these factors effectively, particularly for response variables with moderate heterogeneity.

### Limitations and Recommendations:
One limitation of this analysis is that it focuses on heterogeneity metrics without linking them explicitly to effect size accuracy or hypothesis testing. Future work should assess how heterogeneity partitioning affects the broader objectives of the meta-analysis, such as robustness in effect size estimation or predictive performance. The choice of model should align with the study's goals, balancing parsimony and interpretability with the ability to explain key patterns in the data.

### Conclusion:
The fixed and simplified models emerge as strong candidates for meta-analytic applications, offering a balance between explained heterogeneity and manageable residuals. However, for response variables with high complexity (e.g., soil quality), full models may provide additional insights at the cost of interpretability. Careful justification of model selection based on these metrics is essential for robust and defensible conclusions in meta-analysis.













```{r}

var.forestplot <- function(rma.object){
  count_study = data.frame(variable = character(0),
                           nb_study = integer(0),
                           nb_entries = integer(0),
                           estimate = numeric(0),
                           conf.low = numeric(0),
                           conf.high = numeric(0))
  model_values_table = as.data.frame(tidy(rma.object, conf.int = T))
  rownames(model_values_table) = model_values_table[,1]
  for (i in unique(na.omit(get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), pos = get(as.character(rma.object$call$data)))))){
    count_study[nrow(count_study)+1,] = c(i,
                                          length(unique(subset(get(as.character(rma.object$call$data)),
                                                               get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), 
                                                                   pos = get(as.character(rma.object$call$data))) == i)$Source.ID)),
                                          nrow(subset(get(as.character(rma.object$call$data)),
                                                      get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), 
                                                          pos = get(as.character(rma.object$call$data))) == i)),
                                          rma.object$b[paste(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), i, sep = ""), ],
                                          model_values_table[paste(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), i, sep = ""),]$conf.low,
                                          model_values_table[paste(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), i, sep = ""),]$conf.high)
  }
  if(is.na(count_study$variable[nrow(count_study)])){
    count_study = count_study[-nrow(count_study),]
  }
  count_study = count_study[na.omit(match(levels(get(gsub(" - 1","",as.character(rma.object$formula.mods)[2]), 
                                                     pos = get(as.character(rma.object$call$data)))),
                                          count_study$variable)),]
  count_study$estimate = as.numeric(count_study$estimate)
  count_study$conf.low = as.numeric(count_study$conf.low)
  count_study$conf.high = as.numeric(count_study$conf.high)
  count_study$variable = factor(count_study$variable, levels = count_study[order(count_study$estimate),]$variable)
  
  count_study %>%
    ggplot(aes(y = variable, x = estimate,
               xmin = conf.low, xmax = conf.high)) +
    geom_point(size = 4) +
    geom_errorbarh(height = .5) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
    theme_bw() + # the theme_xx goes before theme, so theme changes prevail over complete themes
    theme(text = element_text(size = 14, color = "black"),
          panel.spacing = unit(1, "lines")) +
    scale_y_discrete(labels = paste(count_study$variable[order(count_study$variable)],
                                    "(",count_study$nb_study[order(count_study$variable)],
                                    "/", count_study$nb_entries[order(count_study$variable)], ")")) +
    labs(x = "log ratio change relative to monocropping", y = "")
}
```
































```{r}
# Set the working directory automatically using 'here'
setwd(here::here())


# Define your working directory using 'here'
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

# Load datasets and V_matrices
non_imp_dataset <- readRDS(file.path(output_dir, "non_imp_dataset.rds"))
imp_dataset <- readRDS(file.path(output_dir, "imp_dataset.rds"))


V_matrix_non_imp <- readRDS(file.path(output_dir, "V_matrix_non_imp_dataset.rds"))
V_matrix_imp <- readRDS(file.path(output_dir, "V_matrix_imp_dataset.rds"))
```



```{r}
# Define random effects structure
random_effects_formular <- ~ 1 | id_article/id_obs

# Define all potential moderators
moderator_formula <- c("tree_type", "crop_type", "season", "soil_texture")  
```
OBS

Are they calculating the effect size on the full dataset or seperately on the subset datasets on each reponse variable? - it should be the same approach!
  Are they running the LOO on the full dataset or the subset datasets (subsetted on individual responsive variables) seperately?
  Are they doing publication bias evaluation (e.g. funnel plot) on the full dataset or the subset datasets (subsetted on individual responsive variables) seperately?
  Is the current LOO assessment running through each study (id_article) or using another identifier (i.e. id_obs)? - because it should be using id_article!
  I want to make a plot of overall effect size on the three main response variables (Biodiversity, Crop yield and Soil quality), like Fig. 1, however the plot I wish to make should have overall effects of agroforestry for each response variable relative to monocropping, this plot should include error bars represent 95% confidence intervals. Hence, visualising how confidence intervals overlapping zero indicate no significant differences with the monocropping and for each response variable numbers should be shown in parentheses indicating 1) the number of studies for the respective response variable and 2) the number of data records (observations) for the respective response variable
Progress bar refined classes


```{r}
split_by_response_variable <- function(dataset, selected_response_variables = NULL) {
  # Extract data and V_matrix
  data <- dataset$data
  V_matrix <- dataset$V_matrix
  
  # Ensure rownames for data
  if (is.null(rownames(data))) {
    rownames(data) <- as.character(data$id_obs)
  }
  
  # Ensure V_matrix dimnames align with data rownames
  if (is.null(dimnames(V_matrix))) {
    dimnames(V_matrix) <- list(rownames(data), rownames(data))
  }
  
  # If selected_response_variables is provided, filter the data
  if (!is.null(selected_response_variables)) {
    data <- data[data$response_variable %in% selected_response_variables, , drop = FALSE]
    
    # Check if any data remains after filtering
    if (nrow(data) == 0) {
      stop("No data remains after filtering for selected response variables.")
    }
    
    # Subset V_matrix to match filtered data
    indices <- rownames(data)
    V_matrix <- V_matrix[indices, indices, drop = FALSE]
  }
  
  # Split data by response variable
  response_splits <- split(data, data$response_variable)
  
  # Align and validate V_matrix for each subset
  splits <- lapply(response_splits, function(sub_data) {
    indices <- rownames(sub_data)
    V_matrix_subset <- V_matrix[indices, indices, drop = FALSE]
    if (!all(rownames(sub_data) == rownames(V_matrix_subset))) {
      stop("Mismatch between data rows and V_matrix rows.")
    }
    list(data = sub_data, V_matrix = V_matrix_subset)
  })
  
  return(splits)
}
```

```{r}
cat("Data rows:", nrow(imp_dataset), "\n")
cat("V_matrix dimensions:", dim(V_matrix_imp), "\n")
cat("Data rows:", nrow(non_imp_dataset), "\n")
cat("V_matrix dimensions:", dim(V_matrix_non_imp), "\n")
```


```{r}
# Define the selected response variables
selected_responses <- c("Crop yield", "Soil quality", "Biodiversity") # "Pests and Diseases", "Water quality", "Product quality"


####################################################################################################


# Prepare datasets and their corresponding V_matrices
datasets <- list(
  imp_dataset = list(data = imp_dataset, V_matrix = V_matrix_imp)
  #non_imp_dataset = list(data = non_imp_dataset, V_matrix = V_matrix_non_imp)
)

# Split datasets
splits <- lapply(datasets, function(ds) {
  split_by_response_variable(ds, selected_response_variables = selected_responses)
})

# Inspect the splits
str(splits)

# View sneak-peak
splits$imp_dataset$Biodiversity |> glimpse()
```



```{r}
# Get unique response variables
unique_response_vars <- unique(non_imp_dataset$response_variable)
print(unique_response_vars)

# Split the dataset
split_data <- split(non_imp_dataset, non_imp_dataset$response_variable)

# Assign names to the list elements
names(split_data) <- unique_response_vars
```


Based on the description in the methods section, the "cabbage approach" for Leave-One-Out (LOO) sensitivity analysis does not include moderators. Instead, it uses a global model to assess the overall effect size without accounting for moderators. This approach focuses solely on the effect size and how its confidence intervals change when individual studies are excluded.

```{r}
# Generic Leave-One-Out Sensitivity Analysis Function with Additional Metrics
loo_analysis <- function(data_split, response_var_name) {
  # Extract data and V_matrix for the current response variable
  data <- data_split$data
  V_matrix <- data_split$V_matrix
  
  # Initialize a data frame to store results
  exclude_study <- data.frame(
    id_article = character(0),
    estimate = numeric(0),
    se = numeric(0),
    p.value = numeric(0),
    tau2 = numeric(0),
    I2 = numeric(0),
    AIC = numeric(0),
    BIC = numeric(0),
    logLik = numeric(0),
    QM = numeric(0),
    pval_QM = numeric(0)
  )
  
  # Perform Leave-One-Out (LOO) analysis
  for (id in base::unique(data$id_article)) {
    # Exclude the current article
    reduced_data <- subset(data, id_article != id)
    
    # Subset the variance-covariance matrix
    indices <- which(data$id_article != id)
    reduced_V_matrix <- V_matrix[indices, indices, drop = FALSE]
    
    # Ensure the dimensions of the data and V_matrix match
    if (nrow(reduced_data) != nrow(reduced_V_matrix)) {
      cat("Skipping id_article", id, ": Dimension mismatch!\n")
      next
    }
    
    # Try to fit the model
    tryCatch({
      # Fit the model without the current article
      model_without_one <- rma.mv(
        yi = yi,
        V = reduced_V_matrix,
        random = ~1 | id_article,
        data = reduced_data,
        method = "REML"
      )
      
      # Collect results
      exclude_study <- rbind(
        exclude_study,
        data.frame(
          id_article = id,
          estimate = as.numeric(coef(model_without_one)),
          se = as.numeric(summary(model_without_one)$se),
          p.value = as.numeric(summary(model_without_one)$pval),
          tau2 = if (!is.null(model_without_one$tau2)) model_without_one$tau2 else NA,
          I2 = if (!is.null(model_without_one$tau2) && !is.null(model_without_one$sigma2)) {
            100 * model_without_one$tau2 / (model_without_one$tau2 + sum(model_without_one$sigma2))
          } else {
            NA
          },
          AIC = tryCatch(AIC(model_without_one), error = function(e) NA),
          BIC = tryCatch(BIC(model_without_one), error = function(e) NA),
          logLik = tryCatch(logLik(model_without_one), error = function(e) NA),
          QM = if (!is.null(model_without_one$QM)) model_without_one$QM else NA,
          pval_QM = if (!is.null(model_without_one$pval.QM)) model_without_one$pval.QM else NA
        )
      )
    }, error = function(e) {
      # Log errors but continue
      cat("Error excluding id_article", id, ":", conditionMessage(e), "\n")
    })
  }
  
  # Add a response variable column for clarity
  exclude_study$response_variable <- response_var_name
  
  return(exclude_study)
}
```

```{r, eval=FALSE}
##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################
# Run Leave-One-Out (LOO) Sensitivity Analysis on dataset subsets for each response variable [SUBSET DATASETS]
# Simplified Leave-One-Out Sensitivity Analysis
# Perform LOO for all datasets and response variables
loo_results <- lapply(names(splits), function(dataset_name) {
  dataset_splits <- splits[[dataset_name]]
  
  # Perform LOO for each response variable in the dataset
  response_results <- lapply(names(dataset_splits), function(response_var) {
    cat("Processing dataset:", dataset_name, ", Response variable:", response_var, "\n")
    
    # Run the LOO analysis and add dataset and response variable columns
    loo_analysis(dataset_splits[[response_var]], response_var) %>%
      mutate(
        dataset = dataset_name,  # Add the dataset name
        response_variable = response_var  # Add the response variable
      )
  })
  
  # Combine results for all response variables in this dataset
  do.call(rbind, response_results)
})

# Combine LOO results across all datasets
final_loo_results <- do.call(rbind, loo_results)

##############################################################
# Define the output directory
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")

# Ensure the directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the full combined results
saveRDS(
  final_loo_results,
  file = file.path(output_dir, "final_loo_results_combined.rds")
)

# Save results for each dataset individually
names(loo_results) <- names(splits)  # Assign names for individual datasets
saveRDS(
  loo_results,
  file = file.path(output_dir, "loo_results_individual_datasets.rds")
)

# Messages for saved files
cat("Combined LOO sensitivity results saved to:", file.path(output_dir, "final_loo_results_combined.rds"), "\n")
cat("Individual LOO sensitivity results saved to:", file.path(output_dir, "loo_results_individual_datasets.rds"), "\n")
##############################################################

##################################################
# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("Total time taken:", time.taken, "\n")
##############################################################
# Last go: (24/11-24)

```

```{r}
# Load the saved results
# Load combined results
combined_loo_res <- readRDS(file.path(output_dir, "final_loo_results_combined.rds"))
combined_loo_res
# str(combined_loo_res)  # Inspect structure

# Load individual dataset results
# individual_results <- readRDS(file.path(output_dir, "loo_results_individual_datasets.rds"))
# names(individual_results)  # Should show dataset names
# str(individual_results[[1]])  # Inspect the first dataset's LOO results

```

Influence Diagnostics
```{r}
combined_loo_res |> str()
```

```{r}
# Step 1: Identify overlapping studies
overlapping_studies <- combined_loo_res %>%
  distinct(response_variable, id_article) %>%  # Get unique combinations
  count(id_article) %>%  # Count how many response variables each study appears in
  filter(n > 1) %>%  # Filter studies that appear in more than 1 response variable
  pull(id_article)  # Extract the IDs of these studies

# Step 2: Add a flag for overlapping studies
all_loo_results_for_plotting <- combined_loo_res %>%
  mutate(
    overlap_flag = ifelse(id_article %in% overlapping_studies, TRUE, FALSE)
  ) |> 
  mutate(
    lower_ci = estimate - 1.96 * se,
    upper_ci = estimate + 1.96 * se
  )

# Step 3: Create the forest plot with highlights
all_loo_results_plot <- all_loo_results_for_plotting %>%
  ggplot(aes(y = as.factor(id_article), x = estimate, color = response_variable)) +
  geom_point(aes(shape = overlap_flag), size = 3) +  # Different shape for overlapping studies
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, alpha = 0.7) +  # Horizontal CI bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Reference line at 0
  facet_wrap(~response_variable, scales = "free_y") +  # Facet by response variable
  labs(
    y = "Excluded Study (ID)",
    x = "Estimated Effect Size",
    title = "Top 10 Most Influential Unique Studies (Leave-One-Out Analysis)",
    subtitle = "Highlighted studies appear in more than one response variable",
    color = "Response Variable",
    shape = "Overlapping Study"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    strip.text = element_text(size = 12)  # Larger facet labels
  )

all_loo_results_plot
# potentially highlight studies that are to-influential and present in more than 1 response variable, e.g. study 32 (in both crop yield and soil quality)
```



Between-Study Heterogeneity
```{r}
# Check data structure
str(imp_dataset)

# Check dimensions of V_matrix
dim(V_matrix_imp)

```


```{r}
# Simplified model fitting on non_imp_dataset
tryCatch({
  model <- rma.mv(
    yi = yi, 
    V = V_matrix_imp, 
    random = ~ 1 | id_article,  # Simplified random structure
    data = imp_dataset, 
    method = "REML"
  )
  summary(model)  # Print model summary
}, error = function(e) {
  message("Error: ", e$message)
})

```
```{r}
# Check dimensions
dim(V_matrix_imp)
nrow(imp_dataset)

# Check if V_matrix is symmetric
is_symmetric <- all.equal(V_matrix_imp, t(V_matrix_imp))
print(is_symmetric)

# Check diagonal elements
diag_positive <- all(diag(V_matrix_imp) > 0)
print(diag_positive)

# Inspect potential outliers in vi
summary(non_imp_dataset$vi)

```
```{r}
calculate_between_study_variance <- function(data, V_matrix, random_formula, dataset_name) {
  tryCatch({
    # Validate V_matrix and ensure positive definiteness
    is_symmetric <- all.equal(V_matrix, t(V_matrix))
    diag_positive <- all(diag(V_matrix) > 0)
    
    if (!is_symmetric || !diag_positive) {
      stop("V_matrix is invalid: Ensure it's symmetric and diagonals are positive.")
    }
    
    # Fit the random-effects model
    model <- rma.mv(
      yi = yi, 
      V = V_matrix, 
      random = random_formula, 
      data = data, 
      method = "ML"
    )
    
    # Extract key metrics
    tibble(
      dataset = dataset_name,
      tau2 = model$tau2,  # Between-study variance
      I2 = 100 * model$tau2 / (model$tau2 + sum(model$sigma2)),  # Proportion of heterogeneity
      H2 = 1 + (model$tau2 / mean(model$sigma2)),  # Total variance to sampling variance ratio
      Q = model$QE,  # Cochran's Q
      pval_Q = model$QEp,  # p-value for Q
      QM = model$QM,  # Test for moderators
      pval_QM = model$pval.QM  # p-value for moderators
    )
  }, error = function(e) {
    # Capture errors and return NA metrics
    message("Error for dataset: ", dataset_name, " - ", e$message)
    tibble(
      dataset = dataset_name,
      tau2 = NA, I2 = NA, H2 = NA, Q = NA, pval_Q = NA, QM = NA, pval_QM = NA
    )
  })
}
```

```{r}
# List of datasets and their V_matrices
datasets <- list(
  imp_dataset = list(data = imp_dataset, V_matrix = V_matrix_imp)
)

# Loop through datasets and compute Between-Study Heterogeneity
dataset_metrics <- lapply(names(datasets), function(dataset_name) {
  dataset <- datasets[[dataset_name]]
  calculate_between_study_variance(
    data = dataset$data, 
    V_matrix = dataset$V_matrix, 
    random_formula = ~ 1 | id_article,  # Adjust random structure if needed
    dataset_name = dataset_name
  )
}) %>% 
  bind_rows()

# View results
str(dataset_metrics)

```






Funnel Plot
```{r}
split_data |> str()
```
```{r}
# Function to calculate study-level effects
get_study_effects <- function(data, id_var, yi, vi, random_formula) {
  study_effects <- data.frame(id_article = character(0), effect_size = numeric(0), se = numeric(0))
  
  for (id in unique(data[[id_var]])) {
    subset_data <- subset(data, data[[id_var]] == id)
    cat("Processing id_article:", id, "- Rows:", nrow(subset_data), "\n")
    
    if (nrow(subset_data) > 1) {
      # Fit a random-effects model using metafor
      study_model <- tryCatch(
        {
          rma.mv(
            yi = subset_data[[yi]], 
            V = subset_data[[vi]], 
            random = random_formula, 
            data = subset_data, 
            method = "REML"
          )
        },
        error = function(e) {
          message("Error fitting model for id_article:", id, "-", e$message)
          return(NULL)
        }
      )
      
      if (!is.null(study_model)) {
        study_effects <- rbind(study_effects, data.frame(
          id_article = id,
          effect_size = as.numeric(study_model$b),
          se = as.numeric(study_model$se)
        ))
      }
    } else {
      # Handle cases with single observations
      study_effects <- rbind(study_effects, data.frame(
        id_article = id,
        effect_size = subset_data[[yi]],
        se = sqrt(subset_data[[vi]])
      ))
    }
  }
  
  return(study_effects)
}

# Function to generate funnel plots for each response variable
generate_funnel_plots <- function(split_data) {
  lapply(names(split_data), function(response_var) {
    data <- split_data[[response_var]]
    
    if (!is.null(data) && all(c("yi", "vi") %in% names(data))) {
      cat("Generating funnel plots for:", response_var, "\n")
      
      # Fit an equal-effects model using metafor
      res <- tryCatch(
        {
          rma(
            yi = data$yi, 
            vi = data$vi, 
            data = data, 
            measure = "GEN", 
            method = "EE"
          )
        },
        error = function(e) {
          message("Error fitting model for ", response_var, ": ", e$message)
          return(NULL)
        }
      )
      
      if (!is.null(res)) {
        # Create a 2x2 grid of metafor funnel plots
        par(mfrow = c(2, 2))
        funnel(res, main = paste("Standard Error -", response_var))
        funnel(res, yaxis = "vi", main = paste("Sampling Variance -", response_var))
        funnel(res, yaxis = "seinv", main = paste("Inverse Standard Error -", response_var))
        funnel(res, yaxis = "vinv", main = paste("Inverse Sampling Variance -", response_var))
      }
    } else {
      message("No valid data for response variable:", response_var)
    }
  })
}
```

```{r}
# Main Workflow
study_effects_results <- lapply(names(split_data), function(response_var) {
  data <- split_data[[response_var]]
  
  if (!is.null(data) && all(c("yi", "vi") %in% names(data))) {
    cat("Processing response variable:", response_var, "\n")
    result <- get_study_effects(
      data = data, 
      id_var = "id_article", 
      yi = "yi", 
      vi = "vi",
      random_formula = ~ 1 | exp_id / id_obs  # Hierarchical structure
    )
    
    if (!is.null(result)) {
      result <- result %>% mutate(response_variable = response_var)
    }
    return(result)
  } else {
    message("No valid data for:", response_var)
    return(NULL)
  }
})

# Combine results into a single data frame
study_effects_df <- do.call(rbind, study_effects_results)

# Debugging: Check structure of results
cat("Final study_effects_df structure:\n")
glimpse(study_effects_df)

# Generate and display funnel plots
cat("Generating funnel plots...\n")
generate_funnel_plots(split_data)
```

Forest Plot in RevMan Style

```{r}

```

Key Model Metrics and Regression Estimates

```{r}

```
























Heterogeneity Metrics Across Response Variables
```{r}

```





Extract Model Diagnostics for Response Variables

Prepare and Process LOO Data for Plotting

Visualize LOO Influence

Summarize LOO Results for Each Response Variable



Metafor Diagnostics Plots

```{r}

```













```{r}
# Helper function to filter out single-level factors

# Enhanced helper function to filter out unsuitable data for meta-regression
filter_data_for_moderator_analysis <- function(data, moderators) {
  # Ensure `yi` and `vi` are included and remove single-level factors
  filtered_data <- data %>%
    select(yi, vi, response_variable, id_article, exp_id, all_of(moderators)) %>%
    filter(n_distinct(id_article) > 1)
  
  # Check each moderator and keep only those with at least two distinct levels
  valid_moderators <- moderators[sapply(moderators, function(mod) {
    n_distinct(filtered_data[[mod]], na.rm = TRUE) > 1
  })]
  
  # Remove rows with missing values for valid moderators
  filtered_data <- filtered_data %>%
    select(yi, vi, response_variable, id_article, exp_id, all_of(valid_moderators)) %>%
    drop_na()
  
  # Return the filtered data and the list of valid moderators
  list(data = filtered_data, valid_moderators = valid_moderators)
}
```


#############
# STEP 3
##########################################################################################################################################
PERFORM MODERATOR ANALYSIS FOR SUBGROUPS AND THEN INCLUDE IN GENERAL META-REGRESSION
##########################################################################################################################################

```{r}
# Helper function to align data and V_matrix
align_data_and_v_matrix <- function(data, V_matrix) {
  common_ids <- intersect(rownames(data), rownames(V_matrix))
  if (length(common_ids) < 2) {
    cat("Warning: Too few common rownames between data and V_matrix. Skipping analysis.\n")
    return(NULL)
  }
  data <- data[common_ids, , drop = FALSE]
  V_matrix <- V_matrix[common_ids, common_ids, drop = FALSE]
  return(list(data = data, V_matrix = V_matrix))
}

# Helper function to filter moderators with at least two levels
filter_valid_moderators <- function(data, moderators) {
  valid_moderators <- sapply(moderators, function(moderator) {
    n_distinct(data[[moderator]]) > 1
  })
  return(moderators[valid_moderators])
}
```

```{r}
# Function for subgroup analysis
run_subgroup_analysis <- function(data, moderator) {
  if (n_distinct(data[[moderator]]) < 2) {
    cat("Insufficient levels for moderator:", moderator, "\n")
    return(NULL)
  }
  
  subgroup_summary <- data %>%
    group_by(!!sym(moderator)) %>%
    summarise(
      mean_yi = mean(yi, na.rm = TRUE),
      ci_low = mean_yi - 1.96 * sd(yi) / sqrt(n()),
      ci_high = mean_yi + 1.96 * sd(yi) / sqrt(n())
    )
  
  print(subgroup_summary)
  return(subgroup_summary)
}
```

```{r}
# Function for meta-regression analysis
run_meta_regression <- function(data, V_matrix, moderator) {
  cat("\nAnalyzing moderator:", moderator, "\n")
  if (n_distinct(data[[moderator]]) < 2) {
    cat("Insufficient levels for moderator:", moderator, "\n")
    return(NULL)
  }
  
  tryCatch({
    model <- rma.mv(
      yi = yi,
      V = V_matrix,
      mods = as.formula(paste("~", moderator)),
      random = list(~ 1 | id_article, ~ 1 | id_article/response_variable),
      data = data,
      method = "REML"
    )
    print(summary(model))
    return(model)
  }, error = function(e) {
    cat("Error in meta-regression for moderator:", moderator, "-", e$message, "\n")
    return(NULL)
  })
}
```

```{r}
# List to store results
moderator_results <- list()

# Loop through response variables
for (response_var in response_variables) {
  cat("\nAnalyzing response variable:", response_var, "\n")
  
  # Get the subset and corresponding V_matrix
  data_subset <- response_splits$imp[[response_var]]
  V_matrix <- v_matrices$v_matrix_imp[[response_var]]
  
  # Align data and V_matrix
  aligned_data <- align_data_and_v_matrix(data_subset, V_matrix)
  if (is.null(aligned_data)) next
  
  data_subset <- aligned_data$data
  V_matrix <- aligned_data$V_matrix
  
  # Filter valid moderators
  valid_moderators <- filter_valid_moderators(data_subset, moderators)
  if (length(valid_moderators) == 0) {
    cat("No valid moderators for analysis. Skipping...\n")
    next
  }
  
  # Perform subgroup and meta-regression analysis
  response_results <- list()
  for (moderator in valid_moderators) {
    subgroup_result <- run_subgroup_analysis(data_subset, moderator)
    meta_regression_result <- run_meta_regression(data_subset, V_matrix, moderator)
    response_results[[moderator]] <- list(
      subgroup = subgroup_result,
      meta_regression = meta_regression_result
    )
  }
  
  moderator_results[[response_var]] <- response_results
}

##############################################################################
# Save the moderator results
# Define output directory using 'here'
output_dir <- here::here("DATA", "OUTPUT_FROM_R", "SAVED_OBJECTS_FROM_R")
# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
# Define the file path for saving the results
output_file <- file.path(output_dir, "moderator_analysis_results.rds")
# Save the moderator results
saveRDS(moderator_results, file = output_file)
cat("Moderator analysis results saved to:", output_file, "\n")
```

##########################################################################################################################################
Check moderator analysis results
##########################################################################################################################################

Next Steps: Reporting
Once you have inspected the results, performed heterogeneity diagnostics, and completed sensitivity analyses, you can proceed to:
  
  Summarize your findings, including which moderators showed significant effects and the overall heterogeneity (I²).
Interpret the results in the context of the agroforestry studies and compare with the findings from the cabbage meta-analysis.
Consider any additional analyses or data adjustments based on the outcomes of the sensitivity checks.


1. Inspect the Output

```{r}
# Load the results if not already loaded
# moderator_results <- readRDS(file.path(output_dir, "moderator_analysis_results.rds"))

##########################################################################
# Set up the parallel processing plan
plan(multisession, workers = parallel::detectCores() - 1)
##################################################
# Start time tracking
start.time <- Sys.time()
##################################################
##################################################


# Quick overview of the structure
# str(moderator_results)

# Function to check which analyses were successful
check_results <- function(results) {
  successful_analyses <- list()
  
  for (response_var in names(results)) {
    cat("\nResponse Variable:", response_var, "\n")
    response_results <- results[[response_var]]
    
    for (moderator in names(response_results)) {
      cat("Moderator:", moderator, "\n")
      
      subgroup <- response_results[[moderator]]$subgroup
      meta_regression <- response_results[[moderator]]$meta_regression
      
      if (!is.null(subgroup)) {
        cat("  Subgroup Analysis: Successful\n")
        successful_analyses[[response_var]][[moderator]]$subgroup <- "Successful"
      } else {
        cat("  Subgroup Analysis: Failed\n")
      }
      
      if (!is.null(meta_regression)) {
        cat("  Meta-Regression: Successful\n")
        successful_analyses[[response_var]][[moderator]]$meta_regression <- "Successful"
      } else {
        cat("  Meta-Regression: Failed\n")
      }
    }
  }
  return(successful_analyses)
}

##################################################
# End time tracking
end.time <- Sys.time()
# Calculate time taken
time.taken <- end.time - start.time
time.taken
##############################################################
# Last go: (17/11-24)
# Time difference of 0.2157011 secs

##############################################################
# Check the results
successful_analyses <- check_results(moderator_results)
# print(successful_analyses)
```

```{r}
# Function to extract successful subgroup analysis
extract_successful <- function(results) {
  data_frame <- list()
  for (response in names(results)) {
    for (moderator in names(results[[response]])) {
      if (!is.null(results[[response]][[moderator]]$subgroup) &&
          results[[response]][[moderator]]$subgroup == "Successful") {
        data_frame <- append(
          data_frame,
          list(data.frame(response_variable = response, moderator = moderator, status = "Successful"))
        )
      }
    }
  }
  bind_rows(data_frame)
}

# Create a data frame of successful analyses
success_df <- extract_successful(successful_analyses)
```

```{r}
# Plot the heatmap
success_df |> 
  ggplot(aes(x = moderator, y = response_variable, fill = status)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Successful" = "#1b9e77", "Unsuccessful" = "#d95f02"), na.value = "grey80") +
  labs(
    title = "Summary of Successful Subgroup Analyses",
    x = "Moderator",
    y = "Response Variable",
    fill = "Analysis Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16)
  )
```
```{r}
# Count the number of successful analyses per response variable
success_count <- success_df %>%
  count(response_variable)

# Bar plot of successful analyses
success_count |> 
  ggplot(aes(x = reorder(response_variable, n), y = n, fill = response_variable)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Number of Successful Subgroup Analyses per Response Variable",
    x = "Response Variable",
    y = "Count of Successful Analyses"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

```


############
# STEP 4
##########################################################################################################################################
HETEROGENITY DIAGNOSTICS
##########################################################################################################################################

Heterogeneity Diagnostics and Sensitivity Analysis

```{r}
# Function to calculate I² statistic for meta-regression models
calculate_I2 <- function(model) {
  if (!is.null(model)) {
    tau2 <- model$tau2
    sigma2 <- model$sigma2
    I2 <- 100 * (tau2 / (tau2 + sigma2))
    return(round(I2, 2))
  } else {
    return(NA)
  }
}

# Iterate through results and calculate I²
I2_diagnostics <- list()

for (response_var in names(moderator_results)) {
  response_results <- moderator_results[[response_var]]
  
  I2_diagnostics[[response_var]] <- list()
  
  for (moderator in names(response_results)) {
    meta_regression <- response_results[[moderator]]$meta_regression
    
    if (!is.null(meta_regression)) {
      I2_stat <- calculate_I2(meta_regression)
      I2_diagnostics[[response_var]][[moderator]] <- I2_stat
      cat("I² for", response_var, "and", moderator, ":", I2_stat, "%\n")
    }
  }
}

# Print the heterogeneity diagnostics
I2_diagnostics
```

Many/all moderator analyses resulted in NaN values for the heterogeneity statistic (I²), which is often due to issues such as:
  
  Insufficient Data: When the sample size for certain subgroups is too low, the variance components cannot be estimated properly.
Zero Between-Study Variance (tau2): If there is no detected between-study variance, I² becomes zero, which might be misleading.
V_matrix Issues: Problems like non-positive definite matrices can affect the calculation of heterogeneity statistics.



############
# STEP 5
##########################################################################################################################################
ADDITIONAL MODEL COMPARISON AND DIAGNOSTICS
##########################################################################################################################################

Extract Model Fit Statistics

```{r}
# Helper function to extract model diagnostics
extract_model_diagnostics <- function(model) {
  if (!is.null(model)) {
    AIC_value <- AIC(model)
    BIC_value <- BIC(model)
    logLik_value <- logLik(model)
    k_all <- model$k.all  # Total number of studies included
    QM <- model$QM        # Omnibus test for moderators
    pval_QM <- model$pval.QM
    
    list(
      AIC = AIC_value,
      BIC = BIC_value,
      LogLik = logLik_value,
      k_all = k_all,
      QM = QM,
      pval_QM = pval_QM
    )
  } else {
    NULL
  }
}

# Extract diagnostics for each model
model_diagnostics <- lapply(moderator_results, function(response_result) {
  lapply(response_result, function(mod_result) {
    if (!is.null(mod_result$meta_regression)) {
      extract_model_diagnostics(mod_result$meta_regression)
    } else {
      NULL
    }
  })
})

# Print a summary of the model diagnostics
# print(model_diagnostics)


# Helper function to filter non-NULL diagnostics
filter_meaningful_diagnostics <- function(diagnostics) {
  meaningful_results <- list()
  
  for (response_var in names(diagnostics)) {
    response_results <- diagnostics[[response_var]]
    valid_results <- list()
    
    for (moderator in names(response_results)) {
      if (!is.null(response_results[[moderator]])) {
        valid_results[[moderator]] <- response_results[[moderator]]
      }
    }
    
    if (length(valid_results) > 0) {
      meaningful_results[[response_var]] <- valid_results
    }
  }
  
  return(meaningful_results)
}

# Apply the function to filter the model diagnostics
filtered_diagnostics <- filter_meaningful_diagnostics(model_diagnostics)

# Print the filtered diagnostics
print(filtered_diagnostics)

```
```{r}
# Convert the filtered diagnostics to a tidy data frame for visualization
extract_diagnostics_df <- function(filtered_diagnostics) {
  diagnostics_list <- list()
  
  for (response_var in names(filtered_diagnostics)) {
    for (moderator in names(filtered_diagnostics[[response_var]])) {
      diag <- filtered_diagnostics[[response_var]][[moderator]]
      if (!is.null(diag)) {
        diagnostics_list <- append(diagnostics_list, list(
          data.frame(
            Response = response_var,
            Moderator = moderator,
            AIC = diag$AIC,
            BIC = diag$BIC,
            LogLik = as.numeric(diag$LogLik),
            k_all = diag$k_all,
            QM = diag$QM
          )
        ))
      }
    }
  }
  
  return(do.call(rbind, diagnostics_list))
}

# Create a data frame for visualization
diagnostics_df <- extract_diagnostics_df(filtered_diagnostics)

# Plot AIC and BIC values
ggplot(diagnostics_df, aes(x = Moderator, y = AIC, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "AIC Values by Moderator and Response Variable",
       x = "Moderator", y = "AIC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot number of studies (k_all) for each analysis
ggplot(diagnostics_df, aes(x = Moderator, y = k_all, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Studies (k_all) by Moderator and Response Variable",
       x = "Moderator", y = "Number of Studies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

Assessing Diagnostics Across Response Variables

```{r}
# Function to extract diagnostics for both moderators and response variables
extract_diagnostics_df <- function(filtered_diagnostics) {
  diagnostics_list <- list()
  
  for (response_var in names(filtered_diagnostics)) {
    for (moderator in names(filtered_diagnostics[[response_var]])) {
      diag <- filtered_diagnostics[[response_var]][[moderator]]
      if (!is.null(diag) && !is.na(diag$AIC)) { # Filter out missing diagnostics
        diagnostics_list <- append(diagnostics_list, list(
          data.frame(
            Response = response_var,
            Moderator = moderator,
            AIC = diag$AIC,
            BIC = diag$BIC,
            LogLik = as.numeric(diag$LogLik),
            k_all = diag$k_all,
            QM = diag$QM,
            I2 = ifelse(!is.null(diag$I2), diag$I2, NA)
          )
        ))
      }
    }
  }
  
  return(do.call(rbind, diagnostics_list))
}

# Create a data frame for diagnostics
diagnostics_df <- extract_diagnostics_df(filtered_diagnostics)

# Check the structure of the data frame
str(diagnostics_df)

```

Plotting AIC and BIC by Response Variable

```{r}
# AIC values by Response and Moderator
ggplot(diagnostics_df, aes(x = Moderator, y = AIC, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "AIC Values by Moderator and Response Variable",
       x = "Moderator", y = "AIC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

Plotting Number of Studies (k_all)

```{r}
# Number of Studies (k_all) by Response and Moderator
ggplot(diagnostics_df, aes(x = Moderator, y = k_all, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Studies (k_all) by Moderator and Response Variable",
       x = "Moderator", y = "Number of Studies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

Plotting I² Heterogeneity Statistics

```{r}
# I² Statistics by Response and Moderator
ggplot(diagnostics_df, aes(x = Moderator, y = I2, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "I² Heterogeneity Statistics by Moderator and Response Variable",
       x = "Moderator", y = "I² (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

Plotting Log-Likelihood Values

```{r}
# Log-Likelihood by Response and Moderator
ggplot(diagnostics_df, aes(x = Moderator, y = LogLik, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Log-Likelihood by Moderator and Response Variable",
       x = "Moderator", y = "Log-Likelihood") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```





Check Fixed Effects Estimates

```{r}
# Function to extract fixed effects estimates
extract_fixed_effects <- function(model) {
  if (!is.null(model)) {
    coef_table <- summary(model)$b
    ci_lower <- summary(model)$ci.lb
    ci_upper <- summary(model)$ci.ub
    p_values <- summary(model)$pval
    
    data.frame(
      Estimate = coef_table,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      P_Value = p_values
    )
  } else {
    NULL
  }
}

# Extract fixed effects for each model
fixed_effects_summary <- lapply(moderator_results, function(response_result) {
  lapply(response_result, function(mod_result) {
    if (!is.null(mod_result$meta_regression)) {
      extract_fixed_effects(mod_result$meta_regression)
    } else {
      NULL
    }
  })
})

# Print the fixed effects estimates
print(fixed_effects_summary)
```



#############
# STEP 2
##########################################################################################################################################
RE-CALCULATE VARIANCE-CO-VARIANCE MATRICES
##########################################################################################################################################

Recalculate Variance-Covariance Matrices (V_matrix)

```{r}
# source("path/to/function/calculate_v_matrix.R")
calculate_v_matrix <- function(data, correlation = 0.5) {
  cat("Calculating variance-covariance matrix...\n")
  
  # Check if 'yi' and 'vi' columns exist
  if (!("yi" %in% colnames(data)) || !("vi" %in% colnames(data))) {
    stop("Data must contain 'yi' (effect size) and 'vi' (variance) columns.")
  }
  
  # Extract variances
  variances <- data$vi
  n <- length(variances)
  
  # Create a correlation matrix
  correlation_matrix <- matrix(correlation, nrow = n, ncol = n)
  diag(correlation_matrix) <- 1  # Set diagonal elements to 1
  
  # Calculate the variance-covariance matrix
  v_matrix <- outer(variances, variances) * correlation_matrix
  rownames(v_matrix) <- rownames(data)
  colnames(v_matrix) <- rownames(data)
  
  return(v_matrix)
}


# Function to calculate V_matrix with robust estimation
calculate_v_matrix_for_subset <- function(data, correlation = 0.5) {
  if (nrow(data) > 1) {
    cat("Calculating V_matrix for subset with robust estimation...\n")
    tryCatch({
      calculate_v_matrix(data, correlation = correlation)
    }, error = function(e) {
      cat("Error in V_matrix calculation:", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("Insufficient data points. Skipping V_matrix calculation.\n")
    NULL
  }
}
```

```{r}
# Generate V_matrices for each response subset
v_matrices <- list(
  v_matrix_non_imp = lapply(response_splits$non_imp, calculate_v_matrix_for_subset),
  v_matrix_imp = lapply(response_splits$imp, calculate_v_matrix_for_subset)
)

# Name the V_matrices by response variables
names(v_matrices$v_matrix_non_imp) <- response_variables
names(v_matrices$v_matrix_imp) <- response_variables
```


Checking v_matrices

```{r}
# Function to print a random sample of the variance-covariance matrix
print_sample_v_matrix <- function(v_matrices, n = 5) {
  # Loop through each response variable
  for (response_var in names(v_matrices)) {
    cat("\n*** Checking V_matrix for:", response_var, "***\n")
    
    # Get the V_matrix for the current response variable
    v_matrix <- v_matrices[[response_var]]
    
    # Check if the V_matrix exists and is not NULL
    if (is.null(v_matrix)) {
      cat("V_matrix is NULL. Skipping...\n")
      next
    }
    
    # Ensure that the matrix is large enough for sampling
    if (nrow(v_matrix) < n || ncol(v_matrix) < n) {
      cat("V_matrix is too small for sampling. Displaying full matrix instead:\n")
      print(v_matrix)
    } else {
      # Randomly select 'n' rows and columns to print a submatrix
      random_indices <- sample(1:nrow(v_matrix), n)
      sampled_v_matrix <- v_matrix[random_indices, random_indices]
      
      # Print the sampled submatrix
      cat("Sampled V_matrix (first", n, "rows and columns):\n")
      print(sampled_v_matrix)
    }
  }
}

# Example usage with your V_matrices
cat("\nNon-Imputed Dataset:\n")
print_sample_v_matrix(v_matrices$v_matrix_non_imp, n = 5)

cat("\nImputed Dataset:\n")
print_sample_v_matrix(v_matrices$v_matrix_imp, n = 5)
```

```{r}
# Function to check data quality
inspect_data <- function(data, moderator) {
  summary_stats <- data %>%
    group_by(!!sym(moderator)) %>%
    summarise(
      n = n(),
      unique_ids = n_distinct(id_article),
      var_yi = var(yi, na.rm = TRUE)
    )
  print(summary_stats)
}

# Inspect data for a specific response variable and moderator
inspect_data(response_splits$imp[["Biodiversity"]], "tree_type")
inspect_data(response_splits$imp[["Crop yield"]], "tree_type")
inspect_data(response_splits$imp[["Soil quality"]], "tree_type")
```

















To make the meta-analysis more robust and comparable to the approach taken in the cabbage meta-analysis, its important to consider the following key steps to  address study heterogeneity, incorporate important moderators, and validate your findings in a rigorous manner. 

Outline based on the cabbage meta-analysis methodology:
  Identify Key Moderators:
  Conduct Subgroup Analysis:
  Include Moderators in a Meta-Regression Model:
  Assess Model Fit and Heterogeneity:
  Check for Multicollinearity Among Moderators:
  Leave-One-Out Sensitivity Analysis:
  Publication Bias Assessment:
  Visualize the Results:
  
  Refined Plan for Moderator Analysis in the Meta-Analysis

Split the data by response variable (ecosystem service).
Recalculate the V_matrix for each subset, using robust variance estimation if needed.
Conduct moderator analysis (both subgroup analysis and meta-regression) within each ecosystem service subset.
Include heterogeneity diagnostics and sensitivity analysis (e.g., leave-one-out analysis).
Explore additional study-level covariates in the meta-regression models.


#############
# STEP 1
##########################################################################################################################################
SPLIT DATA BY RESPONSE VARIABLE
##########################################################################################################################################

Split by Response Variable (Ecosystem Service)

Split the data by response variable (ecosystem service).
Generate new variance-covariance matrices (V_matrix) for each subset.
Conduct moderator analysis within each ecosystem service subset, examining how each moderator affects the effect size.

Split Data by Response Variable

```{r}
# Define response variables (ecosystem services)
response_variables <- c("Biodiversity", 
                        "Crop yield", 
                        "Water quality", 
                        "Pest and Disease", 
                        "Soil quality", 
                        "Greenhouse gas emission", 
                        "Product quality"
)

# Define moderators
moderators <- c("tree_type", "crop_type", "age_system", "season", 
                "soil_texture", "no_tree_per_m", "tree_height", "alley_width")
```

```{r}
# Function to split dataset by response variable
split_data_by_response <- function(data, response_var) {
  cat("\nSplitting dataset by response variable:", response_var, "\n")
  subset(data, response_variable == response_var)
}

# Split non-imputed and imputed datasets by response variable
response_splits <- list(
  non_imp = lapply(response_variables, split_data_by_response, data = non_imp_dataset),
  imp = lapply(response_variables, split_data_by_response, data = imp_dataset)
)

# Name the lists by response variables
names(response_splits$non_imp) <- response_variables
names(response_splits$imp) <- response_variables

```

