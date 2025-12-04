#' Analyze NGO Funding with Multiple Regression
#'
#' Performs comprehensive multiple linear regression analysis to identify
#' predictors of funding amount. Includes diagnostics (LINE conditions),
#' multicollinearity detection (VIF), and influential point removal.
#'
#' @param data A dataset containing NGO funding data
#' @param response Character string specifying the outcome variable (default: "amount")
#' @param predictors Character vector of predictor variable names
#' @param vif_threshold Numeric threshold for VIF to detect multicollinearity (default: 5)#to deal with multicolinearity
#' @param cooks_threshold Numeric threshold for Cook's distance (default:1) #to remove influential points
#' @param remove_influential Logical, whether to remove influential points (default: TRUE)
#' @param autocorr_years Numeric, it gives the user the option to decide how many years of data they have to check for autocorrelation in independence test
#' @param alpha Significance level for tests (default: 0.05)
#'
#' @return A list containing:
#'   \item{final_model}{The fitted lm object}
#'   \item{initial_model}{The initial model before any removals}
#'   \item{removed_predictors}{Character vector of predictors removed due to multicollinearity}
#'   \item{influential_points}{Indices of influential observations removed}
#'   \item{diagnostics}{Results of LINE condition checks}
#'   \item{vif}{Final variance inflation factors}
#'   \item{summary}{Model statistics}
#' @export
#' @importFrom car vif durbinWatsonTest
#' @importFrom lmtest bptest raintest
#' @importFrom MASS boxcox #for Box-Cox transformation if needed to transform it into nearly normal/normal distribution
analyze_funding_linear <- function(data,
                                   outcome = "amount",
                                   predictors,
                                   vif_threshold = 5,
                                   cooks_threshold = 1,
                                   remove_influential = TRUE,
                                   alpha = 0.05,
                                   autocorr_years) {

  if (!outcome %in% names(data)) {
    stop(paste("Outcome variable", outcome, "not found in data"))
  }
  if (!all(predictors %in% names(data))) {
    missing <- predictors[!predictors %in% names(data)]
    stop(paste("Predictor(s) not found:", paste(missing, collapse = ", ")))
  }

  results <- list()
  removed_predictors_vif <- c()
  influential_indices <- integer(0)

  # Create working copy of data
  data_work <- data
  predictors_work <- predictors
  #using cat for custom formatting
  cat("MULTIPLE LINEAR REGRESSION ANALYSIS")
  cat("Outcome:", outcome, "\n")
  cat("Initial Predictors:", paste(predictors, collapse = ", "), "\n\n")

  formula_str <- paste(outcome, "~", paste(predictors_work, collapse = " + "))
  model_formula <- as.formula(formula_str)
  model_initial <- lm(model_formula, data = data_work)
  results$initial_model <- model_initial

  cat("--- Step 1: Initial Model Fitted ---\n")
  cat("Observations:", nrow(data_work), "\n")
  cat("Predictors:", length(predictors_work), "\n\n")

  cat("--- Step 2: Multicollinearity Check (VIF) ---\n")

  vif_values <- car::vif(model_initial)
  cat("Initial VIF values:\n")
  print(round(vif_values, 3))

  while (any(vif_values > vif_threshold)) { #we want to remove high multicollinearity predictors and keep the moderate one
    high_vif_var <- names(which.max(vif_values))
    cat("\nRemoving", high_vif_var, "- VIF:", round(max(vif_values), 3), "\n")
    removed_predictors_vif <- c(removed_predictors_vif, high_vif_var)

    predictors_work <- predictors_work[predictors_work != high_vif_var]
    formula_str <- paste(outcome, "~", paste(predictors_work, collapse = " + "))
    model_formula <- as.formula(formula_str)
    model_initial <- lm(model_formula, data = data_work)
    vif_values <- car::vif(model_initial)
  }

  if (length(removed_predictors_vif) == 0) {
    cat("\nNo multicollinearity detected (all VIF <", vif_threshold, ")\n\n")
  } else {
    cat("\nRemoved predictors:", paste(removed_predictors_vif, collapse = ", "), "\n")
    cat("Final VIF values:\n")
    print(round(vif_values, 3))
    cat("\n")
  }

  results$removed_predictors <- removed_predictors_vif
  results$vif <- vif_values


    if (remove_influential) {
    cat("--- Step 3: Influential Points Detection ---\n")

    cooksd <- cooks.distance(model_initial)
    if (is.null(cooks_threshold)) {
      cooks_threshold <- 4 / nrow(data_work)
    }

    influential_indices <- which(cooksd > cooks_threshold)

    if (length(influential_indices) > 0) {
      cat("Influential points detected:", length(influential_indices), "\n")
      cat("Indices:", paste(head(influential_indices, 10), collapse = ", "))
      if (length(influential_indices) > 10) cat(", ...")
      cat("\n")
      cat("Cook's Distance threshold:", round(cooks_threshold, 6), "\n")
      data_work <- data_work[-influential_indices, ]
      model_final <- lm(model_formula, data = data_work)
      cat("Observations after removal:", nrow(data_work), "\n\n")
    } else {
      cat("No influential points detected.\n\n")
      model_final <- model_initial
    }
  } else {
    model_final <- model_initial
  }

  results$influential_points <- influential_indices
  results$final_model <- model_final

  cat("--- Step 4: Diagnostic Checks (LINE Conditions) ---\n\n")

  diagnostics <- list()
  cat("1. LINEARITY (Rainbow Test):\n")
  rainbow_test <- lmtest::raintest(model_final)
  diagnostics$linearity <- list(
    p_value = rainbow_test$p.value,
    passed = rainbow_test$p.value > alpha
  )
  cat("   p-value:", round(rainbow_test$p.value, 4),
      ifelse(rainbow_test$p.value > alpha, "✓ PASS\n", "✗ FAIL\n"))

  # 2. Independence
  # Given it is a time series data we can use Breusch-Godfrey test
  #Not using the durbin watson test because the time series data might have higher autocorrelation lags
  #Some concepts for me to go over again :
  # What is autocorrelation/Serial/Series (-1 to 1) ?  uses the same time series twice: once in its original form and once lagged one or more time periods. It is majorly used for assessing time series and its past values
  #Suppose it is a data that is a weeks' sales, We want to see how much we gained /lost in a sep column so we subtract
  #lag k autocorrelation is the correlation between values that are k time periods apart.
  #Lag 1 autocorrelation : correlation between each value and the one immediately before it.
  #Lag 2 : 2 time period and so on
  #Given the grant datas are
  cat("2. INDEPENDENCE (Breusch-Godfrey Test):\n")


  bg_test <- lmtest::bgtest(model_final, order =  autocorr_years) #order=1 for lag 1 autocorrelation
  diagnostics$independence <- list(
    p_value = bg_test$p.value,
    passed = bg_test$p.value > alpha
  )
  cat("   p-value:", round(bg_test$p.value, 4),
      ifelse(bg_test$p.value > alpha, "✓ PASS\n", "✗ FAIL\n"))

  # 3. Normality of Residuals
  # The null hypothesis is here to see if the residuals are normally distributed
  cat("3. NORMALITY (Shapiro-Wilk Test):\n")
  # If larger dataset
  if (nrow(data_work) < 5000) { #because it is sensitive to larger size
    shapiro_test <- shapiro.test(residuals(model_final))
    diagnostics$normality <- list(
      p_value = shapiro_test$p.value,
      passed = shapiro_test$p.value > alpha
    )
    cat("   p-value:", round(shapiro_test$p.value, 4),
        ifelse(shapiro_test$p.value > alpha, "✓ PASS\n", "✗ FAIL\n"))
  }

  #Visualize it as well

  ggqqplot <- ggplot2::ggplot(data = data.frame(residuals = residuals(model_final)),
                           ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Q-Q Plot of Residuals") +
    ggplot2::theme_minimal()


  # 4. Equal Variance (Homoscedasticity)
  cat("4. EQUAL VARIANCE (Breusch-Pagan Test):\n")
  bp_test <- lmtest::bptest(model_final)
  diagnostics$equal_variance <- list(
    p_value = bp_test$p.value,
    passed = bp_test$p.value > alpha
  )
  cat("   p-value:", round(bp_test$p.value, 4),
      ifelse(bp_test$p.value > alpha, "✓ PASS\n", "✗ FAIL\n"))

  results$diagnostics <- diagnostics

  # --- STEP 5: MODEL SUMMARY ---
  cat("\n--- Step 5: Final Model Summary ---\n\n")
  model_summary <- summary(model_final)
  print(model_summary)

  cat("\nModel Performance:\n")
  cat("R-squared:", round(model_summary$r.squared, 4), "\n")
  cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
  cat("RMSE:", round(sqrt(mean(residuals(model_final)^2)), 4), "\n")

  results$summary <- model_summary

  # Significant predictors
  coef_table <- model_summary$coefficients
  sig_predictors <- rownames(coef_table)[coef_table[, "Pr(>|t|)"] < alpha &
                                           rownames(coef_table) != "(Intercept)"]

  if (length(sig_predictors) > 0) {
    cat("\nSignificant predictors (p <", alpha, "):\n")
    for (pred in sig_predictors) {
      p_val <- coef_table[pred, "Pr(>|t|)"]
      coef_val <- coef_table[pred, "Estimate"]
      cat("  -", pred, ": β =", round(coef_val, 4), ", p =", format.pval(p_val), "\n")
    }
  } else {
    cat("\nNo significant predictors at α =", alpha, "\n")
  }

  class(results) <- c("ngo_linear_analysis", "list")
  return(results)
}


#' Analyze NGO Funding Decision with Multiple Logistic Regression
#'
#' Performs comprehensive logistic regression analysis to identify predictors
#' of funding decision (funded vs not funded). Includes multicollinearity detection,
#' influential point removal, and model performance metrics.
#'
#' @param data A data frame containing NGO funding data
#' @param outcome Character string specifying binary outcome variable (default: "funded")
#' @param predictors Character vector of predictor variable names
#' @param vif_threshold Numeric threshold for VIF to detect multicollinearity (default: 5)
#' @param cooks_threshold Numeric threshold for Cook's distance (default: 4/n)
#' @param remove_influential Logical, whether to remove influential points (default: TRUE)
#' @param alpha Significance level for tests (default: 0.05)
#'
#' @return A list containing:
#'   \item{final_model}{The fitted glm object after removing problematic observations}
#'   \item{initial_model}{The initial glm object before any removals}
#'   \item{removed_predictors}{Character vector of predictors removed due to multicollinearity}
#'   \item{influential_points}{Indices of influential observations removed}
#'   \item{vif}{Final variance inflation factors}
#'   \item{performance}{Model performance metrics (accuracy, AUC, confusion matrix)}
#'   \item{summary}{Model summary statistics}
#'
#' @export
#' @importFrom car vif
#' @importFrom pROC roc auc
analyze_funding_logistic <- function(data,
                                     outcome = "funded",
                                     predictors,
                                     vif_threshold = 5,
                                     cooks_threshold = 1,
                                     remove_influential = TRUE,
                                     alpha = 0.05) {

  # Validate inputs
  if (!outcome %in% names(data)) {
    stop(paste("Outcome variable", outcome, "not found in data"))
  }
  if (!all(predictors %in% names(data))) {
    missing <- predictors[!predictors %in% names(data)]
    stop(paste("Predictor(s) not found:", paste(missing, collapse = ", ")))
  }

  # Ensure outcome is binary
  if (length(unique(data[[outcome]])) != 2) {
    stop("Outcome variable must be binary (2 levels)")
  }
  if (is.factor(data[[outcome]])) {
    data[[outcome]] <- as.numeric(data[[outcome]]) - 1
  }

  # Initialize results
  results <- list()
  removed_predictors_vif <- c()
  influential_indices <- integer(0)

  # Working copy
  data_work <- data
  predictors_work <- predictors

  cat("=== MULTIPLE LOGISTIC REGRESSION ANALYSIS ===\n")
  cat("Outcome:", outcome, "(Binary)\n")
  cat("Initial Predictors:", paste(predictors, collapse = ", "), "\n\n")

  formula_str <- paste(outcome, "~", paste(predictors_work, collapse = " + "))
  model_formula <- as.formula(formula_str)
  model_initial <- glm(model_formula, data = data_work, family = binomial(link = "logit"))
  results$initial_model <- model_initial

  cat("--- Step 1: Initial Model Fitted ---\n")
  cat("Observations:", nrow(data_work), "\n")
  cat("Predictors:", length(predictors_work), "\n\n")

  cat("--- Step 2: Multicollinearity Check (VIF) ---\n")

  vif_values <- car::vif(model_initial)
  cat("Initial VIF values:\n")
  print(round(vif_values, 3))

  while (any(vif_values > vif_threshold)) {
    high_vif_var <- names(which.max(vif_values))
    cat("\nRemoving", high_vif_var, "- VIF:", round(max(vif_values), 3), "\n")
    removed_predictors_vif <- c(removed_predictors_vif, high_vif_var)

    predictors_work <- predictors_work[predictors_work != high_vif_var]
    formula_str <- paste(outcome, "~", paste(predictors_work, collapse = " + "))
    model_formula <- as.formula(formula_str)
    model_initial <- glm(model_formula, data = data_work, family = binomial(link = "logit"))
    vif_values <- car::vif(model_initial)
  }

  if (length(removed_predictors_vif) == 0) {
    cat("\nNo multicollinearity detected (all VIF <", vif_threshold, ")\n\n")
  } else {
    cat("\nRemoved predictors:", paste(removed_predictors_vif, collapse = ", "), "\n")
    cat("Final VIF values:\n")
    print(round(vif_values, 3))
    cat("\n")
  }

  results$removed_predictors <- removed_predictors_vif
  results$vif <- vif_values

  if (remove_influential) {
    cat("--- Step 3: Influential Points Detection ---\n")

    # Calculate Cook's distance for logistic regression
    cooksd <- cooks.distance(model_initial)
    if (is.null(cooks_threshold)) {
      cooks_threshold <- 4 / nrow(data_work)
    }

    influential_indices <- which(cooksd > cooks_threshold)

    if (length(influential_indices) > 0) {
      cat("Influential points detected:", length(influential_indices), "\n")
      cat("Indices:", paste(head(influential_indices, 10), collapse = ", "))
      if (length(influential_indices) > 10) cat(", ...")
      cat("\n")
      cat("Cook's Distance threshold:", round(cooks_threshold, 6), "\n")

      # Remove influential points
      data_work <- data_work[-influential_indices, ]

      # Refit model
      model_final <- glm(model_formula, data = data_work, family = binomial(link = "logit"))

      cat("Observations after removal:", nrow(data_work), "\n\n")
    } else {
      cat("No influential points detected.\n\n")
      model_final <- model_initial
    }
  } else {
    model_final <- model_initial
  }

  results$influential_points <- influential_indices
  results$final_model <- model_final

  cat("--- Step 4: Final Model Summary ---\n\n")
  model_summary <- summary(model_final)
  print(model_summary)

  results$summary <- model_summary

  coef_table <- model_summary$coefficients
  sig_predictors <- rownames(coef_table)[coef_table[, "Pr(>|z|)"] < alpha &
                                           rownames(coef_table) != "(Intercept)"]

  cat("\n")
  if (length(sig_predictors) > 0) {
    cat("Significant predictors (p <", alpha, "):\n")
    for (pred in sig_predictors) {
      p_val <- coef_table[pred, "Pr(>|z|)"]
      coef_val <- coef_table[pred, "Estimate"]
      or_val <- exp(coef_val)
      cat("  -", pred, ": β =", round(coef_val, 4),
          ", OR =", round(or_val, 4), ", p =", format.pval(p_val), "\n")
    }
  } else {
    cat("No significant predictors at α =", alpha, "\n")
  }

  cat("\n--- Step 5: Model Performance Metrics ---\n\n")


  pred_prob <- predict(model_final, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  actual <- data_work[[outcome]]


  conf_matrix <- table(Predicted = pred_class, Actual = actual)
  cat("Confusion Matrix:\n")
  print(conf_matrix)

  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  cat("\nAccuracy:", round(accuracy, 4), "\n")

  if (all(c(0, 1) %in% actual) && all(c(0, 1) %in% pred_class)) {
    sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
    specificity <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
    cat("Sensitivity (True Positive Rate):", round(sensitivity, 4), "\n")
    cat("Specificity (True Negative Rate):", round(specificity, 4), "\n")
  }

  if (requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(actual, pred_prob, quiet = TRUE)
    auc_val <- pROC::auc(roc_obj)
    cat("AUC-ROC:", round(auc_val, 4), "\n")

    results$performance <- list(
      confusion_matrix = conf_matrix,
      accuracy = accuracy,
      auc = as.numeric(auc_val),
      roc = roc_obj
    )
  } else {
    results$performance <- list(
      confusion_matrix = conf_matrix,
      accuracy = accuracy
    )
  }
  null_deviance <- model_final$null.deviance
  residual_deviance <- model_final$deviance
  mcfadden_r2 <- 1 - (residual_deviance / null_deviance)
  cat("McFadden's Pseudo R²:", round(mcfadden_r2, 4), "\n")

  class(results) <- c("ngo_logistic_analysis", "list")
  return(results)
}


#' Generate NGO Regression Report
#'
#' Generates a comprehensive report including both linear and logistic regression
#' analyses for NGO funding data.
#'
#' @param data A data frame containing NGO funding data
#' @param amount_var Character string for the funding amount variable (default: "amount")
#' @param funded_var Character string for the binary funded variable (default: "funded")
#' @param predictors Character vector of predictor variable names
#' @param vif_threshold Numeric threshold for VIF (default: 5)
#' @param alpha Significance level (default: 0.05)
#'
#' @return A list containing both linear and logistic regression results
#'
#' @export
generate_ngo_report <- function(data,
                                amount_var = "amount",
                                funded_var = "funded",
                                predictors,
                                vif_threshold = 5,
                                alpha = 0.05) {

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║           NGO FUNDING ANALYSIS - FULL REPORT              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")

  # Part 1: Linear Regression (Amount)
  cat("\n")
  cat("┌────────────────────────────────────────────────────────────┐\n")
  cat("│  ANALYSIS 1: What Predicts Funding AMOUNT?                │\n")
  cat("│  (Multiple Linear Regression on Funded NGOs)              │\n")
  cat("└────────────────────────────────────────────────────────────┘\n")
  cat("\n")

  # Filter to only funded organizations
  data_funded <- data[data[[funded_var]] == 1 |
                        (is.factor(data[[funded_var]]) &&
                           as.character(data[[funded_var]]) == levels(data[[funded_var]])[2]), ]

  linear_results <- analyze_funding_linear(
    data = data_funded,
    outcome = amount_var,
    predictors = predictors,
    vif_threshold = vif_threshold,
    alpha = alpha
  )

  cat("\n\n")
  cat("┌────────────────────────────────────────────────────────────┐\n")
  cat("│  ANALYSIS 2: What Predicts Funding DECISION?              │\n")
  cat("│  (Multiple Logistic Regression - Funded vs Not Funded)    │\n")
  cat("└────────────────────────────────────────────────────────────┘\n")
  cat("\n")

  logistic_results <- analyze_funding_logistic(
    data = data,
    outcome = funded_var,
    predictors = predictors,
    vif_threshold = vif_threshold,
    alpha = alpha
  )

  # Summary
  cat("\n\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║                    REPORT SUMMARY                          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")

  cat("LINEAR REGRESSION (Amount):\n")
  cat("  - Observations analyzed:", nrow(data_funded) - length(linear_results$influential_points), "\n")
  cat("  - Predictors removed (multicollinearity):", length(linear_results$removed_predictors), "\n")
  cat("  - Influential points removed:", length(linear_results$influential_points), "\n")
  cat("  - Final R²:", round(linear_results$summary$r.squared, 4), "\n")

  cat("\nLOGISTIC REGRESSION (Funded/Not Funded):\n")
  cat("  - Observations analyzed:", nrow(data) - length(logistic_results$influential_points), "\n")
  cat("  - Predictors removed (multicollinearity):", length(logistic_results$removed_predictors), "\n")
  cat("  - Influential points removed:", length(logistic_results$influential_points), "\n")
  cat("  - Model accuracy:", round(logistic_results$performance$accuracy, 4), "\n")
  if (!is.null(logistic_results$performance$auc)) {
    cat("  - AUC-ROC:", round(logistic_results$performance$auc, 4), "\n")
  }

  cat("\n")

  # Return both results
  report <- list(
    linear = linear_results,
    logistic = logistic_results,
    metadata = list(
      total_observations = nrow(data),
      funded_observations = nrow(data_funded),
      original_predictors = predictors,
      vif_threshold = vif_threshold,
      alpha = alpha
    )
  )

  class(report) <- c("ngo_full_report", "list")
  return(report)
}


