
# ---------------------------- Method Functions ------------------------------ #

# ----------- Helper functions  ----------- #

expit <- function(x) {
  return(1 / (1 + exp(-x)))
}



expit_derivative <- function(x) {
  p <- expit(x)
  return(p * (1 - p))
}



# ----------- Method Functions ----------- #

# Method 1 - Angelopoulos et al. - PPI
ppi <- function(sim_dat_tv, formula) {
  
  # Prepare the data based on ipd package
  sim_dat_tv <- sim_dat_tv %>%
    mutate(set_label = case_when(
      set == "testing" ~ "labeled",
      set == "validation" ~ "unlabeled",
      TRUE ~ set
    ))
  
  # Run PPI from ipd
  fit <- ipd::ipd(
    formula = formula,
    method = "ppi",
    model = "ols",
    data = sim_dat_tv,
    label = "set_label"
  )
  
  # Extract the estimate and standard error
  tidy_fit <- broom::tidy(fit)
  
  # Drop the intercept
  tidy_fit <- tidy_fit %>% filter(term != "(Intercept)")
  
  # Construct output data frame
  df <- tidy_fit %>%
    mutate(
      Method = "ppi"
    ) %>%
    select(Estimate = estimate, Std.Error = std.error,
           Lower.CI = conf.low, Upper.CI = conf.high,
           Method, term)
  
  return(df)
}



# Method 2 - PPI with full data
ppi_full <- function(sim_dat_tv, formula){
  
  # Prepare the data based on ipd package but adjust for full data
  sim_dat_tv_lab <- sim_dat_tv %>%
    filter(set == "testing") %>%
    mutate(set_label = "labeled")
  
  sim_dat_tv_whole <- sim_dat_tv %>%
    mutate(set_label = "unlabeled")
  
  sim_dat_tv_new <- rbind(
    sim_dat_tv_lab,
    sim_dat_tv_whole
  )
  
  # Run PPI from ipd
  fit <- ipd::ipd(
    formula = formula,
    method = "ppi",
    model = "ols",
    data = sim_dat_tv_new,
    label = "set_label"
  )
  
  # Extract the estimate and standard error
  tidy_fit <- broom::tidy(fit)
  
  # Drop the intercept
  tidy_fit <- tidy_fit %>% filter(term != "(Intercept)")
  
  # Construct output data frame
  df <- tidy_fit %>%
    mutate(
      Method = "ppi-full"
    ) %>%
    select(Estimate = estimate, Std.Error = std.error,
           Lower.CI = conf.low, Upper.CI = conf.high,
           Method, term)
  
  return(df)
}



# Method 3: true beta
true_beta <- function(sim_dat_tv, formula) {
  
  # Establish the formula for true beta
  true_formula <- as.formula(call("~", as.name("y"), formula[[3]]))
  
  # Fit linear regression
  fit <- lm(true_formula, data = sim_dat_tv)
  
  # Construct the output data
  tidy_fit <- broom::tidy(fit) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Method = "true",
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    ) %>%
    select(Estimate = estimate, Std.Error = std.error,
           Lower.CI = conf.low, Upper.CI = conf.high,
           Method, term)
  
  return(tidy_fit)
}



# Method 4: naive beta
naive_beta <- function(sim_dat_tv, formula) {
  
  # Establish the formula for naive beta
  naive_formula <- as.formula(call("~", as.name("pred"), formula[[3]]))
  
  # Fit linear regression
  fit <- lm(naive_formula, data = sim_dat_tv)
  
  # Construct the output data
  tidy_fit <- broom::tidy(fit) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Method = "naive",
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    ) %>%
    select(Estimate = estimate, Std.Error = std.error,
           Lower.CI = conf.low, Upper.CI = conf.high,
           Method, term)
  
  return(tidy_fit)
}



# Method 5: observed beta
observed_beta <- function(sim_dat_tv, formula) {
  
  # Establish the formula for observed beta
  observed_formula <- as.formula(call("~", as.name("y"), formula[[3]]))
  
  # Use only the labelled set
  sim_dat_tv_lab <- filter(sim_dat_tv, set == "testing")
  
  # Fit linear regression
  fit <- lm(observed_formula, data = sim_dat_tv_lab)
  
  # Construct the output data
  tidy_fit <- broom::tidy(fit) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Method = "observed",
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    ) %>%
    select(Estimate = estimate, Std.Error = std.error,
           Lower.CI = conf.low, Upper.CI = conf.high,
           Method, term)
  
  return(tidy_fit)
}



# Method 6: Chen chen beta
chen_chen <- function(sim_dat_tv, formula, family = "gaussian") {
  
  # Extract response and covariates
  response <- all.vars(formula[[2]])[1]
  rhs_vars <- all.vars(formula[[3]])
  
  # Subset data
  labeled_data <- filter(sim_dat_tv, set == "testing")
  unlabeled_data <- filter(sim_dat_tv, set == "validation")
  all_data <- sim_dat_tv
  
  n <- nrow(labeled_data)
  N <- nrow(all_data)
  
  X_lab <- as.matrix(labeled_data[, rhs_vars])
  X_all <- as.matrix(all_data[, rhs_vars])
  Y_lab <- labeled_data[[response]]
  pred_lab <- labeled_data$pred
  pred_all <- all_data$pred
  
  X_lab_int <- cbind(1, X_lab)
  X_all_int <- cbind(1, X_all)
  
  # Model 1: Y ~ X using specified family 
  model_beta_lab <- glm(Y_lab ~ X_lab, family = family)
  
  # Residuals and D1 depending on family
  if (family == "gaussian") {
    
    residuals_beta <- residuals(model_beta_lab)
    D1 <- -crossprod(X_lab_int) / n
    
  } else if (family == "binomial") {
    
    eta_hat <- X_lab_int %*% beta_lab
    mu_hat <- expit(eta_hat)
    residuals_beta <- Y_lab - mu_hat
    expit_deriv <- expit_derivative(eta_hat)
    D1 <- -t(X_lab_int) %*% (X_lab_int * expit_deriv) / n
    
  } else {
    
    stop("Only gaussian and binomial families are supported.")
    
  }
  
  
  # Models 2 & 3: pred ~ X (continuous)
  model_gamma_lab <- lm(pred_lab ~ X_lab)
  model_gamma_all <- lm(pred_all ~ X_all)
  
  beta_lab <- coef(model_beta_lab)
  gamma_lab <- coef(model_gamma_lab)
  gamma_all <- coef(model_gamma_all)
  
  # Sandwich pieces
  D2 <- D1
  
  S2 <- X_lab_int * residuals_beta
  S_tilde2 <- X_lab_int * residuals(model_gamma_lab)
  
  C11 <- crossprod(S2) / n
  C12 <- crossprod(S2, S_tilde2) / n
  C22 <- crossprod(S_tilde2) / n
  
  D1_inv <- solve(D1)
  C22_inv <- solve(C22)
  
  D1C12 <- D1_inv %*% C12
  D1C12C22 <- D1C12 %*% C22_inv
  
  theta_hat_cc <- as.vector(beta_lab - D1C12C22 %*% D2 %*% (gamma_lab - gamma_all))
  
  Omega <- (D1_inv %*% C11 %*% D1_inv -
              (1 - n / N) * D1C12C22 %*% t(D1C12)) / n
  
  se_beta_bar <- sqrt(diag(Omega))
  
  # Get the scores
  
  # Output
  terms <- rhs_vars
  df <- data.frame(
    Estimate = theta_hat_cc[-1],
    Std.Error = se_beta_bar[-1],
    Method = "chen-chen",
    term = terms
  ) %>%
    mutate(
      Lower.CI = Estimate - 1.96 * Std.Error,
      Upper.CI = Estimate + 1.96 * Std.Error
    ) %>%
    select(Estimate, Std.Error, Lower.CI, Upper.CI, Method, term)
    
  return(df)
}




# Method 7: PSPA 
pspa <- function(sim_dat_tv, formula){
  
  # Prepare the data based on ipd package
  sim_dat_tv <- sim_dat_tv %>%
    mutate(set_label = case_when(
      set == "testing" ~ "labeled",
      set == "validation" ~ "unlabeled",
      TRUE ~ set
    ))
  
  # Run PSPA from ipd
  fit <- ipd::ipd(
    formula = formula,
    method = "pspa",
    model = "ols",
    data = sim_dat_tv,
    label = "set_label"
  )
  
  # Extract the estimate and standard error
  tidy_fit <- broom::tidy(fit)
  
  # Drop the intercept
  tidy_fit <- tidy_fit %>% filter(term != "(Intercept)")
  
  # Construct output data frame
  df <- tidy_fit %>%
    mutate(
      Method = "pspa"
    ) %>%
    select(Estimate = estimate, Std.Error = std.error,
           Lower.CI = conf.low, Upper.CI = conf.high,
           Method, term)
  
  return(df)
}


