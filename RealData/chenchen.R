chen_chen <- function(Y, Xlab, flab, fall, Xall) {
  # Sample sizes
  n_labeled <- length(Y) # Number of labeled samples
  n_total <- length(fall) # Total number of samples (labeled + unlabeled)
  
  # Fit models
  beta_model <- lm(Y ~ ., data = cbind(Y, Xlab)) # Model for observed outcomes
  gamma_labeled_model <- lm(flab ~ ., data = cbind(flab, Xlab)) # Model for predictions in labeled set
  gamma_all_model <- lm(fall ~ ., data = cbind(fall, Xall)) # Model for predictions in all data
  
  # Extract coefficients
  beta_coeff <- coef(beta_model) # Coefficients for observed outcomes
  gamma_labeled_coeff <- coef(gamma_labeled_model) # Coefficients for predictions in labeled data
  gamma_all_coeff <- coef(gamma_all_model) # Coefficients for predictions in all data
  
  # Add intercept for labeled covariates
  Xlab_int <- cbind(1, as.matrix(Xlab))
  Xall_int <- cbind(1, as.matrix(Xall))
  
  # Compute derivative matrices
  D1 <- D2 <- -crossprod(Xall_int, Xall_int) / n_total
  
  # Compute score matrices
  score_beta <- Xlab_int * residuals(beta_model)
  score_gamma <- Xlab_int * residuals(gamma_labeled_model)
  
  # Covariance matrices
  C11 <- crossprod(score_beta, score_beta) / n_labeled
  C12 <- crossprod(score_beta, score_gamma) / n_labeled
  C22 <- crossprod(score_gamma, score_gamma) / n_labeled
  
  # Matrix inverses
  D1_inv <- solve(D1)
  C22_inv <- solve(C22)
  
  # Compute correction terms
  D1_C12 <- D1_inv %*% C12
  D1_C12_C22 <- D1_C12 %*% C22_inv
  
  # Calculate adjusted coefficient estimates
  theta_hat <- beta_coeff - D1_C12_C22 %*% D2 %*% (gamma_labeled_coeff - gamma_all_coeff)
  
  # Compute variance-covariance matrix for the adjusted estimates
  Omega <- (D1_inv %*% C11 %*% D1_inv - (1 - n_labeled / n_total) * D1_C12_C22 %*% t(C12) %*% D1_inv) / n_labeled
  
  # Standard errors for the adjusted estimates
  se_beta <- sqrt(diag(Omega))
  
  # Return results for the specific coefficient of interest
  df <- data.frame(Estimate = theta_hat, Std.Error = se_beta)
  
  return(df)
}
