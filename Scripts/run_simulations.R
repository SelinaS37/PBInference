
# ------------------- Load Packages and Source Files --------------------------#

# Load necessary packages
library(dplyr)
library(tidyr)
library(ipd)

#Source files
source('data_generation.R')
source('method_functions.R')

# Simulation Parameters
scnes <- c("1a")
n_sim <- 10

# Data setting
n_train <- 100000
n_tot <- 10000
n_tests <- c(300, 600, 1000, 2000)

# Model formula
formula <- y - pred ~ (x1)


# --------------------------- Run Simulations ----------------------------------#

# Set Seed
set.seed(2025)

# Run Simulations
for (i in 1:length(scnes)) {
  
  # Scenarios
  sce <- scnes[i]
  
  # Generate one prediction model per scenario
  pred_model <- data_gen_train(n_train, sce)
  
  # Simulations
  sim_function_new <- function(k) {
    
    # Generate dataset without labels
    sim_dat_tv_orig <- data_gen_testval(n_tot, pred_model, sce)
    
    # Placeholder for results
    runresults <- c()
    
    # Change proportion for each
    for (j in n_tests) {
      
      # Determine the labelled proportion
      n_test <- j
      n_val <- n_tot - n_test
      sim_dat_tv <- sim_dat_tv_orig
      sim_dat_tv$set <- rep(c("testing", "validation"), c(n_test, n_val))
      
      # Run all different methods
      method_dfs <- rbind(
        true_beta(sim_dat_tv, formula),
        naive_beta(sim_dat_tv, formula),
        observed_beta(sim_dat_tv, formula),
        chen_chen(sim_dat_tv, formula),
        ppi(sim_dat_tv, formula),
        ppi_full(sim_dat_tv, formula),
        pspa(sim_dat_tv, formula)
      )
      
      # Additional info about the data
      method_dfs$R2 <- mean(sim_dat_tv$R2)
      method_dfs$n_test <- n_test
      method_dfs$n_val <- n_val
      method_dfs$sim <- k
      
      # Save results
      runresults <- rbind(runresults, method_dfs)
    }
    
    return(runresults)
    
  }
  
  # Parallel simulations
  results <- lapply(1:n_sim, sim_function_new)
  
  # Combine all results together
  combined_results <- do.call(rbind, results)
  
  # Add calculated results
  final_result <- combined_results %>%
    mutate(
      n_train = n_train,
      sce = sce,
      z_stat = Estimate / Std.Error,
      p_value = 2 * (1 - pnorm(abs(z_stat))),
      power = as.integer(p_value < 0.05),
      cilength = Upper.CI - Lower.CI
    ) 
  
  # Store results
  if (i == 1) {dir.create("results")}
  file <- paste0("results/main_sim_scenario_", sce, ".rds")
  saveRDS(final_result, file)
}




