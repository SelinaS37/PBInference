# Another Look at Inference After Prediction

This repository contains the code and results for the paper *"Another Look at Inference After Prediction"* by Jessica Gronsbell,  Jianhui Gao, Yaqi Shi, Zachary R. McCaw, and David Cheng. You can find the preprint [here](https://arxiv.org/abs/2411.19908).

## Overview


Our paper investigates the statistical efficiency of prediction-based (PB) inference methods. We analyze and compare several PB inference approaches, including the prediction-powered inference (PPI) method from [Angelopoulos et al. (2023)](https://www.science.org/doi/10.1126/science.adi6000) and the [Chen and Chen (2000)](https://www.jstor.org/stable/2680690) estimator with theoretical and numerical evaluations.

The repository includes:
- Implementation of PB inference methods discussed in the paper.
- Simulations and analyses used to generate the results in the paper.
- Code for reproduce our UK Biobank Analysis, but access to UK Biobank is required as the data cannot be released.

The simulation evaluates the performance of several methods implemented with the [`ipd`](https://ipd-tools.github.io/ipd/) package, including:

* Naive estimation (`naive_beta`)
* Traditional least squares estimation with the labeled data (`observed_beta`)
* Chen & Chen estimation (`chen-chen`)
* Post-prediction adaptive estimation (`pspa`)
* Prediction-powered inference (PPI) (`ppi`, `ppi_full`)

## Repository Structure

Within the Scripts folder:

* `run_simulation.R`: Script for running simulations
* `data_generation.R`: Contains functions for data generation
* `method_functions.R`: Contains functions for the methods 

## Requirements

Install the following R packages before running the code:

```r
install.packages(c("dplyr", "tidyr", "ipd"))
```


## Demonstration

Here is a simple demonstration:

```r
# Load necessary packages
library(dplyr)
library(tidyr)
library(ipd)

#Source files
source('data_generation.R')
source('method_functions.R')

# Simulation Parameters
sce <- c("1a")
n_train <- 100000
n_tot <- 10000
n_test <- 3700
n_val <- n_tot - n_test

# Model formula
formula <- y - pred ~ (x1)

# Set Seed
set.seed(2025)

# Prediction model:
pred_model <- data_gen_train(n_train, sce)

# Generate working dataset
sim_dat_tv <- data_gen_testval(n_tot, pred_model, sce)
sim_dat_tv$set <- rep(c("testing", "validation"), c(n_test, n_val))

# Implementation
method_dfs <- rbind(
  true_beta(sim_dat_tv, formula),
  naive_beta(sim_dat_tv, formula),
  observed_beta(sim_dat_tv, formula),
  chen_chen(sim_dat_tv, formula),
  ppi_full(sim_dat_tv, formula),
  ppi(sim_dat_tv, formula),
  pspa(sim_dat_tv, formula)
)


```
You should expect the following results by running the above code:

```r
# A tibble: 7 × 8
    sim Estimate Std.Error Lower.CI Upper.CI Method     term     R2
  <dbl>    <dbl>     <dbl>    <dbl>    <dbl> <chr>      <chr>  <dbl>
1     1     2.32     0.0223     2.27     2.36 true       x1     0.905
2     1     2.33     0.0199     2.29     2.37 naive      x1     0.905
3     1     2.34     0.0359     2.27     2.41 observed   x1     0.905
4     1     2.32     0.0255     2.27     2.37 chen-chen  x1     0.905
5     1     2.32     0.0255     2.27     2.37 ppi-full   x1     0.905
6     1     2.31     0.0298     2.25     2.37 ppi        x1     0.905
7     1     2.32     0.0257     2.27     2.37 pspa       x1     0.905

```


## Acknowledgments

This work builds on the work of [Angelopoulos et al. (2023)](https://www.science.org/doi/10.1126/science.adi6000), [Chen and Chen (2000)](https://www.jstor.org/stable/2680690), and [Motwani and Witten (2023)](https://www.jmlr.org/papers/volume24/23-0896/23-0896.pdf), with implementation support from the [ipd](https://ipd-tools.github.io/ipd/) package.

## Contact

For questions, please contact [Yaqi Shi](mailto:yaqi.shi@mail.utoronto.com) or open an issue on this repository.


