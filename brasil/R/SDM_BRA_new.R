

library("Matrix")
library("spatialreg")
library("mvtnorm")

library(sf)
library(tidyverse)
library(viridis)
library(fineprintutils)
library(stringr)
library(zoo)
library(imputeTS)
library(data.table)

source("brasil/R/functions_sdm.R")


# select years, growth horizons and weight matrix for model
# t_vector <- c(2002:2016)
t_vector <- seq(2012, 2016, 1)
g_horizons <- c(2)
k_n <- c(5) 

# here a possible loop could start
interact <- FALSE
g_horizon <- 2
k_nn <- 5
drop_horizon <- c((max(t_vector)-g_horizon+1) : max(t_vector))

source("brasil/R/regression_data_BRA.R")

dat_mat <- cbind(Y, X[,-1])
t <- length(t_vector) - g_horizon
rm(C, D, geometries_mu, data, sf_panel, W_str, W_data, knear_nb)
gc()


results_mh <- sar_mh(
  x = dat_mat, 
  W = W_k, 
  LX = TRUE,
  n_draw = 1000L, n_burn = 0L, n_thin = 1L,
  tfe = TRUE, 
  ife = FALSE, 
  n_time = t,
  sigma_a = 0.01, sigma_b = 0.01,
  beta_mean = 0, beta_var = 10 ^ 8,
  rho_a = 1.01,
  type = "eigen",
  verbose = TRUE)

betas <- apply(results_mh$beta, 2, median)
plot(density(results_mh$sigma))
plot(density(results_mh$rho))
median(results_mh$rho)

start_time <- Sys.time()
test <- effects(results_mh)
Sys.time() - start_time


# results_gg <- sar_grid(
#   x = dat_mat, 
#   W = W_mat, 
#   LX = TRUE,
#   n_draw = 1000L, n_burn = 0L, n_thin = 1L,
#   tfe = TRUE, 
#   ife = FALSE, 
#   n_time = t,
#   sigma_a = 0.01, sigma_b = 0.01,
#   beta_mean = 0, beta_var = 10 ^ 8,
#   rho_a = 1.01,
#   grid = NULL,
#   verbose = TRUE)





