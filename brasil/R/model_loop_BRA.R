
source("R/functions.R")

# select years and countries for panel
# t_vector <- c(2002:2016) # too big weights matrix -> cloud
t_vector <- c(2012:2016)
g_horizons <- c(2, 5)
c_vector1 <- c("Brazil")
c_vector2 <- c("BRA")
k_n <- c(5) 

store <- list()
model <- c(TRUE, FALSE) # interaction TRUE or FALSE, or both


interact <- FALSE
g_horizon <- 2
k_nn <- 5

for(interact in model){
  
  for (g_horizon in g_horizons){
    
    if(interact == TRUE){
      store[[as.character(paste0(g_horizon, "int"))]] <- list()
    } else {
      store[[as.character(paste0(g_horizon, "full"))]] <- list()
    }
    
    for (k_nn in k_n){
      
      drop_horizon <- c((max(t_vector)-g_horizon+1) : max(t_vector))
      
      if(interact == TRUE){
        source("brasil/R/regression_data_interaction_BRA.R") # comment to switch to alternative model
        #source("regression_data_interaction_manu.R") # uncomment to switch to alternative model
      } else {
        source("brasil/R/regression_data_BRA.R") # comment to switch to alternative model
        #source("regression_data_manu.R") # uncomment to switch to alternative model
      }
      
      source("brasil/R/SDM_BRA.R")
      
      
      # arrange results ---------------------------------------------------------
      
      if(g_horizon == g_horizons[1] & k_nn == k_n[1] & interact == model[1]){
        
        # output as table, post_mean/post_sd ~ bayesian t-values
        ifelse(interact == TRUE, Vars <- c(1, coefs[c(-1, -3)], "CHL", "MEX", "PER"), Vars <- c(1, coefs[-1]))
        results <- data.frame(
          Variables = Vars,
          Direct = direct_post_mean,
          Direct_t = direct_post_mean / direct_post_sd,
          Indirect = indirect_post_mean,
          Indirect_t = indirect_post_mean / indirect_post_sd
        )
        
        results_rho <- data.frame(
          Variables = "Rho",
          Direct = rho_post_mean,
          Direct_t = rho_post_mean / rho_post_sd,
          Indirect = NA,
          Indirect_t = NA
        )
        
        results_sigmas <- data.frame(
          Variables = c("sigma_MEX", "sigma_PER", "sigma_CHL"),
          Direct = sigma_post_mean,
          Direct_t = sigma_post_mean / sigma_post_sd,
          Indirect = NA,
          Indirect_t = NA
        )
        
        results_R2 <- data.frame(
          Variables = c("R2", "R2bar", "Obs."),
          Direct = c(R2, R2bar, n),
          Direct_t = NA,
          Indirect = NA,
          Indirect_t = NA
        )
        
        results <- dplyr::bind_rows(results, results_rho, results_sigmas, results_R2)
        colnames(results) <- c("Variables", paste(c("PM (direct)", "t value ", "PM (indirect)", "t value"), paste0(g_horizon, "(", k_nn, ")")))
      } else {
        
        # output as table, post_mean/post_sd ~ bayesian t-values
        ifelse(interact == TRUE, Vars <- c(1, coefs[c(-1, -3)], "CHL", "MEX", "PER"), Vars <- c(1, coefs[-1]))
        results_temp <- data.frame(
          Variables = Vars,
          Direct = direct_post_mean,
          Direct_t = direct_post_mean / direct_post_sd,
          Indirect = indirect_post_mean,
          Indirect_t = indirect_post_mean / indirect_post_sd
        )
        
        results_rho_temp <- data.frame(
          Variables = "Rho",
          Direct = rho_post_mean,
          Direct_t = rho_post_mean / rho_post_sd,
          Indirect = NA,
          Indirect_t = NA
        )
        
        results_sigmas_temp <- data.frame(
          Variables = c("sigma_MEX", "sigma_PER", "sigma_CHL"),
          Direct = sigma_post_mean,
          Direct_t = sigma_post_mean / sigma_post_sd,
          Indirect = NA,
          Indirect_t = NA
        )
        
        results_R2_temp <- data.frame(
          Variables = c("R2", "R2bar", "Obs."),
          Direct = c(R2, R2bar, n),
          Direct_t = NA,
          Indirect = NA,
          Indirect_t = NA
        )
        
        results_temp <- dplyr::bind_rows(results_temp, results_rho_temp, results_sigmas_temp, results_R2_temp)
        colnames(results_temp) <- c("Variables", paste(c("PM (direct)", "t value ", "PM (indirect)", "t value"), paste0(g_horizon, "(", k_nn, ")")))
        
        results <- dplyr::full_join(results, results_temp, by = "Variables")
        
      }
      
      if(interact == TRUE){
        store[[as.character(paste0(g_horizon, "int"))]][["coefs"]] <- postb
        store[[as.character(paste0(g_horizon, "int"))]][["rho"]] <- postr
        store[[as.character(paste0(g_horizon, "int"))]][["sigmas"]] <- posts
        store[[as.character(paste0(g_horizon, "int"))]][["R2"]] <- postrsq
        store[[as.character(paste0(g_horizon, "int"))]][["R2bar"]] <- postrsqbar
        store_int <- store
      } else {
        store[[as.character(paste0(g_horizon, "full"))]][["coefs"]] <- postb
        store[[as.character(paste0(g_horizon, "full"))]][["rho"]] <- postr
        store[[as.character(paste0(g_horizon, "full"))]][["sigmas"]] <- posts
        store[[as.character(paste0(g_horizon, "full"))]][["R2"]] <- postrsq
        store[[as.character(paste0(g_horizon, "full"))]][["R2bar"]] <- postrsqbar
        store_full <- store
      }
      
    }
  }
  
  if(interact == TRUE){
    store[["int"]][["results"]] <- results
  } else {
    store[["full"]][["results"]] <- results
  }
  
} # end loop m (model with or without interaction)

# latex output mean and t values ------------------------------------------

results <- results[-1,]

if(TRUE %in% model & FALSE %in% model){
  
  results$Variables <- c("Initial income", "Population density",
                         "GVA agriculture, foresty and fishing", "GVA financial and insurance", "Large port",
                         "Chile", "Mexico", "Peru",
                         as.character(results_rho$Variables), 
                         as.character(results_sigmas$Variables), 
                         as.character(results_R2$Variables), "Ore extraction")
  
} else {
  
  if(interact == TRUE){
    results$Variables <- c("Initial income", "Population density",
                           "GVA agriculture, foresty and fishing", "GVA financial and insurance", "Large port",
                           "Chile", "Mexico", "Peru",
                           as.character(results_rho$Variables), 
                           as.character(results_sigmas$Variables), 
                           as.character(results_R2$Variables))
  } else {
    results$Variables <- c("Initial income", "Ore extraction", "Population density",
                           "GVA agriculture, foresty and fishing", "GVA financial and insurance", "Large port",
                           as.character(results_rho$Variables), 
                           as.character(results_sigmas$Variables), 
                           as.character(results_R2$Variables))
  }
  
}






# latex results -----------------------------------------------------------
# i.e. direct, indirect and total combined

# combine
if(TRUE %in% model & FALSE %in% model){
  full_table <- results %>%
    dplyr::arrange(factor(Variables, 
                          levels = c("Initial income", "Ore extraction", "Chile", "Mexico", "Peru",
                                     "Population density", "GVA agriculture, foresty and fishing",
                                     "GVA financial and insurance", "Large port", 
                                     "Rho", "sigma_CHL", "sigma_MEX", "sigma_PER", "R2", "R2bar", "BIC", "AIC", "Obs.")))
  if(ncol(full_table == 17)){
    full_table <- full_table %>% dplyr::select("Variables", 
                                               "PM (direct) 2(5).y", "t value  2(5).y", "PM (indirect) 2(5).y", "t value 2(5).y",
                                               "PM (direct) 2(5).x", "t value  2(5).x", "PM (indirect) 2(5).x", "t value 2(5).x",
                                               "PM (direct) 5(5).y", "t value  5(5).y", "PM (indirect) 5(5).y", "t value 5(5).y",
                                               "PM (direct) 5(5).x", "t value  5(5).x", "PM (indirect) 5(5).x", "t value 5(5).x")
  }
  print(xtable::xtable(full_table, 
                       digits = c(0, 0, rep(c(3, 3, 3, 3), length(g_horizons)*length(model))), align = paste0("ll", paste0(rep("|rr|rr", length(g_horizons)*length(model)), collapse = "")),
                       caption = paste("Panel SDM impact estimates",
                                       paste0(min(t_vector), "-", max(t_vector)-g_horizon), 
                                       paste0(paste(g_horizons, collapse = " and "), " y avg. annual growth rates"),
                                       "time FE",
                                       "country FE", sep = ", ")), 
        include.rownames=FALSE, size = "small")
} else {
  
  # interaction
  if(TRUE %in% model){
    print(xtable::xtable(results, 
                         digits = c(0, 0, rep(c(3, 3, 3, 3), length(g_horizons))), align = paste0("ll", paste0(rep("|rr|rr", length(g_horizons)), collapse = "")),
                         caption = paste("Panel SDM impact estimates",
                                         paste0(min(t_vector), "-", max(t_vector)-g_horizon), 
                                         paste0(paste(g_horizons, collapse = " and "), " y avg. annual growth rates"),
                                         "time FE",
                                         "country FE", sep = ", ")), 
          include.rownames=FALSE, size = "small")
  }
  
  # no interaction
  if(FALSE %in% model){
    print(xtable::xtable(results, 
                         digits = c(0, 0, rep(c(3, 3, 3, 3), length(g_horizons))), align = paste0("ll", paste0(rep("|rr|rr", length(g_horizons)), collapse = "")),
                         caption = paste("Panel SDM impact estimates",
                                         paste0(min(t_vector), "-", max(t_vector)-g_horizon), 
                                         paste0(paste(g_horizons, collapse = " and "), " y avg. annual growth rates"),
                                         "time FE",
                                         "country FE", sep = ", ")), 
          include.rownames=FALSE, size = "small")
  }
  
}


save(store, file = paste0("output/store_", Sys.Date(), ".RData"))



# # for variations in W
# print(xtable::xtable(results, 
#                      digits = c(0, 0, rep(c(3, 3, 3, 3), length(k_n))), align = paste0("ll", paste0(rep("|rr|rr", length(k_n)), collapse = "")),
#                      caption = paste("Panel SDM impact estimates",
#                                      paste0(min(t_vector), "-", max(t_vector)-g_horizon), 
#                                      paste0(paste(g_horizons, collapse = " and "), " y avg. annual growth rates"),
#                                      "time FE",
#                                      "country FE", sep = ", ")), 
#       include.rownames=FALSE, size = "small")


