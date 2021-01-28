
source("R/functions.R")

# select years and countries for panel
# t_vector <- c(2002:2016) # too big weights matrix -> cloud
t_vector <- c(2002:2016)
g_horizons <- c(2)
c_vector1 <- c("Brazil")
c_vector2 <- c("BRA")
k_n <- c(5) 

store <- list()
model <- c(FALSE) # interaction TRUE or FALSE, or both

# interact <- FALSE
# g_horizon <- 2
# k_nn <- 5

for(interact in model){
  
  for (g_horizon in g_horizons){
    
    if(interact == TRUE){
      store[[as.character(paste0(g_horizon, "int"))]] <- list()
    } else {
      store[[as.character(paste0(g_horizon, "full"))]] <- list()
    }
    
    for (k_nn in k_n){
      
      drop_horizon <- c((max(t_vector)-g_horizon+1) : max(t_vector))
      
      source("brasil/R/regression_data_BRA.R")
      
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
          Variables = c("sigma.BRA.1_1", 
                        "sigma.BRA.2_1",
                        "sigma.BRA.3_1",
                        "sigma.BRA.4_1",
                        "sigma.BRA.5_1", 
                        "sigma.BRA.6_1",
                        "sigma.BRA.7_1",
                        "sigma.BRA.8_1", 
                        "sigma.BRA.9_1", 
                        "sigma.BRA.10_1", 
                        "sigma.BRA.11_1", 
                        "sigma.BRA.12_1",
                        "sigma.BRA.13_1",
                        "sigma.BRA.14_1", 
                        "sigma.BRA.15_1", 
                        "sigma.BRA.16_1", 
                        "sigma.BRA.17_1",
                        "sigma.BRA.18_1",
                        "sigma.BRA.19_1",
                        "sigma.BRA.20_1", 
                        "sigma.BRA.21_1", 
                        "sigma.BRA.22_1",
                        "sigma.BRA.23_1",
                        "sigma.BRA.24_1",
                        "sigma.BRA.25_1",
                        "sigma.BRA.26_1",
                        "sigma.BRA.27_1"),
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
        ifelse(interact == TRUE, Vars <- c(1, coefs[c(-1, -3)], results_sigmas$Variables), 
               Vars <- c(1, coefs[-1]))
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
          Variables = results_sigmas$Variables,
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

save(store, file = paste0("brasil/output/store_BRA_", Sys.Date(), ".RData"))

results <- results[-1,]

results$Variables <- c("Initial income", "Ore extraction", "Population density",
                       "GVA agriculture, foresty and fishing", "GVA financial and insurance", "Large port",
                       as.character(results_rho$Variables), 
                       as.character(results_sigmas$Variables), 
                       as.character(results_R2$Variables))




# latex results -----------------------------------------------------------
# i.e. direct, indirect and total combined

print(xtable::xtable(results, 
                      digits = c(0, 0, rep(c(3, 3, 3, 3), length(g_horizons))), align = paste0("ll", paste0(rep("|rr|rr", length(g_horizons)), collapse = "")),
                      caption = paste("Panel SDM impact estimates",
                                     paste0(min(t_vector), "-", max(t_vector)-g_horizon), 
                                     paste0(paste(g_horizons, collapse = " and "), " y avg. annual growth rates"),
                                     "time FE",
                                     "country FE", sep = ", ")), 
          include.rownames=FALSE, size = "small")



# confidence intervalls (full impact result data to be stored in loop!)
direct_post_05 = apply( post.direct , 1 , quantile , probs = 0.05)[-1]
direct_post_50 = apply( post.direct , 1 , quantile , probs = 0.5)[-1]
indirect_post_05 = apply( post.indirect , 1 , quantile , probs = 0.05)[-1]
indirect_post_50 = apply( post.indirect , 1 , quantile , probs = 0.5)[-1]
direct_post_95 = apply( post.direct , 1 , quantile , probs = 0.95)[-1]
indirect_post_95 = apply( post.indirect , 1 , quantile , probs = 0.95)[-1]
rho_post_05 = quantile(postr, 0.05)
rho_post_50 = quantile(postr, 0.5)
rho_post_95 = quantile(postr, 0.95)
rsq_post_05 = quantile(postrsq, 0.05)
rsq_post_50 = quantile(postrsq, 0.5)
rsq_post_95 = quantile(postrsq, 0.95)
rsqbar_post_05 = quantile(postrsqbar, 0.05)
rsqbar_post_50 = quantile(postrsqbar, 0.5)
rsqbar_post_95 = quantile(postrsqbar, 0.95)

results2 <- results[c(1:7, 35:37),]
results2 <- results2 %>% 
  dplyr::mutate(direct5perc = c(direct_post_05, rho_post_05, rsq_post_05, rsqbar_post_05, NA)) %>%
  dplyr::mutate(direct50perc = c(direct_post_50, rho_post_50, rsq_post_50, rsqbar_post_50, NA)) %>%
  dplyr::mutate(direct95perc = c(direct_post_95, rho_post_95, rsq_post_95, rsqbar_post_95, NA)) %>%
  dplyr::mutate(indirect5perc = c(indirect_post_05, rep(NA, 4))) %>%
  dplyr::mutate(indirect50perc = c(indirect_post_50, rep(NA, 4))) %>%
  dplyr::mutate(indirect95perc = c(indirect_post_95, rep(NA, 4))) %>%
  dplyr::select(Variables, direct5perc, direct50perc, direct95perc,  indirect5perc, indirect50perc, indirect95perc )

print(xtable::xtable(results2, 
                     digits = c(0, 0, rep(c(3, 3, 3, 3, 3, 3), length(g_horizons))), align = paste0("ll", paste0(rep("|rrr|rrr", length(g_horizons)), collapse = "")),
                     caption = paste("Panel SDM impact estimates",
                                     paste0(min(t_vector), "-", max(t_vector)-g_horizon), 
                                     paste0(paste(g_horizons, collapse = " and "), " y avg. annual growth rates"),
                                     "time FE",
                                     "country FE", sep = ", ")), 
      include.rownames=FALSE, size = "tiny")

