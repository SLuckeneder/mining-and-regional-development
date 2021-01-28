
sar_grid <- function(
  x, W, LX = FALSE,
  n_draw = 1000L, n_burn = 0L, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time,
  sigma_a = 10, sigma_b = 1,
  beta_mean = 0, beta_var = 10 ^ 8,
  rho_a = 1.01,
  grid = NULL,
  verbose = TRUE) {
  
  # Setup ----------
  
  cl <- match.call()
  start_time <- Sys.time()
  
  n_save <- as.integer((n_draw - n_burn) / n_thin)
  
  y <- x[, 1] # Dependent in first column
  N <- length(y)
  
  # Check W
  msg <- "Please make sure W has the right dimension."
  W_pre <- NULL
  if(tfe || ife) {
    if(N == nrow(W)) {
      W_pre <- W[seq(1, N / n_time), seq(1, N / n_time)]
    } else if(as.integer(N / nrow(W)) %% 1 == 0) {
      if(missing(n_time)) {
        n_time <- N / nrow(W)
      } else if(n_time != N / nrow(W)) {stop(msg)}
      W_pre <- W
      W <- kronecker(Matrix::.sparseDiagonal(n_time), W)
    } else {stop(msg)}
  } else if(N != nrow(W)) {stop(msg)}
  
  X <- build_X(x, const = TRUE, tfe, ife, n_time,
               W = if(LX) {W} else {NULL})
  K <- ncol(X)
  k <- sum(colnames(X) %in% c("alpha", "beta"))
  
  
  # Grid ----------
  if(is.null(grid)) {
    message("Calculating grid... Consider providing `get_grid()` to `grid`.")
    grid <- get_grid(W, W_pre, y, N, n_rho = 200L, type = "eigen")
  }
  
  
  # Priors ----------
  
  beta_pr_mean <- matrix(beta_mean, K, 1)
  beta_pr_var <- diag(K) * beta_var
  beta_pr_var_inv <- solve(beta_pr_var)
  
  
  # Storage ----------
  
  beta_store <- matrix(NA, nrow = n_save, ncol = K)
  colnames(beta_store) <- colnames(X)
  sigma_store <- rho_store <- vector("numeric", n_save)
  ll <- vector("numeric", n_save)
  
  # Starting values
  beta_draw <- as.numeric(mvtnorm::rmvnorm(1L, mean = beta_pr_mean,
                                           sigma = beta_pr_var))
  sigma_draw <- 1 / rgamma(1, sigma_a / 2, sigma_b / 2)
  j <- as.integer(grid[["n_rho"]] / 2L)
  rho_draw <- grid[["rhos"]][j] # Middle
  
  XX <- crossprod(X)
  Xy <- crossprod(X, y)
  Wy <- W %*% y
  XWy <- crossprod(X, Wy)
  
  if(verbose) {pb <- txtProgressBar(min = 0, max = n_draw, style = 3)}
  
  for(i in (1 - n_burn):(n_draw - n_burn)) {
    # Beta
    V <- solve(beta_pr_var_inv + 1 / sigma_draw * XX)
    b <- V %*% (beta_pr_var %*% beta_pr_mean +
                  1 / sigma_draw * crossprod(X, grid[["Ay"]][j, ]))
    beta_draw <- as.numeric(mvtnorm::rmvnorm(1L, mean = b, sigma = V))
    
    # Sigma
    ESS_draw <- as.double(crossprod(grid[["Ay"]][j, ] - X %*% beta_draw))
    sigma_draw <- 1 / rgamma(1, sigma_a + N / 2, sigma_b + ESS_draw / 2)
    
    # Rho
    V <- solve(beta_pr_var_inv + 1 / sigma_draw * XX)
    b0 <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / sigma_draw * Xy)
    bd <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / sigma_draw * XWy)
    
    e0 <- y - X %*% b0
    ed <- Wy - X %*% bd
    
    epe0 <- as.double(crossprod(e0))
    eped <- as.double(crossprod(ed))
    epe0d <- as.double(crossprod(ed, e0))
    
    z0 <- -(N - K) / 2 *
      log(epe0 - 2 * grid[["rhos"]] * epe0d + grid[["rhos"]] ^ 2 * eped)
    den <- grid[["ldets"]] + z0 + log(beta_prob(grid[["rhos"]], rho_a))
    ex <- exp(den - max(den))
    i_sum <- sum((grid[["rhos"]][-1] - grid[["rhos"]][-grid[["n_rho"]]]) *
                   (ex[-1] + ex[-length(ex)]) / 2)
    z1 <- abs(ex / i_sum)
    dens <- cumsum(z1)
    rnd <- runif(1) * sum(z1)
    j <- max(which(dens <= rnd))
    if(length(j) != 1) { # Reuse the last
      j <- which(grid[["rhos"]] == rho_draw)
    }
    rho_draw <- grid[["rhos"]][j]
    
    # Store
    if(i > 0 && i %% n_thin == 0) {
      
      beta_store[(i / n_thin), ] <- beta_draw
      sigma_store[(i / n_thin)] <- sigma_draw
      rho_store[(i / n_thin)] <- rho_draw
      ll[(i / n_thin)] <- grid[["ldets"]][j] - ESS_draw / (2 * sigma_draw) +
        beta_prob(rho_draw, rho_a)
    }
    
    if(verbose) {setTxtProgressBar(pb, (i + n_burn))}
  }
  
  timer <- Sys.time() - start_time
  
  if(verbose) {
    close(pb)
    cat("Finished after ", format(round(timer, 2)), ".\n", sep = "")
  }
  
  # Outputs ----------
  
  out <- list(
    "beta" = beta_store,
    "sigma" = sigma_store,
    "rho" = rho_store,
    "ll" = ll,
    "priors" = list(
      "sigma_a" = sigma_a, "sigma_b" = sigma_b,
      "beta_mean" = beta_mean, "beta_var" = beta_var,
      "rho_a" = rho_a
    ),
    "meta" = list(
      "timer" = timer,
      "y" = y, "X" = X, "W" = W, "LX" = LX, "N" = N, "K" = K, "k" = k,
      "n_draw" = n_draw, "n_burn" = n_burn, "n_save" = n_save, "n_thin" = n_thin
    )
  )
  class(out) <- "sar"
  
  return(out)
}


get_grid <- function(
  W, W_pre = NULL, y, N,
  n_rho, rho_min = -1, rho_max = 1,
  type = c("exact",
           "eigen", "Matrix_J", "spam", "LU", "MC", "cheb", "moments")) {
  
  type <- match.arg(type)
  if(type != "exact") {
    ldet_env <- new.env()
    ldet_env[["listw"]] <- spdep::mat2listw(if(is.null(W_pre)) {W} else {W_pre})
    ldet_env[["can.sim"]] <- spatialreg::can.be.simmed(ldet_env[["listw"]])
    ldet_env[["verbose"]] <- FALSE
    ldet_env[["family"]] <- "SAR"
    ldet_env[["n"]] <- N
    eval(call(paste0(type, "_setup"), ldet_env))
  }
  if(missing(W) | is.null(W)) {W <- kronecker(diag(N / nrow(W_pre)), W_pre)}
  
  rhos <- seq(rho_min, rho_max, length.out = n_rho + 2)[-c(1, n_rho + 2)]
  
  ldets <- vector("numeric", n_rho)
  A <- AA <- vector("list", n_rho)
  Ay <- Matrix(0, n_rho, N)
  B_trace <- B_sum <- BW_trace <- BW_sum <- vector("numeric", n_rho)
  
  for(i in seq_along(rhos)) {
    A[[i]] <- Matrix:::.sparseDiagonal(N) - rhos[i] * W
    Ay[i, ] <- A[[i]] %*% y
  }
  
  # Cheaper log|A| if W is created via a Kronecker product
  if(is.null(W_pre)) {
    ldets <- if(type == "exact") {
      vapply(A, function(x) {log(det(x))}, numeric(1L))
    } else {
      vapply(rhos, do_ldet, numeric(1L), env = ldet_env)
    }
  } else {
    ldets <- if(type == "exact") {
      vapply(rhos, function(x, W_pre, N) {
        log(det(Matrix:::.sparseDiagonal(N) - x * W_pre))
      }, numeric(1L), W_pre, N = nrow(W_pre))
    } else {
      vapply(rhos, do_ldet, numeric(1L), env = ldet_env)
    }
    ldets <- log(exp(ldets) ^ as.integer(N / nrow(W_pre)))
  }
  
  list(
    "rhos" = rhos, "ldets" = ldets, "Ay" = Ay, "n_rho" = n_rho
  )
}


build_X <- function(
  x, const = TRUE,
  tfe = FALSE, ife = FALSE, n_time,
  W = NULL) {
  
  N <- nrow(x)
  X <- X_pre <- x[, -1]
  colnames(X) <- rep("beta", ncol(X))
  if(const) {X <- cbind("alpha" = 1, X)}
  if(!is.null(W)) {
    WX <- W %*% X_pre
    colnames(WX) <- rep("theta", ncol(WX))
    X <- cbind(X, WX)
  }
  if(tfe | ife) {
    if(missing(n_time)) {stop("Please provide the number of time periods.")}
    n_ind <- N / n_time
    if(tfe) {
      TFE <- kronecker(diag(n_time), matrix(1, n_ind))[, -1]
      colnames(TFE) <- rep("tfe", ncol(TFE))
      X <- cbind(X, TFE)
    }
    if(ife) {
      IFE <- kronecker(matrix(1, n_time), diag(n_ind))[, -1]
      colnames(IFE) <- rep("ife", ncol(IFE))
      X <- cbind(X, IFE)
    }
  }
  
  X
}


beta_prob <- function(rho, a) {
  1 / beta(a, a) *
    ((1 + rho) ^ (a - 1) * (1 - rho) ^ (a - 1)) /
    (2 ^ (2 * a - 1))
}


effects.sar <- function(x, n_draw) {
  
  x <- results_mh
  
  if(missing(n_draw)) {n_draw <- x$meta$n_save}
  draws <- sample(x$meta$n_save, n_draw, replace = FALSE)
  
  rhos <- x$rho[draws]
  betas <- x$beta[draws, ]
  k <- x$meta$k
  W <- x$meta$W
  N <- x$meta$N
  LX <- x$meta$LX
  index <- colnames(x$beta)
  
  eff_dir <- eff_ind <- matrix(NA, nrow = n_draw, ncol = k)
  
  for(i in seq_len(n_draw)) {
    B <- solve(Matrix:::.sparseDiagonal(N) - rhos[i] * W); gc()
    eff_dir[i, ] <- sum(diag(B)) / N * betas[i, seq_len(k)] + if(LX) {
      
      BW12 <- B[, 1:(ncol(B) / 13 * 2)] %*% W[1:(ncol(B) / 13 * 2), 1:(ncol(B) / 13 * 2)]; gc()
      BW34 <- B[, ((ncol(B) / 13 * 2)+1):((ncol(B) / 13 * 4))] %*% W[ ((ncol(B) / 13 * 2)+1):((ncol(B) / 13 * 4)),  ((ncol(B) / 13 * 2)+1):((ncol(B) / 13 * 4))]; gc()
      BW56 <- B[, ((ncol(B) / 13 * 4)+1):((ncol(B) / 13 * 8))] %*% W[ ((ncol(B) / 13 * 4)+1):((ncol(B) / 13 * 8)),  ((ncol(B) / 13 * 4)+1):((ncol(B) / 13 * 8))]; gc()
      BW <- cbind()
      
      c(0, sum(diag(BW)) / N * betas[i, index == "theta"])
    } else {0}
    eff_tot <- sum(B) / N * betas[i, seq_len(k)] + if(LX) {
      c(0, sum(BW) / N * betas[i, index == "theta"])
    } else {0}
    eff_ind[i, ] <- eff_tot - eff_dir[i, ]
  }
  
  return(list("direct" = eff_dir, "indirect" = eff_ind))
}

logLik.sar <- function(x, fun = median) {
  rho <- fun(x$rho)
  beta <- apply(x$beta, 2, fun)
  sigma <- fun(x$sigma)
  
  A <- Matrix:::.sparseDiagonal(x$meta$N) - rho * x$meta$W
  ldet <- log(det(A))
  ESS <- as.numeric(crossprod(A %*% x$meta$y - x$meta$X %*% beta))
  
  ldet - ESS / (2 * sigma) + beta_prob(rho, x$priors$rho_a)
}


BIC.sar <- function(x) {
  
  -2 * logLik(x) + log(x$meta$N) * (x$meta$K + 1)
  
}



sar_mh <- function(
  x, W, LX = FALSE,
  n_draw = 1000L, n_burn = 0L, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time,
  sigma_a = 10, sigma_b = 1,
  beta_mean = 0, beta_var = 10 ^ 8,
  rho_a = 1.01,
  type = "eigen",
  verbose = TRUE) {
  
  # Setup ----------
  
  cl <- match.call()
  start_time <- Sys.time()
  
  n_save <- as.integer((n_draw - n_burn) / n_thin)
  
  y <- x[, 1] # Dependent in first column
  N <- length(y)
  
  # Check W
  msg <- "Please make sure W has the right dimension."
  W_pre <- NULL
  if(tfe || ife) {
    if(N == nrow(W)) {
      W_pre <- W[seq(1, N / n_time), seq(1, N / n_time)]
    } else if(as.integer(N / nrow(W)) %% 1 == 0) {
      if(missing(n_time)) {
        n_time <- N / nrow(W)
      } else if(n_time != N / nrow(W)) {stop(msg)}
      W_pre <- W
      W <- kronecker(Matrix::.sparseDiagonal(n_time), W)
    } else {stop(msg)}
  } else if(N != nrow(W)) {stop(msg)}
  
  X <- build_X(x, const = TRUE, tfe, ife, n_time,
               W = if(LX) {W} else {NULL})
  K <- ncol(X)
  k <- sum(colnames(X) %in% c("alpha", "beta"))
  
  
  # Setup for spatialreg's Jacobian ----------
  ldet_env <- new.env()
  ldet_env[["listw"]] <- spdep::mat2listw(if(is.null(W_pre)) {W} else {W_pre})
  ldet_env[["can.sim"]] <- spatialreg::can.be.simmed(ldet_env[["listw"]])
  ldet_env[["verbose"]] <- FALSE
  ldet_env[["family"]] <- "SAR"
  ldet_env[["n"]] <- N
  eval(call(paste0(type, "_setup"), ldet_env))
  
  
  # Priors ----------
  
  beta_pr_mean <- matrix(beta_mean, K, 1)
  beta_pr_var <- diag(K) * beta_var
  beta_pr_var_inv <- solve(beta_pr_var)
  
  
  # Storage ----------
  
  beta_store <- matrix(NA, nrow = n_save, ncol = K)
  colnames(beta_store) <- colnames(X)
  sigma_store <- rho_store <- vector("numeric", n_save)
  ll <- vector("numeric", n_save)
  
  # Starting values
  beta_draw <- as.numeric(mvtnorm::rmvnorm(1L, mean = beta_pr_mean,
                                           sigma = beta_pr_var))
  sigma_draw <- 1 / rgamma(1, sigma_a / 2, sigma_b / 2)
  rho_draw <- 0 # Middle
  rho_var <- 0.2
  accepted <- accepted_adj <- 0
  Ay_draw <- (Matrix:::.sparseDiagonal(N) - rho_draw * W) %*% y
  
  XX <- crossprod(X)
  Xy <- crossprod(X, y)
  Wy <- W %*% y
  XWy <- crossprod(X, Wy)
  
  if(verbose) {pb <- txtProgressBar(min = 0, max = n_draw, style = 3)}
  
  for(i in (1 - n_burn):(n_draw - n_burn)) {
    # Beta
    V <- solve(beta_pr_var_inv + 1 / sigma_draw * XX)
    b <- V %*% (beta_pr_var %*% beta_pr_mean +
                  1 / sigma_draw * crossprod(X, Ay_draw))
    beta_draw <- as.numeric(MASS::mvrnorm(1L, mu = b, Sigma = V))
    
    # Sigma
    X_beta <- X %*% beta_draw
    ESS_draw <- as.double(crossprod(Ay_draw - X_beta))
    sigma_draw <- 1 / rgamma(1, sigma_a + N / 2, sigma_b + ESS_draw / 2)
    
    # Rho
    if(i == 1 - n_burn) { # Initial draw
      ldet_draw <- if(is.null(W_pre)) {
        do_ldet(rho_draw, env = ldet_env)
      } else {
        log(exp(do_ldet(rho_draw, env = ldet_env)) ^
              as.integer(N / nrow(W_pre)))
      }
    }
    ll_draw <- (ldet_draw - ESS_draw / (2 * sigma_draw) +
                  beta_prob(rho_draw, rho_a))
    
    while(TRUE) {
      rho_temp <- rnorm(1, mean = rho_draw, sd = rho_var)
      if(rho_temp < 1 && rho_temp > -1) {break}
    }
    
    Ay_temp <- (Matrix:::.sparseDiagonal(N) - rho_temp * W) %*% y
    ESS_temp <- as.double(crossprod(Ay_temp - X_beta))
    sigma_temp <- 1 / rgamma(1, sigma_a + N / 2, sigma_b + ESS_temp / 2)
    ldet_temp <- if(is.null(W_pre)) {
      do_ldet(rho_temp, env = ldet_env)
    } else {
      log(exp(do_ldet(rho_temp, env = ldet_env)) ^ as.integer(N / nrow(W_pre)))
    }
    ll_temp <- (ldet_temp - ESS_temp / (2 * sigma_temp) +
                  beta_prob(rho_temp, rho_a))[1]
    
    if(runif(1) < exp(ll_temp - ll_draw)) {
      accepted_adj <- accepted_adj + 1
      if(i > 0) {accepted <- accepted + 1}
      ll_draw <- ll_temp
      ldet_draw <- ldet_temp
      rho_draw <- rho_temp
      Ay_draw <- Ay_temp
      sigma_draw <- sigma_temp
    }
    
    # Tune acceptance during burn-in phase
    if(i <= 0 && (i + n_burn) %% 10 == 0) {
      acc_rate <- accepted_adj / (i + n_burn)
      if(acc_rate < 0.2) {
        rho_var <- rho_var * 0.99
      } else if(acc_rate > 0.5) {
        rho_var <- rho_var * 1.01
      }
    }
    
    # Store
    if(i > 0 && i %% n_thin == 0) {
      
      beta_store[(i / n_thin), ] <- beta_draw
      sigma_store[(i / n_thin)] <- sigma_draw
      rho_store[(i / n_thin)] <- rho_draw
      ll[(i / n_thin)] <- ldet_draw - ESS_draw / (2 * sigma_draw) +
        beta_prob(rho_draw, rho_a)
    }
    
    if(verbose) {setTxtProgressBar(pb, (i + n_burn))}
  }
  
  timer <- Sys.time() - start_time
  
  if(verbose) {
    close(pb)
    cat("Finished after ", format(round(timer, 2)), ".\n", sep = "")
    cat("\nAccepted draws (%): ", accepted,
        " (", round(accepted / (n_draw - n_burn), 2L), ")\n", sep = "")
  }
  
  # Outputs ----------
  
  out <- list(
    "beta" = beta_store,
    "sigma" = sigma_store,
    "rho" = rho_store,
    "ll" = ll,
    "priors" = list(
      "sigma_a" = sigma_a, "sigma_b" = sigma_b,
      "beta_mean" = beta_mean, "beta_var" = beta_var,
      "rho_a" = rho_a
    ),
    "meta" = list(
      "timer" = timer,
      "y" = y, "X" = X, "W" = W, "LX" = LX, "N" = N, "K" = K, "k" = k,
      "n_draw" = n_draw, "n_burn" = n_burn, "n_save" = n_save, "n_thin" = n_thin
    )
  )
  class(out) <- "sar"
  
  return(out)
}

