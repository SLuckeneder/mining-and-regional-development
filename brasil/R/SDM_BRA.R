


# prepare data ------------------------------------------------------------

t <- length(t_vector) - g_horizon
# choose k nearest and calculate kronecker product
W <- kronecker(diag(t), W_k)

X <- X[,-1]
smallk <- ncol(X)
X <- cbind(1, X,W %*% X, D, C) # time + country dummy
n <- nrow(X)
k <- ncol(X)


### let us assign prior values
# beta mean and variance are proper, but with high variance (= low prior information)
beta_prior_mean = matrix(0,k,1)
beta_prior_var = diag(k) * 10^8

# sigma rate and shape are also proper but non-informative
sigma_a = 0.01
sigma_b = 0.01

# rho prior is beta, with 
beta_prob = function(rho,a) 1/beta(a,a) * ((1+rho)^(a-1) *(1-rho)^(a-1))/(2^(2*a - 1))
rho_a = 1.01

# ### calibration parameters for rho sampling
# cc = 1 # scaling of rho proposals
# c_adjust = 1.1 # proposal distribution adjustment
# rho_accept = 0 # counter for rho acceptance rates

### set-up the gibbs sampler
# total number of draws
niter = 2000
# retain only S2 and discard S1, with S = S1 + S2
nretain = 1000
ndiscard = niter - nretain
# save the posterior draws here
postb = matrix(0,k,nretain)
posts = matrix(0,27,nretain)
postr = matrix(0,1,nretain)
postrsq = matrix(0,1,nretain)
postrsqbar = matrix(0,1,nretain)
postbic = matrix(0,1,nretain)
postaic = matrix(0,1,nretain)

post.direct = matrix(0,smallk + 1,nretain)
post.indirect = matrix(0,smallk + 1,nretain)
post.total = matrix(0,smallk + 1,nretain)

# set-up for griddy gibbs
# griddy_n = 500
griddy_n = 100
logdets = lndetPaceBarry(as.matrix(W),length.out = griddy_n+2)
logdets = logdets[-c(1,griddy_n + 2),]
rrhos = logdets[,2]
AYs = array(0,c(n,griddy_n))
## storage for efficient partial derivatives
ai_diags = rep(0,griddy_n)
ai_tots = rep(0,griddy_n)
aiW_diags = rep(0,griddy_n)
aiW_tots = rep(0,griddy_n)
cat("Pre-calculate griddy GIBBS...")
for (ii in 1:griddy_n) {
  cat("\n", ii)
  A = (Matrix::.sparseDiagonal(n) - rrhos[ii] * W)
  AYs[,ii] = as.matrix(A %*% Y)
  AI = solve(A)
  ai_diags[ii] = sum(diag(AI))
  ai_tots[ii] = sum(AI)
  aiW_diags[ii] = sum(diag(AI %*% W))
  aiW_tots[ii] = sum(AI %*% W)
}
cat("Done!\n")
save(AYs, file = paste0("brasil/output/store_AYs_", Sys.Date(), ".RData"))
save(ai_diags, file = paste0("brasil/output/store_ai_diags_", Sys.Date(), ".RData"))
save(ai_tots, file = paste0("brasil/output/store_ai_tots_", Sys.Date(), ".RData"))
save(aiW_diags, file = paste0("brasil/output/store_aiW_diags_", Sys.Date(), ".RData"))
save(aiW_tots, file = paste0("brasil/output/store_aiW_tots_", Sys.Date(), ".RData"))

# starting values (won't matter after sufficient draws)
curr.beta = MASS::mvrnorm(1,beta_prior_mean,beta_prior_var)
curr.sigma = 1/rgamma(1,sigma_a/2,sigma_b/2)
curr.rho = 0.2

# pre-calculate some terms for faster draws
beta_prior_var_inv = solve(beta_prior_var)
XpX = t(X) %*% X
WY = W %*% Y
curr.Ay = Y - curr.rho*WY

# get respective rows for subsetting residuals
nregs <- length(W_str$GID_2)
nyrs <- n / nregs
str_panel <- do.call("rbind", replicate(nyrs, W_str, simplify = FALSE))

for(GID in unique(str_panel$GID_1)){
  e. <- str_panel %>%
    dplyr::mutate(id = as.numeric(rownames(str_panel))) %>%
    dplyr::filter(GID_1 == GID) %>%
    dplyr::select(id) %>% unlist()
  assign(paste0("e.", GID), e.)
}

### Gibbs sampling
for (iter in 1:niter) {
  cat("iter:",iter,"curr.rho:",curr.rho,"\n")
  
  curr.xb = X %*% curr.beta
  
  # calculate resisuals and state-specific sigma
  e <- curr.Ay - curr.xb
  
  curr.ee.BRA.1_1 <- crossprod(e[e.BRA.1_1])
  curr.sigma.BRA.1_1 <- 1/rgamma(1, sigma_a + length(e.BRA.1_1)/2, sigma_b + as.double(curr.ee.BRA.1_1) / 2)
  curr.ee.BRA.2_1 <- crossprod(e[e.BRA.2_1])
  curr.sigma.BRA.2_1 <- 1/rgamma(1, sigma_a + length(e.BRA.2_1)/2, sigma_b + as.double(curr.ee.BRA.2_1) / 2)
  curr.ee.BRA.3_1 <- crossprod(e[e.BRA.3_1])
  curr.sigma.BRA.3_1 <- 1/rgamma(1, sigma_a + length(e.BRA.3_1)/2, sigma_b + as.double(curr.ee.BRA.3_1) / 2)
  curr.ee.BRA.4_1 <- crossprod(e[e.BRA.4_1])
  curr.sigma.BRA.4_1 <- 1/rgamma(1, sigma_a + length(e.BRA.4_1)/2, sigma_b + as.double(curr.ee.BRA.4_1) / 2)
  curr.ee.BRA.5_1 <- crossprod(e[e.BRA.5_1])
  curr.sigma.BRA.5_1 <- 1/rgamma(1, sigma_a + length(e.BRA.5_1)/2, sigma_b + as.double(curr.ee.BRA.5_1) / 2)
  curr.ee.BRA.6_1 <- crossprod(e[e.BRA.6_1])
  curr.sigma.BRA.6_1 <- 1/rgamma(1, sigma_a + length(e.BRA.6_1)/2, sigma_b + as.double(curr.ee.BRA.6_1) / 2)
  curr.ee.BRA.7_1 <- crossprod(e[e.BRA.7_1])
  curr.sigma.BRA.7_1 <- 1/rgamma(1, sigma_a + length(e.BRA.7_1)/2, sigma_b + as.double(curr.ee.BRA.7_1) / 2)
  curr.ee.BRA.8_1 <- crossprod(e[e.BRA.8_1])
  curr.sigma.BRA.8_1 <- 1/rgamma(1, sigma_a + length(e.BRA.8_1)/2, sigma_b + as.double(curr.ee.BRA.8_1) / 2)
  curr.ee.BRA.9_1 <- crossprod(e[e.BRA.9_1])
  curr.sigma.BRA.9_1 <- 1/rgamma(1, sigma_a + length(e.BRA.9_1)/2, sigma_b + as.double(curr.ee.BRA.9_1) / 2)
  curr.ee.BRA.10_1 <- crossprod(e[e.BRA.10_1])
  curr.sigma.BRA.10_1 <- 1/rgamma(1, sigma_a + length(e.BRA.10_1)/2, sigma_b + as.double(curr.ee.BRA.10_1) / 2)
  curr.ee.BRA.11_1 <- crossprod(e[e.BRA.11_1])
  curr.sigma.BRA.11_1 <- 1/rgamma(1, sigma_a + length(e.BRA.11_1)/2, sigma_b + as.double(curr.ee.BRA.11_1) / 2)
  curr.ee.BRA.12_1 <- crossprod(e[e.BRA.12_1])
  curr.sigma.BRA.12_1 <- 1/rgamma(1, sigma_a + length(e.BRA.12_1)/2, sigma_b + as.double(curr.ee.BRA.12_1) / 2)
  curr.ee.BRA.13_1 <- crossprod(e[e.BRA.13_1])
  curr.sigma.BRA.13_1 <- 1/rgamma(1, sigma_a + length(e.BRA.13_1)/2, sigma_b + as.double(curr.ee.BRA.13_1) / 2)
  curr.ee.BRA.14_1 <- crossprod(e[e.BRA.14_1])
  curr.sigma.BRA.14_1 <- 1/rgamma(1, sigma_a + length(e.BRA.14_1)/2, sigma_b + as.double(curr.ee.BRA.14_1) / 2)
  curr.ee.BRA.15_1 <- crossprod(e[e.BRA.15_1])
  curr.sigma.BRA.15_1 <- 1/rgamma(1, sigma_a + length(e.BRA.15_1)/2, sigma_b + as.double(curr.ee.BRA.15_1) / 2)
  curr.ee.BRA.16_1 <- crossprod(e[e.BRA.16_1])
  curr.sigma.BRA.16_1 <- 1/rgamma(1, sigma_a + length(e.BRA.16_1)/2, sigma_b + as.double(curr.ee.BRA.16_1) / 2)
  curr.ee.BRA.17_1 <- crossprod(e[e.BRA.17_1])
  curr.sigma.BRA.17_1 <- 1/rgamma(1, sigma_a + length(e.BRA.17_1)/2, sigma_b + as.double(curr.ee.BRA.17_1) / 2)
  curr.ee.BRA.18_1 <- crossprod(e[e.BRA.18_1])
  curr.sigma.BRA.18_1 <- 1/rgamma(1, sigma_a + length(e.BRA.18_1)/2, sigma_b + as.double(curr.ee.BRA.18_1) / 2)
  curr.ee.BRA.19_1 <- crossprod(e[e.BRA.19_1])
  curr.sigma.BRA.19_1 <- 1/rgamma(1, sigma_a + length(e.BRA.19_1)/2, sigma_b + as.double(curr.ee.BRA.19_1) / 2)
  curr.ee.BRA.20_1 <- crossprod(e[e.BRA.20_1])
  curr.sigma.BRA.20_1 <- 1/rgamma(1, sigma_a + length(e.BRA.20_1)/2, sigma_b + as.double(curr.ee.BRA.20_1) / 2)
  curr.ee.BRA.21_1 <- crossprod(e[e.BRA.21_1])
  curr.sigma.BRA.21_1 <- 1/rgamma(1, sigma_a + length(e.BRA.21_1)/2, sigma_b + as.double(curr.ee.BRA.21_1) / 2)
  curr.ee.BRA.22_1 <- crossprod(e[e.BRA.22_1])
  curr.sigma.BRA.22_1 <- 1/rgamma(1, sigma_a + length(e.BRA.22_1)/2, sigma_b + as.double(curr.ee.BRA.22_1) / 2)
  curr.ee.BRA.23_1 <- crossprod(e[e.BRA.23_1])
  curr.sigma.BRA.23_1 <- 1/rgamma(1, sigma_a + length(e.BRA.23_1)/2, sigma_b + as.double(curr.ee.BRA.23_1) / 2)
  curr.ee.BRA.24_1 <- crossprod(e[e.BRA.24_1])
  curr.sigma.BRA.24_1 <- 1/rgamma(1, sigma_a + length(e.BRA.24_1)/2, sigma_b + as.double(curr.ee.BRA.24_1) / 2)
  curr.ee.BRA.25_1 <- crossprod(e[e.BRA.25_1])
  curr.sigma.BRA.25_1 <- 1/rgamma(1, sigma_a + length(e.BRA.25_1)/2, sigma_b + as.double(curr.ee.BRA.25_1) / 2)
  curr.ee.BRA.26_1 <- crossprod(e[e.BRA.26_1])
  curr.sigma.BRA.26_1 <- 1/rgamma(1, sigma_a + length(e.BRA.26_1)/2, sigma_b + as.double(curr.ee.BRA.26_1) / 2)
  curr.ee.BRA.27_1 <- crossprod(e[e.BRA.27_1])
  curr.sigma.BRA.27_1 <- 1/rgamma(1, sigma_a + length(e.BRA.27_1)/2, sigma_b + as.double(curr.ee.BRA.27_1) / 2)
  
  # merge sigmas into vector
  sigma_i <- rep(c(rep(curr.sigma.BRA.1_1, length(e.BRA.1_1)/t), 
                   rep(curr.sigma.BRA.2_1, length(e.BRA.2_1)/t),
                   rep(curr.sigma.BRA.3_1, length(e.BRA.3_1)/t),
                   rep(curr.sigma.BRA.4_1, length(e.BRA.4_1)/t),
                   rep(curr.sigma.BRA.5_1, length(e.BRA.5_1)/t),
                   rep(curr.sigma.BRA.6_1, length(e.BRA.6_1)/t),
                   rep(curr.sigma.BRA.7_1, length(e.BRA.7_1)/t),
                   rep(curr.sigma.BRA.8_1, length(e.BRA.8_1)/t),
                   rep(curr.sigma.BRA.9_1, length(e.BRA.9_1)/t),
                   rep(curr.sigma.BRA.10_1, length(e.BRA.10_1)/t),
                   rep(curr.sigma.BRA.11_1, length(e.BRA.11_1)/t),
                   rep(curr.sigma.BRA.12_1, length(e.BRA.12_1)/t),
                   rep(curr.sigma.BRA.13_1, length(e.BRA.13_1)/t),
                   rep(curr.sigma.BRA.14_1, length(e.BRA.14_1)/t),
                   rep(curr.sigma.BRA.15_1, length(e.BRA.15_1)/t),
                   rep(curr.sigma.BRA.16_1, length(e.BRA.16_1)/t),
                   rep(curr.sigma.BRA.17_1, length(e.BRA.17_1)/t),
                   rep(curr.sigma.BRA.18_1, length(e.BRA.18_1)/t),
                   rep(curr.sigma.BRA.19_1, length(e.BRA.19_1)/t),
                   rep(curr.sigma.BRA.20_1, length(e.BRA.20_1)/t),
                   rep(curr.sigma.BRA.21_1, length(e.BRA.21_1)/t),
                   rep(curr.sigma.BRA.22_1, length(e.BRA.22_1)/t),
                   rep(curr.sigma.BRA.23_1, length(e.BRA.23_1)/t),
                   rep(curr.sigma.BRA.24_1, length(e.BRA.24_1)/t),
                   rep(curr.sigma.BRA.25_1, length(e.BRA.25_1)/t),
                   rep(curr.sigma.BRA.26_1, length(e.BRA.26_1)/t),
                   rep(curr.sigma.BRA.27_1, length(e.BRA.27_1)/t)), t)
  
  # draw beta incl correction for sigmas
  Xtemp <- X/sqrt(sigma_i)
  Ytemp <- curr.Ay/sqrt(sigma_i)
  
  V = solve(beta_prior_var_inv + crossprod(Xtemp))
  b = V %*% (beta_prior_var_inv%*%beta_prior_mean + crossprod(Xtemp,Ytemp))
  curr.beta = MASS::mvrnorm(1,b,V)
  
  ## Griddy-Gibbs step for rho
  Xtemp <- X/sqrt(sigma_i)
  WYtemp <- WY/sqrt(sigma_i)
  Ytemp <- Y/sqrt(sigma_i)
  
  # V = solve(beta_prior_var_inv + crossprod(Xtemp)) # already calculated above
  b0 = V %*% (beta_prior_var_inv%*%beta_prior_mean + crossprod(Xtemp,Ytemp))
  bd = V %*% (beta_prior_var_inv%*%beta_prior_mean + crossprod(Xtemp,WYtemp))
  e0 = Y - X %*% b0
  ed = WY - X %*% bd
  epe0 = as.double(t(e0) %*% e0)
  eped = as.double(t(ed) %*% ed)
  epe0d = as.double(t(ed) %*% e0)
  z = epe0  - 2 * rrhos * epe0d + rrhos^2 * eped
  z = -(n-k)/2 * log(z)
  den = logdets[,1] + z + log(beta_prob(rrhos,rho_a))
  y = rrhos
  adj = max(den)
  den = den - adj
  x = exp(den)
  isum = sum((y[-1] + y[-length(y)])*(x[-1]  - x[-length(x)])/2)
  z = abs(x/isum)
  den = cumsum(z)
  rnd = runif(1) * sum(z)
  ind = max(which(den <= rnd))
  if (is.integer(ind) && ind <= length(rrhos)) {
    curr.rho = rrhos[ind]
    curr.Ay = AYs[,ind]
    curr.ai_diag = ai_diags[ind]
    curr.ai_tot = ai_tots[ind]
    curr.aiW_diag = aiW_diags[ind]
    curr.aiW_tot = aiW_tots[ind]
  }
  
  
  # we are past the burn-in, save the draws
  if (iter > ndiscard) {
    s = iter - ndiscard
    postb[,s] = as.matrix(curr.beta)
    posts[,s] <- c(curr.sigma.BRA.1_1, 
                       curr.sigma.BRA.2_1,
                       curr.sigma.BRA.3_1,
                       curr.sigma.BRA.4_1,
                       curr.sigma.BRA.5_1, 
                       curr.sigma.BRA.6_1,
                       curr.sigma.BRA.7_1,
                       curr.sigma.BRA.8_1, 
                       curr.sigma.BRA.9_1, 
                       curr.sigma.BRA.10_1, 
                       curr.sigma.BRA.11_1, 
                       curr.sigma.BRA.12_1,
                       curr.sigma.BRA.13_1,
                       curr.sigma.BRA.14_1, 
                       curr.sigma.BRA.15_1, 
                       curr.sigma.BRA.16_1, 
                       curr.sigma.BRA.17_1,
                       curr.sigma.BRA.18_1,
                       curr.sigma.BRA.19_1,
                       curr.sigma.BRA.20_1, 
                       curr.sigma.BRA.21_1, 
                       curr.sigma.BRA.22_1,
                       curr.sigma.BRA.23_1,
                       curr.sigma.BRA.24_1,
                       curr.sigma.BRA.25_1,
                       curr.sigma.BRA.26_1,
                       curr.sigma.BRA.27_1)
    postr[s] = curr.rho
    
    # calculate summary spatial effects
    post.direct[,s] = curr.ai_diag/n * curr.beta[1:(smallk + 1)] + 
      c(0,curr.aiW_diag/n * curr.beta[(smallk + 2):(k - ncol(D) - ncol(C))] )
    post.total[,s] = curr.ai_tot/n * curr.beta[1:(smallk + 1)] + 
      c(0,curr.aiW_tot/n * curr.beta[(smallk + 2):(k - ncol(D) - ncol(C))] )
    post.indirect[,s] = post.total[,s] - post.direct[,s]
    
    ### r2 
    ym  = Y - mean(Y)
    resid = as.matrix((diag(n) - curr.rho * W) %*% Y - (X %*% curr.beta))
    ssr = t(resid) %*% resid
    tss = t(ym) %*% ym
    R2 = 1 - ssr / tss
    ssr = ssr/(n-k)
    tss = tss/(n-1)
    R2bar = 1 - ssr/tss
    postrsq[s] = R2
    postrsqbar[s] = R2bar
    
  }
}

# Geweke convergence diagnostic
full_chain_m = cbind(t(postb),t(postr), t(posts))
mh.draws <- coda::mcmc(full_chain_m[,]) # seq(1,1000,by = 10)
gconv = coda::geweke.diag(mh.draws)$z ## z>3 or z<-3 indicates non-convergence!
# indicates convergence

### r2
R2 <- median(postrsq)
R2bar <- median(postrsqbar)


### calculate posterior mean of beta, sigma and rho
beta_post_mean = apply(postb,c(1),mean)
sigma_post_mean = apply(posts,c(1),mean)
sigma_post_sd = apply(posts,c(1),sd)
rho_post_mean = mean(postr)
rho_post_sd = sd(postr)

# spatial effects estimates
direct_post_mean = apply(post.direct,c(1),mean)
indirect_post_mean = apply(post.indirect,c(1),mean)
total_post_mean = apply(post.total,c(1),mean)
direct_post_sd = apply(post.direct,c(1),sd)
indirect_post_sd = apply(post.indirect,c(1),sd)
total_post_sd = apply(post.total,c(1),sd)
