


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

### calibration parameters for rho sampling
cc = 1 # scaling of rho proposals
c_adjust = 1.1 # proposal distribution adjustment
rho_accept = 0 # counter for rho acceptance rates

### set-up the gibbs sampler
# total number of draws
niter = 2000
# retain only S2 and discard S1, with S = S1 + S2
nretain = 1000
ndiscard = niter - nretain
# save the posterior draws here
postb = matrix(0,k,nretain)
posts = matrix(0,3,nretain)
postr = matrix(0,1,nretain)
postrsq = matrix(0,1,nretain)
postrsqbar = matrix(0,1,nretain)
postbic = matrix(0,1,nretain)
postaic = matrix(0,1,nretain)

post.direct = matrix(0,smallk + 1,nretain)
post.indirect = matrix(0,smallk + 1,nretain)
post.total = matrix(0,smallk + 1,nretain)

# set-up for griddy gibbs
griddy_n = 500
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
  A = (.sparseDiagonal(n) - rrhos[ii] * W)
  AYs[,ii] = as.matrix(A %*% Y)
  AI = solve(A)
  ai_diags[ii] = sum(diag(AI))
  ai_tots[ii] = sum(AI)
  aiW_diags[ii] = sum(diag(AI %*% W))
  aiW_tots[ii] = sum(AI %*% W)
}
cat("Done!\n")

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
nregs <- length(W_str$GID_0)
nyrs <- n / nregs
str_panel <- do.call("rbind", replicate(nyrs, W_str, simplify = FALSE))
e.mex <- str_panel %>%
  dplyr::mutate(id = as.numeric(rownames(str_panel))) %>%
  dplyr::filter(GID_0 == "MEX") %>%
  dplyr::select(id) %>% unlist()
e.per <- str_panel %>%
  dplyr::mutate(id = as.numeric(rownames(str_panel))) %>%
  dplyr::filter(GID_0 == "PER") %>%
  dplyr::select(id) %>% unlist()
e.chl <- str_panel %>%
  dplyr::mutate(id = as.numeric(rownames(str_panel))) %>%
  dplyr::filter(GID_0 == "CHL") %>%
  dplyr::select(id) %>% unlist()


### Gibbs sampling
for (iter in 1:niter) {
  cat("iter:",iter,"curr.rho:",curr.rho,"\n")
  
  curr.xb = X %*% curr.beta
  
  # calculate resisuals and country-specific sigma
  e <- curr.Ay - curr.xb
  curr.ee.mex <- crossprod(e[e.mex])
  curr.sigma.mex <- 1/rgamma(1, sigma_a + length(e.mex)/2, sigma_b + as.double(curr.ee.mex) / 2)
  curr.ee.per <- crossprod(e[e.per])
  curr.sigma.per <- 1/rgamma(1, sigma_a + length(e.per)/2, sigma_b + as.double(curr.ee.per) / 2)
  curr.ee.chl <- crossprod(e[e.chl])
  curr.sigma.chl <- 1/rgamma(1, sigma_a + length(e.chl)/2, sigma_b + as.double(curr.ee.chl) / 2)
  
  # merge sigmas into vector
  sigma_i <- rep(c(rep(curr.sigma.mex, length(e.mex)/t), rep(curr.sigma.per, length(e.per)/t), rep(curr.sigma.chl, length(e.chl)/t)), t)
  
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
  
  V = solve(beta_prior_var_inv + crossprod(Xtemp))
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
    posts[,s] <- c(curr.sigma.mex,curr.sigma.per,curr.sigma.chl)
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
