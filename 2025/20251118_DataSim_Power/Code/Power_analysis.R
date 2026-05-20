#===============================================================================
# Intro to power analysis
#===============================================================================

# Load in packages
library(mvnfast)
library(lmerTest)

#===============================================================================
# Turn simulation into a function and simulate n datasets

n.sim <- 100

sim <- function(
    # Variable parameters
    #Data structure parameters
    n.ind = 100, # Number of individuals
    n.env = 3, # Number of environments
    n.rep = 2, # Number of individual repeats per environment  
    
    # Effect sizes 
    # Fixed effects
    mu = 5, 
    beta1 = 2,
    beta2 = 0.25,
    int1 = 0.1, 
    
    # Random effects
    var_int = sqrt(1.5),
    var_slope = sqrt(0.75),
    cor_int_slope = 0.5 *sqrt(var_int*var_slope),
    
    # Reisudal variance  
    res_var = sqrt(1)  
  )
  {
  # Data structure
  obs_ind <- n.env * n.rep # Total number of individual observations 
  
  df <- data.frame(
    ID = rep(1:n.ind, each = obs_ind),
    Env = rep(rep(1:n.env, times = n.rep), times = n.ind),
    Trial = rep(1:obs_ind, times = n.ind))
  
  # Simuale data
  sigma_I = matrix(c(var_int, cor_int_slope,
                     cor_int_slope, var_slope), 
                   nrow = 2, ncol = 2)
  
  I <- as.data.frame(rmvn(n = n.ind, mu = rep(0,2), sigma = sigma_I))
  colnames(I) <- c("I_mu", "I_beta")

  # Match with ID in df
  I$ID <- 1:n.ind
  df <- merge(df, I, by = "ID")
  
  # Residual variance
  df$Res <- rnorm(nrow(df), 0, res_var) 
  
  # Check data structure again
  head(df, n = 12)
  
  # Scale environmental covariate for simplicity
  df$Env <- as.vector(scale(df$Env))
  
  # Generate observed trait values (phenotype)
  df$z <- mu + df$I_mu + (beta1 + df$I_beta) * df$Env + 
    beta2 * df$Trial + int1 *(df$Env * df$Trial) + df$Res 
  
  df
}

#===============================================================================
# Simulate data with function
test <- sim()
head(test)

# Simualte data n.sim times

# Using a for loop (can be slow)
dat <- vector("list", length = n.sim)

for(i in 1:n.sim){
  dat[[i]] <- sim()
}

# using replicate
dat <- replicate(n.sim, sim(), simplify = FALSE)

#===============================================================================
# Fit model across simulated list

output <- lapply(seq_along(dat), function(i) {
  md <- lmerTest::lmer(z ~ Env * Trial + (Env | ID), data = dat[[i]])
  coefs <- as.data.frame(summary(md)$coef)
  coefs$param <- row.names(coefs)
  coefs$n <- i 
  coefs
})

# unlist
output <- do.call(rbind, output)

# Calculate power per fixed effect
alpha <- 0.05

output$sig <- as.numeric(output[, "Pr(>|t|)"]) < alpha

sum(output[output$param == "Env", "sig"])
sum(output[output$param == "Trial", "sig"])
sum(output[output$param == "Env:Trial", "sig"])

#===============================================================================
# Bayesian framework with BRMS

library(brms)
library(rstan)

# Get stan model
dat1 <- dat[[i]]
md_brms <- brms::brm(z ~ Env * Trial + (Env | ID), data = dat1,
                     chains = 1, iter = 10)
brm_mod <- rstan::stan_model(model_code = stancode(md_brms)) 

standata <- brms::make_standata(z ~ Env * Trial + (Env | ID), data = dat1)

md <- rstan::sampling(brm_mod, data = standata,
                      pars = c("Intercept", "b","cor_1"),
                      chains = 1, iter = 3000, warmup = 1000,
               thin = 1, cores = 1, save_warmup = FALSE)


brms_func <- function(dat) {
  # library(rstan)
  # library(brms)
  
  standata <- brms::make_standata(z ~ Env * Trial + (Env | ID), data = dat)
  
  md <- sampling(brm_mod, data = standata,
                 pars = c("Intercept", "b","cor_1"),
                 chains = 1, iter = 3000, warmup = 1000, 
                 thin = 1, cores = 1, save_warmup = FALSE)
  
  summ <- summary(md)$summary
  param_names <- rownames(summ)
  
  var <- as.data.frame(round(summ[, c(1, 4, 6, 8, 9, 10)], 3))
  var$Parameter <- param_names
  var$n_div <- rep(rstan::get_num_divergent(md), nrow(var)) # Get n divergencies
  return(var)
}

brms_output <- lapply(dat, brms_func)
brms_output <- do.call(rbind, brms_output)

# Check whether lower bound overlaps with 0 (all simulated effects are positive)
brms_output$sig <- brms_output$"2.5%" > 0 

sum(brms_output[brms_output$Parameter == "Intercept", "sig"])
sum(brms_output[brms_output$Parameter == "b[1]", "sig"])
sum(brms_output[brms_output$Parameter == "b[2]", "sig"])
sum(brms_output[brms_output$Parameter == "b[3]", "sig"])
sum(brms_output[brms_output$Parameter == "cor_1[1]", "sig"])

# How does the spread around the median estimate change with sample size and study design?
# Bias & precision

#================================================================================
# Parralel fitting, more efficient than sequential
# library(parallel)
# library(future)
# library(future.apply)
# 
# # Specify parallel worker (cores)
# plan(multisession, workers = 5)
# 
# # Run stan sem for simulated datalist ---
# output_list_sim <- future_lapply(
#   dat,
#   brms_func,
#   future.globals = list(brm_mod = brm_mod),
#   future.seed = TRUE,
#   options(future.debug = TRUE)
# )

#===============================================================================
