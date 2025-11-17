#===============================================================================
# Intro to data simulation
#===============================================================================

# Load in packages
library(mvnfast)
library(lmerTest)

#===============================================================================
# Testing out different distributions

# Normal distribution
y_norm <- rnorm(n = 1000, mean = 0, sd = 1)

hist(y_norm)
mean(y_norm)
sd(y_norm)

# Binomial distribution
y_binom <- rbinom(n = 1000, size = 1, prob = 0.5)

table(y_binom)

y_binom2 <- rbinom(n = 1000, size = 10, prob = 0.5)
hist(y_binom2)

# Poisson distribution
y_pois <- rpois(n = 1000, lambda = 5)

hist(y_pois)
mean(y_pois)
var(y_pois)

# Multinormal distribution
sigma_mat <- matrix(c(1,0.75,0.75,1), nrow = 2, ncol = 2)
y_mvn <- mvnfast::rmvn(n = 1000, mu = rep(0,2), sigma = sigma_mat)

hist(y_mvn[,1])
hist(y_mvn[,2])
plot(y_mvn)

cor(y_mvn[,1], y_mvn[,2])
cov(y_mvn[,1], y_mvn[,2])

#===============================================================================
# Generate data structure

n.ind <- 100 # Number of individuals

n.env <- 3 # Number of environments
n.rep <- 2 # Number of individual repeats per environment  

obs_ind <- n.env * n.rep # Total number of individual observations 

df <- data.frame(
  ID = rep(1:n.ind, each = obs_ind),
  Env = rep(rep(1:n.env, times = n.rep), times = n.ind),
  Trial = rep(1:obs_ind, times = n.ind))

# Check whether our data structures matches
head(df, n = 12)

#===============================================================================
# Set parameter values

# Fixed effects
mu <- 5 
  
beta1 <- 2
beta2 <- 0.25

int1 <- 0.1 

# Random effects
var_int <- sqrt(1.5)
var_slope <- sqrt(0.75)

cor_int_slope <- 0.5 *sqrt(var_int*var_slope)

sigma_I <- matrix(c(var_int, cor_int_slope,
                    cor_int_slope, var_slope), 
                  nrow = 2, ncol = 2)
  
# Reisudal variance  
res_var <- sqrt(1)  

#===============================================================================
# Simulate data

I <- as.data.frame(rmvn(n = n.ind, mu = rep(0,2), sigma = sigma_I))
colnames(I) <- c("I_mu", "I_beta")
cor(I) # Check whether the correlation matches set values

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

hist(df$z)

#===============================================================================
# Fit model

md <- lmerTest::lmer(z ~  Env*Trial + (Env|ID), data = df)

summary(md)
summary(md)$coef
VarCorr(md)

#===============================================================================
