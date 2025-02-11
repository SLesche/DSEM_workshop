#Load data
library(dplyr)

dat<-read.csv("https://raw.githubusercontent.com/mearistodemou/DSEM_workshop/main/Data/Two-Level%20Data.csv",header=F) #load McNeish data

#####################
# 1. Rename columns
####################

colnames(dat) <- c('urge',"dep",'js', "hs", 'subject', 'time') #rename columns

#####################
# 2. Subset data
####################

set.seed(5483)
# Get unique account_id
unique_id <- unique(dat$subject)

# 2.1 Subset N = 20
# Randomly select 20 participants
subsample <- sample(unique_id, size = 20)
dat_sub <- dat %>% filter(subject %in% subsample)

# 2.2 Subset data (N = 1)
dat_n1 <- dat_sub[which(dat_sub$subject == 5),]

###############################
# 3. Convert data into a list
##############################

# 3.1 Make list of variables and values for DSEM (N = 20)
dsem_list <- list(N_subj = length(unique(dat_sub$subject)), # subject number
                  Y = matrix(dat_sub$urge, 20, byrow = TRUE), # outcome variable matrix
                  N_obs = length(unique(dat_sub$time))) # number of observations


# 3.2 Make list of variables and values for DSEM (N = 1)
dsem_list_N1 <- list(Y = dat_n1$urge, # outcome variable matrix
                     N_obs = length(unique(dat_n1$time))) # number of observations


model_n1 <- "
  data {
    int<lower=1> N_obs;
    vector[N_obs] Y;
  }
  
  parameters {
    vector[2] gamma;
  }
  
  model {
    real mu;
    real psi;
    
    mu = gamma[1];
    psi = exp(gamma[2]);
    
    gamma ~ normal(0, 1e6);
    
    for (i in 1:N_obs){
      Y[i] ~ normal(mu, psi);
    }
  }
"

# R code for fitting the model here
library(rstan)

mod <- stan(model_code = model_n1, data = dsem_list_N1, 
            verbose = FALSE, iter = 1000, chains = 4, 
            cores = 4, init = 0)
print(mod, digits = 3, pars = c('gamma'))
plot(mod, pars = c('gamma'))

stan_dens(mod, pars = c('gamma'))

## ---- Autoregression
model_n1_autoreg <- "
  data {
    int<lower=1> N_obs;
    vector[N_obs] Y;
  }
  
  parameters {
    vector[3] gamma;
  }
  
  model {
    real mu;
    real psi;
    real phi;
    
    mu = gamma[1];
    psi = exp(gamma[2]);
    phi = gamma[3];
    
    gamma ~ normal(0, 1e6);
    
    Y[1] ~ normal(mu, psi);
    for (i in 2:N_obs){
      Y[i] ~ normal(mu + phi*(Y[i-1] - mu) , psi);
    }
  } 
"
mod <- stan(model_code = model_n1_autoreg, data = dsem_list_N1, 
            verbose = FALSE, iter = 1000, chains = 4, 
            cores = 4, init = 0)
print(mod, digits = 3, pars = c('gamma'))
plot(mod, pars = c('gamma'))

stan_dens(mod, pars = c('gamma'))

## -- Multiple Subjects

model_nmult_autoreg <- "
  data {
    int<lower=1> N_obs;
    int<lower=1> N_subj;
    
    array[N_subj] vector[N_obs] Y;
  }
  
  parameters {
    vector[3] gamma; // Fixed effects
  }
  
  model {
    vector[N_subj] mu;          // mean
    vector[N_subj] psi;         // residual sd
    vector[N_subj] phi;         // autoregression
    
    gamma ~ normal(0, 1e6);     // prior on fixed effects
    
    for (i in 1:N_subj) {
      mu[i] = gamma[1];              // subject-specific mean equals fixed effect
      psi[i] = exp(gamma[2]);        // subject-specific sd equals fixed effect
      phi[i] = gamma[3];             // subject-specific autoregression equals fixed effect
      
      Y[i][1] ~ normal(mu[i], psi[i]); // estimate first observation
      for (t in 2:N_obs) {
        Y[i][t] ~ normal(mu[i] + phi[i]*(Y[i][t-1] - mu[i]), psi[i]);   // three-parameter DSEM
      }
    }
  }
"


mod <- stan(model_code = model_nmult_autoreg, data = dsem_list, 
            verbose = FALSE, iter = 1000, chains = 4, 
            cores = 4, init = 0)
print(mod, digits = 3, pars = c('gamma'))
plot(mod, pars = c('gamma'))

stan_dens(mod, pars = c('gamma'))


## ---- Individual Differences in Parameters?
model_nmult_autoreg <- "
  data {
    int<lower=1> N_obs;
    int<lower=1> N_subj;
    
    array[N_subj] vector[N_obs] Y;
  }
  
  parameters {
    vector[3] gamma; // Fixed effects
    array[N_subj] vector[3] u;
    
    real<lower=0> tau[3]; 
  }
  
  model {
    vector[N_subj] mu;
    vector[N_subj] psi;
    vector[N_subj] phi;
    
    gamma ~ normal(0, 1e6);     // prior on fixed effects
    tau ~ cauchy(0, 2.5);
    
    for (i in 1:N_subj) {
      u[i] ~ normal(0, tau);

      mu[i] = gamma[1] + u[i][1];              // subject-specific mean equals fixed effect
      psi[i] = exp(gamma[2] + u[i][2]);        // subject-specific sd equals fixed effect
      phi[i] = gamma[3] + u[i][3];             // subject-specific autoregression equals fixed effect
      
      Y[i][1] ~ normal(mu[i], psi[i]); // estimate first observation
      for (t in 2:N_obs) {
        Y[i][t] ~ normal(mu[i] + phi[i]*(Y[i][t-1] - mu[i]), psi[i]);   // three-parameter DSEM
      }
    }
  }
"

#Make list of variables and values for DSEM (N = 20)
dsem_list <- list(N_subj = length(unique(dat_sub$subject)), # subject number
                  Y = matrix(dat_sub$urge, 20, byrow = TRUE), # outcome variable matrix
                  N_obs = length(unique(dat_sub$time))) # number of observations

# R code for fitting the model here
mod <- stan(model_code = model_nmult_autoreg, data = dsem_list, 
            verbose = FALSE, iter = 4000, chains = 4, 
            cores = 4, init = 0)

# View results
print(mod, digits = 3, pars = c('gamma','tau'))

stan_dens(mod, pars = c('gamma','tau'))


# ---- Correlation of between-subject variance
model_nmult_autoreg_corr <- "
  data {
    int<lower=1> N_obs;
    int<lower=1> N_subj;
    
    array[N_subj] vector[N_obs] Y;
  }
  
  parameters {
    vector[3] gamma; // Fixed effects
    array[N_subj] vector[3] u;
    
    vector<lower=0>[3] tau; 
    corr_matrix[3] R; // correlation matrix
  }
  
  model {
    vector[N_subj] mu;
    vector[N_subj] psi;
    vector[N_subj] phi;
    
    gamma ~ normal(0, 1e6);     // prior on fixed effects
    tau ~ cauchy(0, 2.5);
    R ~ lkj_corr(2);
    
    for (i in 1:N_subj) {
      u[i] ~ multi_normal(rep_vector(0,3), // vector of means
      quad_form_diag(R,tau)); // Sigma = diag_matrix(tau)* R *diag_matrix(tau);

      mu[i] = gamma[1] + u[i][1];              // subject-specific mean equals fixed effect
      psi[i] = exp(gamma[2] + u[i][2]);        // subject-specific sd equals fixed effect
      phi[i] = gamma[3] + u[i][3];             // subject-specific autoregression equals fixed effect
      
      Y[i][1] ~ normal(mu[i], psi[i]); // estimate first observation
      for (t in 2:N_obs) {
        Y[i][t] ~ normal(mu[i] + phi[i]*(Y[i][t-1] - mu[i]), psi[i]);   // three-parameter DSEM
      }
    }
  }
"
# Randomly select 20 participants (or more up to 100)
subsample <- sample(unique_id, size = 20)
dat_sub <- dat %>% filter(subject %in% subsample)

#Make list of variables and values for DSEM (N = 20)
dsem_list <- list(N_subj = length(unique(dat_sub$subject)), # subject number
                  Y = matrix(dat_sub$urge, 
                             length(unique(dat_sub$subject)), 
                             byrow = TRUE), # outcome variable matrix
                  N_obs = length(unique(dat_sub$time))) # number of observations

# R code for fitting the model here
mod <- stan(model_code = model_nmult_autoreg_corr, data = dsem_list, 
            verbose = FALSE, iter = 4000, chains = 4, 
            cores = 4, init = 0)

# View results
print(mod, digits = 3, pars = c('gamma','tau', 'R'))


# ---- Correlation of between-subject variance
model_nmult_autoreg_corr <- "
  data {
    int<lower=1> N_obs;
    int<lower=1> N_subj;
    
    array[N_subj] vector[N_obs] Y;
  }
  
  parameters {
    vector[3] gamma; // Fixed effects
    array[N_subj] vector[3] u;
    
    vector<lower=0>[3] tau; 
    corr_matrix[3] R; // correlation matrix
  }
  
  model {
    vector[N_subj] mu;
    vector[N_subj] psi;
    vector[N_subj] phi;
    
    gamma ~ normal(0, 1e6);     // prior on fixed effects
    tau ~ cauchy(0, 2.5);
    R ~ lkj_corr(2);
    
    for (i in 1:N_subj) {
      u[i] ~ multi_normal(rep_vector(0,3), // vector of means
      quad_form_diag(R,tau)); // Sigma = diag_matrix(tau)* R *diag_matrix(tau);

      mu[i] = gamma[1] + u[i][1];              // subject-specific mean equals fixed effect
      psi[i] = exp(gamma[2] + u[i][2]);        // subject-specific sd equals fixed effect
      phi[i] = gamma[3] + u[i][3];             // subject-specific autoregression equals fixed effect
      
      Y[i][1] ~ normal(mu[i], psi[i]); // estimate first observation
      for (t in 2:N_obs) {
        Y[i][t] ~ normal(mu[i] + phi[i]*(Y[i][t-1] - mu[i]), psi[i]);   // three-parameter DSEM
      }
    }
  }
"
# Randomly select 20 participants (or more up to 100)
subsample <- sample(unique_id, size = 20)
dat_sub <- dat %>% filter(subject %in% subsample)

#Make list of variables and values for DSEM (N = 20)
dsem_list <- list(N_subj = length(unique(dat_sub$subject)), # subject number
                  Y = matrix(dat_sub$urge, 
                             length(unique(dat_sub$subject)), 
                             byrow = TRUE), # outcome variable matrix
                  N_obs = length(unique(dat_sub$time))) # number of observations

# R code for fitting the model here
mod <- stan(model_code = model_nmult_autoreg_corr, data = dsem_list, 
            verbose = FALSE, iter = 4000, chains = 4, 
            cores = 4, init = 0)

# View results
print(mod, digits = 3, pars = c('gamma','tau', 'R'))
