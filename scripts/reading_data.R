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
