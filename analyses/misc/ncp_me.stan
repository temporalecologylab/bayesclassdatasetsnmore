/* How to do a non-centered parameterization of your stan model 
   Copied with small edits from from git/teaching/stan/ */

   /* See also: https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html */

/* NOTE! This code is not designed to run, it's designed to compare models */

/* What should NOT change when you change model to NCP:
      - data block
      - you'll have the same # of params, but they mean something different
      - your y = a + xb bit
   And what WILL change
      - you will change the name of some params in the param block, those old params are defined in the transformed param block
      - speaking of which, you need a transformed parameter block
      - you should NOT have param ~ N(mu, sigma) in your model block 
      - I believe that all pararms in transformed params must be declared together ...
        so you may have to move up the yhat */

/* Away we go ... */ 

// Centered parameterization

data {
  int<lower=0> J;          // number of schools
  real y[J];               // estimated treatment effect (school j)
  real<lower=0> sigma[J];  // std err of effect estimate (school j)
}
parameters {
  // "local" parameters
  vector[J] theta;
  
  // "global" parameters
  real mu; // mean
  real<lower=0> tau; // sd
}
model {
  // Data model
  y ~ normal(theta, sigma);
  
  // Priors 
  theta ~ normal(mu, tau);
  tau ~ cauchy(0, 10);
  // can also put prior on mu
}


// Non-centered parameterization

data {
  int<lower=0> J;          // number of schools
  real y[J];               // estimated treatment effect (school j)
  real<lower=0> sigma[J];  // std err of effect estimate (school j)
}
parameters {
  vector[J] theta_raw; /* HERE WE CHANGE*/
  real mu;
  real<lower=0> tau;
}
transformed parameters {
  vector[J] theta; /* HERE WE CHANGE*/
  theta = mu + tau * theta_raw; /* HERE WE CHANGE: Relate new theta to old theta*/
}
model {
  // Data model
  y ~ normal(theta, sigma); /* HERE WE CHANGE: no theta ~ normal(mu, tau) anymore*/
  
  // Priors: Define all params! 
  theta_raw ~ normal(0, 1);  // implies theta ~ normal(mu, tau)
  tau ~ cauchy(0, 10);
  // can also put prior on mu
}


