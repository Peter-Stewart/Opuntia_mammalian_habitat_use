functions{
  matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
    int N = dims(x)[1];
    matrix[N, N] K;
    for (i in 1:(N-1)) {
      K[i, i] = sq_alpha + delta;
      for (j in (i + 1):N) {
        K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
        K[j, i] = K[i, j];
      }
    }
    K[N, N] = sq_alpha + delta;
    return K;
  }
}

data{
  // Observation and season indexes
  int<lower=1> n_obs; // Total number of observations
  int<lower=1> n_site; // Total number of sites
  array[n_obs] int<lower=1> site_ind; // Site index for varying intercept (NB: not the actual site ID!)
  int<lower=1> nseasons; // Number of seasons
  array[n_obs] int<lower=1> season; // Season ID for each observation
  
  // Observed data
  array[n_obs] int night; // Number of detections at night
  array[n_obs] int surveyed; // Total number of detections
  
  // Covariates
  array[n_obs] real opuntia; // Opuntia cover or volume (standardised)
  array[n_obs] real d_water; // Distance to nearest river (standardised)
  array[n_obs] real d_road; // Distance to nearest road (standardised)
  array[n_obs] real livestock; // Proportion of days where livestock are present (standardised)
  array[n_obs] real moon; // Lunar illumination (1 = full moon)
  
  // Distance matrix
  matrix[n_site, n_site] dmat;
}
parameters{
  // Parameters for mu
  vector[nseasons] alpha_bar; // Global intercept
  
  vector[nseasons] beta_opuntia; // Effect of Opuntia for each season
  vector[nseasons] beta_d_water; // Effect of distance to water
  vector[nseasons] beta_d_road; // Effect of distance to road
  vector[nseasons] beta_livestock; // Effect of livestock

  
  vector[nseasons] beta_moon; // Effect of moonlight 
  vector[nseasons] gamma_moon; // Moonlight*Opuntia interaction term

  // Gaussian process parameters
  vector[n_site] z; // z-scores for intercept term (for non-centred parameterisation)
  real<lower=0> etasq; // Maximum covariance between sites
  real<lower=0> rhosq; // Rate of decline in covariance with distance
}
model{
  // Model parameters
  vector[n_obs] theta;
  
  matrix[n_site, n_site] L_SIGMA; // Cholesky-decomposed covariance matrix
  matrix[n_site, n_site] SIGMA; // Covariance matrix
  vector[n_site] k; // Intercept term for each site (offset from k_bar)
  
  // Priors
  alpha_bar ~ normal(0, 1.5);
  
  beta_opuntia ~ normal(0,0.5);
  beta_d_water ~ normal(0,0.5);
  beta_d_road ~ normal(0,0.5);
  beta_livestock ~ normal(0,0.5);
  beta_moon ~ normal(0,0.5);
  gamma_moon ~ normal(0,1);
  
  etasq ~ exponential(2);
  rhosq ~ lognormal(0, 1);
  z ~ normal(0, 1);
  
  // Gaussian process - non-centred
  SIGMA = cov_GPL2(dmat, etasq, rhosq, 0.01);
  L_SIGMA = cholesky_decompose(SIGMA);
  k = L_SIGMA * z;
  
  // Theta
  for(i in 1:n_obs){
    theta[i] = inv_logit(alpha_bar[season[i]] + k[site_ind[i]] +
                           opuntia[i]*moon[i]*gamma_moon[season[i]] + 
                           opuntia[i]*beta_opuntia[season[i]] + 
                           moon[i]*beta_moon[season[i]] + 
                           d_water[i]*beta_d_water[season[i]] +
                           d_road[i]*beta_d_road[season[i]] + 
                           livestock[i]*beta_livestock[season[i]]);
  }
  
  // Likelihood
  night ~ binomial(surveyed, theta);
}
