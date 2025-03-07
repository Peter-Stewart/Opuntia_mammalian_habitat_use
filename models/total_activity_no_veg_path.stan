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
  array[n_obs] int detections; // Total number of detections

  // Covariates
  array[n_obs] real opuntia; // Opuntia cover or volume (standardised)
  array[n_obs] real d_water; // Distance to nearest river (standardised)
  array[n_obs] real d_road; // Distance to nearest road (standardised)
  array[n_obs] real livestock; // Proportion of days where livestock are present (standardised)
  array[n_obs] real grass; // Grass % cover (standardised)
  array[n_obs] real forb; // Forb % cover (standardised)
  array[n_obs] real shrub; // Shrub % cover (standardised)
  array[n_obs] real succulent; // Succulent % cover (standardised)
  array[n_obs] real tree; // Tree % cover (standardised)

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
  vector[nseasons] beta_grass; // Effect of grass
  vector[nseasons] beta_forb; // Effect of forb
  vector[nseasons] beta_shrub; // Effect of shrub
  vector[nseasons] beta_succulent; // Effect of succulent
  vector[nseasons] beta_tree; // Effect of trees 
  
  real<lower=0> phi; // Variance parameter for negative binomial
  
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
  alpha_bar ~ normal(7, 3);

  beta_opuntia ~ normal(0,0.5);
  beta_d_water ~ normal(0,0.5);
  beta_d_road ~ normal(0,0.5);
  beta_livestock ~ normal(0,0.5);
  beta_grass ~ normal(0,0.5);
  beta_forb ~ normal(0,0.5);
  beta_shrub ~ normal(0,0.5);
  beta_succulent ~ normal(0,0.5);
  beta_tree ~ normal(0,0.5);
  
  phi ~ exponential(1);

  etasq ~ exponential(2);
  rhosq ~ lognormal(0, 1);
  z ~ normal(0, 1);
  
  // Gaussian process - non-centred
  SIGMA = cov_GPL2(dmat, etasq, rhosq, 0.01);
  L_SIGMA = cholesky_decompose(SIGMA);
  k = L_SIGMA * z;
  
  // Theta
  for(i in 1:n_obs){
    theta[i] = exp(alpha_bar[season[i]] + k[site_ind[i]] +
                           opuntia[i]*beta_opuntia[season[i]] + 
                           d_water[i]*beta_d_water[season[i]] +
                           d_road[i]*beta_d_road[season[i]] + 
                           livestock[i]*beta_livestock[season[i]] +
                           grass[i]*beta_grass[season[i]] +
                           forb[i]*beta_forb[season[i]] +
                           shrub[i]*beta_forb[season[i]] +
                           succulent[i]*beta_succulent[season[i]] +
                           tree[i]*beta_tree[season[i]]);
  }
  
  // Likelihood
  detections ~ neg_binomial_2(theta, phi);
}
