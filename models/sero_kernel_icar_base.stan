
data {
  int<lower=1> N;                    // number of serosurveys
  int<lower=0> y[N];                 // positives
  int<lower=1> n[N];                 // tested

  int<lower=1> G;                    // number of grid cells 
  int<lower=1, upper=G> cell[N];     // grid cell index

  int<lower=1> E;                    // number of adjacency edges total
  int<lower=1, upper=G> edge_u[E];   // ICAR edge start
  int<lower=1, upper=G> edge_v[E];   // ICAR edge end

  int<lower=0> K;                    // number of outbreak-survey pairs
  int<lower=1> start[N+1];           // index pointers
  vector<lower=0>[K] dist;           // spatial distances
  vector<lower=0>[K] dt;             // time lags
  vector<lower=0>[K] mag;            // outbreak magnitudes (cases or 1 if not using)

  int<lower=1> C;                    // number of separated clusters of grid cells
  int<lower=1, upper=C> comp_id[G];  // cluster group membership id
}

parameters {
  real beta0;                        // global intercept (baseline seroprevalence)
  real alpha;                        // scaling of kernel sum (outbreak impact based on space and time) into impact on seroprevalence
   
  real log_rho_s;                    // exponential spatial decay (fit on log scale)
  real log_rho_t;                    // exponential temporal decay (fit on log scale)

  real<lower=1e-6> sigma_b;          // spatial field variance parameter (think of it something like a random effect variance)
  vector[G] b_raw;                   // raw per-grid cell background adjustment to seroprevalence (e.g., the conditional modes of the random effect)
}

transformed parameters {

  real<lower=1e-6> rho_s = exp(log_rho_s);  // transform to positive for likelihood
  real<lower=1e-6> rho_t = exp(log_rho_t);  // transform to positive for likelihood

  vector[G] b_tilde;                        // scaled b 
  vector[G] b;                              // final b (scaled conditional mode)
  

  //// centering per grid cell group of b_raw
  {
    vector[C] sum_c = rep_vector(0, C);
    vector[C] n_c   = rep_vector(0, C);

    for (g in 1:G) {
      sum_c[comp_id[g]] += b_raw[g];
      n_c[comp_id[g]]   += 1;
    }

    for (g in 1:G) {
      b_tilde[g] = b_raw[g] - sum_c[comp_id[g]] / n_c[comp_id[g]];
    }

  }

  b = sigma_b * b_tilde;              // final b
}

model {

  //// Priors
 
  beta0 ~ normal(0, 2.5);             // prior on global intercept
  alpha ~ normal(0, 2.0);             // prior on kernel scaling 

  log_rho_s ~ normal(log(100), 0.7);  // distance units consistent with dist
  log_rho_t ~ normal(log(6), 0.7);    // time units consistent with dt

  sigma_b ~ normal(0, 0.5);           // half-normal prior on variance. Smaller variance leads to smoother layer

  b_raw ~ normal(0, 2);               // stabilize each of the conditional modes

  real lambda = 0.05;                 // ICAR prior: penalty on neighbor differences for b_tilde

  target += -0.5 * dot_self(b_tilde[edge_u] - b_tilde[edge_v]) -0.5 * lambda * dot_self(b_tilde);


  /// Likelihood

  for (i in 1:N) {
    real Ei = 0;
    int s = start[i];
    int e = start[i+1] - 1;
    
    // The cumulative impact across all of the linked outbreaks (those under the threshold)
    if (K > 0 && s <= e) {
      for (k in s:e) {
        Ei += mag[k] * exp(-dist[k] / rho_s - dt[k] / rho_t);
      }
    }
    y[i] ~ binomial_logit(n[i], beta0 + b[cell[i]] + alpha * Ei);
  }
}

generated quantities {

  vector[N] log_lik;
  for (i in 1:N) {
    real Ei = 0;
    int s = start[i];
    int e = start[i+1] - 1;
    if (K > 0 && s <= e) {
      for (k in s:e) {
        Ei += mag[k] * exp(-dist[k] / rho_s - dt[k] / rho_t);
      }
    }
    log_lik[i] = binomial_logit_lpmf(y[i] | n[i], beta0 + b[cell[i]] + alpha * Ei);
  }

}
