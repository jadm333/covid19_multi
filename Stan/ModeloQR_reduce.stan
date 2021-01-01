functions {
  real partial_sum(real[] y_slice,
                   int start, int end,
                   real alpha,
                   vector Q) {
    return weibull_lpdf(y_slice | alpha, exp(-Q[start:end]));
  }
}
data {
  int N;
  int N2;
  //vector<lower=0>[N] y_mort; 
  real<lower=0> y_mort[N]; 
  //vector<lower=0>[N2] y_hosp; // id de censura (0=obs,1=censd,2=censi)
  real<lower=0> y_hosp[N2]; // id de censura (0=obs,1=censd,2=censi)
  int M;                               // n?mero de covariables
  matrix[N, M] x;
  int M_hosp;                               // n?mero de covariables
  matrix[N, M_hosp] x_hosp; // matriz de covariables(con Nren y Mcol)            // matrix of covariates (with n rows and H columns)
}
transformed data {
  real<lower=0> tau_mu;
  real<lower=0> tau_al;
  matrix[N, M] Q_ast;
  matrix[M, M] R_ast;
  matrix[M, M] R_ast_inverse;
  matrix[N, M] x_centered;
  matrix[N2, M_hosp] Q_ast_h;
  matrix[M_hosp, M_hosp] R_ast_h;
  matrix[M_hosp, M_hosp] R_ast_inverse_h;
  matrix[N2, M_hosp] x_centered_h;
  
  for (m in 1:M)
    x_centered[,m] = x[,m] - mean(x[,m]);
  for (m in 1:M_hosp)
    x_centered_h[,m] = x_hosp[,m] - mean(x_hosp[,m]);
  
  // thin and scale the QR decomposition
  Q_ast = qr_thin_Q(x_centered) * sqrt(N - 1);
  //Q_ast = qr_thin_Q(x)*sqrt(N-1);
  R_ast = qr_thin_R(x_centered) / sqrt(N - 1);
  //R_ast = qr_thin_R(x)/sqrt(N-1);
  R_ast_inverse = inverse(R_ast);
  Q_ast_h = qr_thin_Q(x_centered_h) * sqrt(N2 - 1);
  //Q_ast_h = qr_thin_Q(x_hosp) * sqrt(N2 - 1);
  R_ast_h = qr_thin_R(x_centered_h) / sqrt(N2 - 1);
  //R_ast_h = qr_thin_R(x_hosp) / sqrt(N2 - 1);
  R_ast_inverse_h = inverse(R_ast_h);
  
  tau_mu=3;
  tau_al=10;
}
parameters {
  real mu_raw_mort;
  real mu_raw_hosp; // Coeficientes en el predictor lineal
  real<lower=0> alpha_raw;
  vector[M] theta;
  //real theta[M];
  //matrix[M,1] theta;
  vector[M_hosp] theta_h;
}

transformed parameters {
  //vector[N] linpred;
  //matrix[N,M] Q_mort;
  real alpha;
  
  //for(i in 1:M){
  //  theta[i] = 1.0;
  //  theta_h[i] = 1;
  //  
  //}
  
  alpha=exp(tau_al*alpha_raw);
  //Q_mort = (Q_ast*theta+ mu_raw_mort)/alpha;
  //linpred = x*beta;
 // mu = exp(linpred);

}
model {
  int grainsize= 1;
  target += normal_lpdf(alpha_raw | 0, 1);
  target += normal_lpdf(mu_raw_mort | 0, tau_mu);
  target += normal_lpdf(mu_raw_hosp | 0, tau_mu);
  //target += weibull_lpdf(y_mort | alpha, exp(-(Q_ast*theta +mu_raw_mort)/alpha));
  target += reduce_sum(partial_sum,y_mort,grainsize,alpha,(Q_ast*theta + mu_raw_mort)/alpha);
  //target += weibull_lpdf(y_hosp | alpha, exp(-(Q_ast_h*theta_h +mu_raw_hosp)/alpha));
  target += reduce_sum(partial_sum,y_hosp,grainsize,alpha, (Q_ast_h*theta_h +mu_raw_hosp)/alpha);
}

generated quantities {
  vector[M] beta;
  vector[M_hosp] beta_h;
  real log_lik_mort[N];
  real log_lik_hosp[N2];
  real<lower=0> y_mort_tilde[N];
  real<lower=0> y_hosp_tilde[N2];
  
  
  beta = R_ast_inverse * theta;
  beta_h = R_ast_inverse_h * theta_h;
  
  for(i in 1:N){
    log_lik_mort[i]=weibull_lpdf(y_mort[i] | alpha, exp(-(Q_ast[i]*theta +mu_raw_mort)/alpha));
    y_mort_tilde[i]=weibull_rng(alpha,exp(-(Q_ast[i]*theta +mu_raw_mort)/alpha));
  }
  
  for(i in 1:N2){
    log_lik_hosp[i]=weibull_lpdf(y_hosp[i] | alpha, exp(-(Q_ast_h[i]*theta_h +mu_raw_hosp)/alpha));
    y_hosp_tilde[i]=weibull_rng(alpha,exp(-(Q_ast_h[i]*theta_h +mu_raw_hosp)/alpha));
  }
  

}
