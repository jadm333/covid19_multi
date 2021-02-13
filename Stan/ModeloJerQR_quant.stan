data {
  int N;
  int N2;
  int<lower=0> Gniv1;                  // num de grupos en niv1
  int<lower=1,upper=Gniv1> Niv1[N];
  vector<lower=0>[N] y_mort; 
  vector<lower=0>[N2] y_hosp; // id de censura (0=obs,1=censd,2=censi)
  int M;                               // n?mero de covariables
  matrix[N, M] x;
  int M_hosp;                               // n?mero de covariables
  matrix[N, M_hosp] x_hosp;
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
  R_ast = qr_thin_R(x_centered) / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
  Q_ast_h = qr_thin_Q(x_centered_h) * sqrt(N2 - 1);
  R_ast_h = qr_thin_R(x_centered_h) / sqrt(N2 - 1);
  R_ast_inverse_h = inverse(R_ast_h);
  
  tau_mu=3;
  tau_al=10;
}
parameters {
  real mu_raw_mort;
  real mu_raw_hosp;
  vector[Gniv1] mu_l_raw; // Coeficientes en el predictor lineal
  real<lower=0> alpha_raw;
  vector[M] theta; 
  vector[M_hosp] theta_h;
  real<lower=0> tau;
  real<lower=0> stdnormal;
}

transformed parameters {
  real alpha;
  vector[Gniv1] mu_l;
  real<lower=0>sigma;

  sigma = stdnormal/sqrt(tau);
  
  alpha=exp(tau_al*alpha_raw);
  mu_l=sigma*mu_l_raw;

}

generated quantities {
  
  real log_lik[N];
  for(i in 1:N){
    log_lik[i]=weibull_lpdf(y_mort[i] | alpha, exp(-(Q_ast[i]*theta +mu_raw_mort+mu_l_raw[Niv1])/alpha))+
    weibull_lpdf(y_hosp[i] | alpha, exp(-(Q_ast_h[i]*theta_h +mu_raw_hosp)/alpha));
    
  }
}
