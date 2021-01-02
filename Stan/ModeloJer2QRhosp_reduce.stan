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
  int<lower=0> Gniv1;                  // num de grupos en niv1
  int<lower=1,upper=Gniv1> Niv1[N];
  int<lower=0> Gniv2;                  // num de grupos en niv1
  int<lower=1,upper=Gniv2> Niv2[N];
  int<lower=0> Gnivh;                  // num de grupos en niv1
  int<lower=1,upper=Gnivh> Nivh1[N2];
  int<lower=0> Gnivh2;                  // num de grupos en niv1
  int<lower=1,upper=Gnivh2> Nivh2[N];
  //vector<lower=0>[N] y_mort; 
  real<lower=0> y_mort[N];
  //vector<lower=0>[N2] y_hosp; // id de censura (0=obs,1=censd,2=censi)
  real<lower=0> y_hosp[N];
  int M;                               // n?mero de covariables
  matrix[N, M] x;
  int M_hosp;                               // n?mero de covariables
  matrix[N2, M_hosp] x_hosp;// matriz de covariables(con Nren y Mcol)            // matrix of covariates (with n rows and H columns)
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
  vector[Gniv1] mu_l_raw;
  vector[Gniv1] mu_l_raw_h;
  vector[Gniv2] mu_l2_raw;
  vector[Gniv2] mu_l2_raw_h;// Coeficientes en el predictor lineal
  real<lower=0> alpha_raw;
  vector[M] theta;
  vector[M_hosp] theta_h;// parametro de escala
  vector<lower=0>[4] tau;
  vector<lower=0>[4] stdnormal;
}

transformed parameters {
  //vector[N] linpred;
  real alpha;
  vector[Gniv1] mu_l;
  vector[Gniv1] mu_l_h;
  vector[Gniv2] mu_l2;
  vector[Gniv2] mu_l2_h;
  vector<lower=0>[4] sigma;

  for (j in 1:4)
  sigma[j] = stdnormal[j]/sqrt(tau[j]);
  
  alpha=exp(tau_al*alpha_raw);
  mu_l=sigma[1]*mu_l_raw;
  mu_l2=sigma[2]*mu_l2_raw;
  mu_l_h=sigma[3]*mu_l_raw_h;
  mu_l2_h=sigma[4]*mu_l2_raw_h;
  //linpred = x*beta;
 // mu = exp(linpred);

}
model {
  int grainsize=1;
  target += normal_lpdf(alpha_raw | 0, 1);
  target += normal_lpdf(mu_raw_mort | 0, tau_mu);
  target += normal_lpdf(mu_raw_hosp | 0, tau_mu);
  target += normal_lpdf(mu_l_raw | 0, 1);
  target += normal_lpdf(mu_l_raw_h | 0, 1);
  target += normal_lpdf(mu_l2_raw | 0, 1);
  target += normal_lpdf(mu_l2_raw_h | 0, 1);
  target += gamma_lpdf(tau | 0.5*3, 0.5*3);
  target += normal_lpdf(stdnormal | 0, 1);
  //target += weibull_lpdf(y_mort | alpha, exp(-(Q_ast*theta +mu_raw_mort+mu_l_raw[Niv1]+mu_l2_raw[Niv2])/alpha));
  target += reduce_sum(partial_sum,y_mort,grainsize,alpha,(Q_ast*theta +mu_raw_mort+mu_l[Niv1]+mu_l2[Niv2])/alpha);
  //target += weibull_lpdf(y_hosp | alpha, exp(-(Q_ast_h*theta_h +mu_raw_hosp)/alpha));
  target += reduce_sum(partial_sum,y_hosp,grainsize,alpha,(Q_ast_h*theta_h +mu_raw_hosp+mu_l_h[Nivh1]+
  mu_l2_h[Nivh2])/alpha);
}

generated quantities {
  vector[M] beta;
  vector[M_hosp] beta_h;
  real log_lik[N];
  real<lower=0> y_mort_tilde[N];
  real<lower=0> y_hosp_tilde[N];
  
  
  beta = R_ast_inverse * theta;
  beta_h = R_ast_inverse_h * theta_h;
  
  for(i in 1:500){
    log_lik_mort[i]=weibull_lpdf(y_mort[i] | alpha, exp(-(Q_ast[i]*theta +mu_raw_mort+mu_l[Niv1[i]]+
    mu_l2[Niv2[i]])/alpha));
    y_mort_tilde[i]=weibull_rng(alpha,exp(-(Q_ast[i]*theta +mu_raw_mort+mu_l[Niv1[i]]+
    mu_l2[Niv2[i]])/alpha));
  }
  
  for(i in 1:500){
    log_lik_hosp[i]=weibull_lpdf(y_hosp[i] | alpha, exp(-(Q_ast_h[i]*theta_h +mu_raw_hosp+
    mu_l_h[Nivh1[i]]+mu_l2_h[Nivh2[i]])/alpha));
    y_hosp_tilde[i]=weibull_rng(alpha,exp(-(Q_ast_h[i]*theta_h +mu_raw_hosp+
    mu_l_h[Nivh1[i]]+mu_l2_h[Nivh2[i]])/alpha));
  }
  
}
