data {
  int N;
  int N2;
  int<lower=0> Gniv1;                  // num de grupos en niv1
  int<lower=1,upper=Gniv1> Niv1[N];
  int<lower=0> Gniv2;                  // num de grupos en niv1
  int<lower=1,upper=Gniv2> Niv2[N];
  vector<lower=0>[N] y_mort; 
  vector<lower=0>[N2] y_hosp; // id de censura (0=obs,1=censd,2=censi)
  int M;                               // n?mero de covariables
  matrix[N, M] x;
  int M_hosp;                               // n?mero de covariables
  matrix[N, M_hosp] x_hosp;// matriz de covariables(con Nren y Mcol)            // matrix of covariates (with n rows and H columns)
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
  vector[Gniv2] mu_l2_raw; // Coeficientes en el predictor lineal
  real<lower=0> alpha_raw;
  vector[M] theta;
  vector[M_hosp] theta_h;// parametro de escala
  vector<lower=0>[2] tau;
  vector<lower=0>[2] stdnormal;
}

transformed parameters {
  //vector[N] linpred;
  real alpha;
  vector[Gniv1] mu_l;
  vector[Gniv2] mu_l2;
  vector<lower=0>[2] sigma;

  for (j in 1:2)
  sigma[j] = stdnormal[j]/sqrt(tau[j]);
  
  alpha=exp(tau_al*alpha_raw);
  mu_l=sigma[1]*mu_l_raw;
  mu_l2=sigma[2]*mu_l2_raw;
  //linpred = x*beta;
 // mu = exp(linpred);

}
model {
  target += normal_lpdf(alpha_raw | 0, 1);
  target += normal_lpdf(mu_raw_mort | 0, tau_mu);
  target += normal_lpdf(mu_raw_hosp | 0, tau_mu);
  target += normal_lpdf(mu_l_raw | 0, 1);
  target += normal_lpdf(mu_l2_raw | 0, 1);
  target += gamma_lpdf(tau | 0.5*3, 0.5*3);
  target += normal_lpdf(stdnormal | 0, 1);
  target += weibull_lpdf(y_mort | alpha, exp(-(Q_ast*theta +mu_raw_mort+mu_l_raw[Niv1]+mu_l2_raw[Niv2])/alpha));
  target += weibull_lpdf(y_hosp | alpha, exp(-(Q_ast_h*theta_h +mu_raw_hosp)/alpha));
}

generated quantities {
  vector[M] beta;
  vector[M_hosp] beta_h;
  beta = R_ast_inverse * theta;
  beta_h = R_ast_inverse_h * theta_h;
}
// generated quantities {
//   vector[N] log_lik;
//   for (i in 1:N) {
//     if (event[i] == 0) {
//       log_lik[i]=weibull_lpdf((y[i] )| alpha, exp(-(mu_raw+mu_l_raw[Niv1]+x*beta)/alpha));
//       }else {
//         if(event[i]==1){
//           log_lik[i]=weibull_lccdf((y[i]) | alpha, exp(-(mu_raw+mu_l_raw[Niv1]+x*beta)/alpha));
//           }else{
//             log_lik[i]=weibull_lcdf((y[i]) | alpha, exp(-(mu_raw+mu_l_raw[Niv1]+x*beta)/alpha));
//             }
//         }
//     }
// }
