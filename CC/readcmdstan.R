library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(tidybayes)
library(readr)
library(rjson)
library(loo)

#set_cmdstan_path()

###multi

mod_jer2modi <- cmdstan_model("jer2modi/ModeloJer2QRhosp_quant.stan")
json_data <- fromJSON(file="jer2modi/jer_2modi.json")
fit_jer2modi <- mod_jer2modi$generate_quantities(c("jer2modi/jer2modi_1.csv","jer2modi/jer2modi_2.csv",
                                       "jer2modi/jer2modi_3.csv"), data = "jer2modi/jer_2modi.json",  
                                     parallel_chains = 3)

y_rep_hosp=fit_jer2modi$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)
y_rep_mort=fit_jer2modi$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

loo_plot <- ppc_dens_overlay(json_data$y_mort,y_rep_mort[1:200,])

loo_hosp_jer2=loo(fit_jer2modi$draws("log_lik_hosp"), r_eff = NA)
print(loo_hosp_jer2)
loo_mort_jer2=loo(fit_jer2modi$draws("log_lik_mort"), r_eff = NA)
print(loo_mort_jer2)


#jer2
mod_jer2 <- cmdstan_model("jer2/ModeloJer2QR_quant.stan")
fit_jer2<- mod_jer2$generate_quantities(c("jer2/jer2_1.csv","jer2/jer2_2.csv",
                                                   "jer2/jer2_3.csv"), data = "jer2modi/jer_2modi.json",  
                                                 parallel_chains = 3)

loo_jer2=loo(fit_jer2$draws("log_lik"), r_eff = NA)
print(loo_jer2)



#jer1
mod_jer1 <- cmdstan_model("jer1/ModeloJerQR_quant.stan")
fit_jer1<- mod_jer1$generate_quantities(c("jer1/jer1_1.csv","jer1/jer1_2.csv",
                                          "jer1/jer1_3.csv"), data = "jer2modi/jer_2modi.json",  
                                        parallel_chains = 3)

loo_jer1=loo(fit_jer1$draws("log_lik"), r_eff = NA)
print(loo_jer1)



#sinjer
mod_sinjer <- cmdstan_model("sinjer/ModeloQR_quant.stan")
fit_sinjer<- mod_jer1$generate_quantities(c("sinjer/sin_jer_1.csv","sinjer/sin_jer_2.csv",
                                          "sinjer/sin_jer_3.csv"), data = "jer2modi/jer_2modi.json",  
                                        parallel_chains = 3)

loo_sinjer=loo(fit_sinjer$draws("log_lik"), r_eff = NA)
print(loo_sinjer)







#fit=read_cmdstan_csv(files = c("jer2modi/jer2modi_1.csv","jer2modi/jer2modi_2.csv","jer2modi/jer2modi_3.csv"))
#mcmc_intervals(fit$post_warmup_draws,regex_pars = c("mu_l2\\W"))
# 
# fit_jer2modi=readRDS("jer2modi/fit_jer2modi.rds")
# 
# json_data <- fromJSON(file="jer2modi/jer_2modi.json")
# y_rep_hosp <- readRDS("jer2modi/y_rep_hosp.rds")
# Se recomienda haber guardado una iniciación del objeto y_rep_hosp
# # para agilizar actualizaciones a este código
# y_rep_mort <- readRDS("jer2modi/y_rep_mort.rds")
# Se recomienda haber guardado una iniciación del objeto y_rep_mort
# para agilizar actualizaciones a este código
