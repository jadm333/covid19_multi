library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(tidybayes)
library(readr)
library(rjson)
library(loo)

#set working directory to covid19_multi

###multi

mod_jer2modi <- cmdstan_model("./CC/jer2modi/ModeloJer2QRhosp_quant.stan")
json_data_jer2modi <- fromJSON(file="./Cmdstan/jer_2modi.json")
fit_jer2modi <- mod_jer2modi$generate_quantities(c("./CC/jer2modi/jer2modi_1.csv","./CC/jer2modi/jer2modi_2.csv",
                                       "./CC/jer2modi/jer2modi_3.csv"), data = "./Cmdstan/jer_2modi.json",  
                                     parallel_chains = 3)

y_rep_hosp=fit_jer2modi$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)
y_rep_mort=fit_jer2modi$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

ppc_plot_modi <- ppc_dens_overlay(json_data_jer2modi$y_mort,y_rep_mort[1:200,])

loo_hosp_jer2modi=loo(fit_jer2modi$draws("log_lik_hosp"), r_eff = NA)
print(loo_hosp_jer2modi)
loo_mort_jer2=loo(fit_jer2modi$draws("log_lik_mort"), r_eff = NA)
print(loo_mort_jer2modi)

#loo_compare(loo1, loo2)

#jer2
mod_jer2 <- cmdstan_model("./CC/jer2/ModeloJer2QR_quant.stan")

json_data_jer2 <- fromJSON(file="./Cmdstan/jer_2.json")

fit_jer2 <- mod_jer2$generate_quantities(c("./CC/jer2/jer2_1.csv","./CC/jer2/jer2_2.csv",
                                                   "./CC/jer2/jer2_3.csv"), data = "./Cmdstan/jer_2.json",  
                                                 parallel_chains = 3)

loo_hosp_jer2=loo(fit_jer2$draws("log_lik_hosp"), r_eff = NA)
print(loo_hosp_jer2)
loo_mort_jer2=loo(fit_jer2$draws("log_lik_mort"), r_eff = NA)
print(loo_mort_jer2)



#jer1
mod_jer1 <- cmdstan_model("./CC/jer1/ModeloJerQR_quant.stan")
fit_jer1<- mod_jer1$generate_quantities(c("./CC/jer1/jer1_1.csv","./CC/jer1/jer1_2.csv",
                                          "./CC/jer1/jer1_3.csv"), data = "./Cmdstan/jer_1.json",  
                                        parallel_chains = 3)

loo_hosp_jer1=loo(fit_jer1$draws("log_lik_hosp"), r_eff = NA)
print(loo_hosp_jer1)
loo_mort_jer1=loo(fit_jer1$draws("log_lik_mort"), r_eff = NA)
print(loo_mort_jer1)



#sinjer
mod_sinjer <- cmdstan_model("./CC/sinjer/ModeloQR_quant.stan")
fit_sinjer<- mod_jer1$generate_quantities(c("./CC/sinjer/sin_jer_1.csv","./CC/sinjer/sin_jer_2.csv",
                                          "./CC/sinjer/sin_jer_3.csv"), data = "./Cmdstan/sin_jer.json",  
                                        parallel_chains = 3)

loo_hosp_sinjer=loo(fit_sinjer$draws("log_lik_hosp"), r_eff = NA)
print(loo_hosp_sinjer)
loo_mort_sinjer=loo(fit_sinjer$draws("log_lik_mort"), r_eff = NA)
print(loo_mort_sinjer)







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
