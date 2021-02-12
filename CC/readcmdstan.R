library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(tidybayes)
library(readr)
library(rjson)
library(loo)

###multi

fit=read_cmdstan_csv(files = c("jer2modi/jer2modi_1.csv","jer2modi/jer2modi_2.csv","jer2modi/jer2modi_3.csv"))
mcmc_intervals(fit$post_warmup_draws,regex_pars = c("mu_l2\\W"))

fit_gq=readRDS("jer2modi/fit_gq.rds")

json_data <- fromJSON(file="jer2modi/jer_2modi.json")
y_rep_hosp <- readRDS("jer2modi/y_rep_hosp.rds")
y_rep_mort <- readRDS("jer2modi/y_rep_mort.rds")

loo_plot <- ppc_dens_overlay(json_data$y_mort,y_rep_mort[1:200,])

loo_hosp_jer2=loo(y_rep_hosp, r_eff = NA)
print(loo_hosp_jer2)
plot(loo_hosp_jer2)
loo_mort_jer2=loo(y_rep_mort, r_eff = NA)
print(loo_mort_jer2)
plot(loo_mort_jer2)




# fit=read_cmdstan_csv(files = c("jer2modi/jer2modi_1.csv","jer2modi/jer2modi_2.csv","jer2modi/jer2modi_3.csv"))
# mcmc_intervals(fit$post_warmup_draws,regex_pars = c("mu_l2\\W"))
#mod_gq <- cmdstan_model("jer2modi/ModeloJer2QRhosp_quant.stan")
#fit_gq <- mod_gq$generate_quantities(c("jer2modi/jer2modi_1.csv","jer2modi/jer2modi_2.csv",
#                                       "jer2modi/jer2modi_3.csv"), data = "jer2modi/jer_2modi.json",  
#                                     parallel_chains = 3)
# json_data <- fromJSON(file="jer2modi/jer_2modi.json")
# y_rep_hosp=fit_gq$draws("y_hosp_tilde")
# y_rep_hosp=as_draws_matrix(y_rep_hosp)
# Se recomienda haber guardado una iniciación del objeto y_rep_hosp
# # para agilizar actualizaciones a este código
# y_rep_mort=fit_gq$draws("y_mort_tilde")
# y_rep_mort=as_draws_matrix(y_rep_mort)
# Se recomienda haber guardado una iniciación del objeto y_rep_mort
# # para agilizar actualizaciones a este códigoloo_plot <- ppc_dens_overlay(json_data$y_mort,y_rep_mort[1:200,])
# #ggsave(loo_plot,)
