library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(tidybayes)
library(readr)
library(rjson)
library(loo)
library(ggplot2)
library(tidyverse)

#set working directory to covid19_multi


color_scheme_set("red")
datos=readRDS("Data/datos.rds")
mdat=datos$muerte
hdat=datos$hosp


################
### jer2modi ###
################

mod_jer2modi <- cmdstan_model("./CC2/jer2modi/ModeloJer2QRhosp_quant.stan")

json_data_jer2modi <- fromJSON(file="./Cmdstan/jer_2modi.json")

fit_jer2modi <- mod_jer2modi$generate_quantities(c("./CC2/jer2modi/jer2modi_1.csv","./CC2/jer2modi/jer2modi_2.csv",
                                                   "./CC2/jer2modi/jer2modi_3.csv"), data = "./Cmdstan/jer_2modi.json",
                                                 parallel_chains = 3)


##########################
### plots ppc jer2modi ###
##########################

#y_rep_hosp=fit_jer2modi$draws("y_hosp_tilde")
#y_rep_hosp=as_draws_matrix(y_rep_hosp)
y_rep_mort=fit_jer2modi$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

ppc_plot_modi_mort <- ppc_dens_overlay(json_data_jer2modi$y_mort,y_rep_mort[1:200,]) + 
  labs(x="Days from hospitalization to death")

ggsave("./CC2/jer2modi/ppc_plot_modi_mort.png",ppc_plot_modi_mort,width = 23.05,height = 17.57,units="cm")


###############################
### mcmc intervals jer2modi ###
###############################

intervals_jer2modi=read_cmdstan_csv(files = 
                                      c("./CC2/jer2modi/jer2modi_1.csv","./CC2/jer2modi/jer2modi_2.csv",
                                        "./CC2/jer2modi/jer2modi_3.csv"))

ylabs_mu_l2_intervals_jer2modi = str_replace_all(levels(mdat$SECENT),c("ESTATAL" = "State managed",
                                      "SE_MAR_PE" = "SEDENA/SEMAR/PEMEX", 
                                      "SSA_OTROS" = "SSA",
                                      "PRIVADA"= "Private healthcare provider"))

mu_l2_intervals_jer2modi <- mcmc_intervals(intervals_jer2modi$post_warmup_draws,regex_pars = c("mu_l2\\W"),prob_outer = .95) +
  ggplot2::labs( x="Log Hazard Ratio"
                 #,title = "Mu_l2 Jer2"
                 ) +
  scale_y_discrete(labels=rev(ylabs_mu_l2_intervals_jer2modi),limits=rev)

ggsave("./CC2/jer2modi/mu_12_intervalsjer2modi.png",mu_l2_intervals_jer2modi,width = 23.05,height = 52.71,units="cm")


mu_l_intervals_jer2modi <- mcmc_intervals(intervals_jer2modi$post_warmup_draws,regex_pars = c("mu_l\\W"),prob_outer = .95) +
  ggplot2::labs( x="Log Hazard Ratio"
                 #,title = "Mu_l2 Jer2"
  ) +
  scale_y_discrete(labels=rev(levels(mdat$ENTIDAD_UM)),limits=rev)

ggsave("./CC2/jer2modi/mu_l_intervalsjer2modi.png",mu_l_intervals_jer2modi,width = 23.05,height = 17.57,units="cm")



####################
### loo jer2modi ###
####################

loo_hosp_jer2modi=loo(fit_jer2modi$draws("log_lik_hosp"), r_eff = NA)

loo_mort_jer2modi=loo(fit_jer2modi$draws("log_lik_mort"), r_eff = NA)

#loo_compare(loo1, loo2)



############
### jer2 ###
############

mod_jer2 <- cmdstan_model("./CC2/jer2/ModeloJer2QR_quant.stan")

json_data_jer2 <- fromJSON(file="./Cmdstan/jer_2.json")

fit_jer2 <- mod_jer2$generate_quantities(c("./CC2/jer2/jer2_1.csv","./CC2/jer2/jer2_2.csv",
                                           "./CC2/jer2/jer2_3.csv"), data = "./Cmdstan/jer_2.json",  
                                         parallel_chains = 3)


######################
### plots ppc jer2 ###
######################

y_rep_hosp=fit_jer2$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)
y_rep_mort=fit_jer2$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

ppc_plot_jer2_mort <- ppc_dens_overlay(json_data_jer2$y_mort,y_rep_mort[1:200,]) + 
  labs(x="Days from hospitalization to death")

ggsave("./CC2/jer2/ppc_plot_jer2_mort.png",ppc_plot_jer2_mort,width = 23.05,height = 17.57,units="cm")



ppc_plot_jer2_hosp <- ppc_dens_overlay(json_data_jer2$y_hosp,y_rep_hosp[1:200,])

ggsave("./CC2/jer2/ppc_plot_jer2_hosp.png",ppc_plot_jer2_hosp,width = 23.05,height = 17.57,units="cm")

###########################
### mcmc intervals jer2 ###
###########################

intervals_jer2=read_cmdstan_csv(files = c("./CC2/jer2/jer2_1.csv","./CC2/jer2/jer2_2.csv","./CC2/jer2/jer2_3.csv"))
int_jer2_post=as_draws_matrix(intervals_jer2$post_warmup_draws)

ylabs_mu_l2_intervals_jer2 = str_replace_all(levels(mdat$SECTOR),c("ESTATAL" = "State managed",
                                                                   "SE_MAR_PE" = "SEDENA/SEMAR/PEMEX", 
                                                                   "SSA_OTROS" = "SSA",
                                                                   "PRIVADA"= "Private healthcare provider"))

mu_l2_intervals_jer2 = mcmc_intervals(int_jer2_post,regex_pars = "mu_l2\\W",prob_outer = .95) + 
  labs(x="Log Hazard Ratio") + 
  scale_y_discrete(labels=rev(ylabs_mu_l2_intervals_jer2),limits=rev)

ggsave("./CC2/jer2/mu_12_intervalsjer2.png",mu_l2_intervals_jer2,width = 23.05,height = 17.57,units="cm")



mu_l_intervals_jer2 = mcmc_intervals(int_jer2_post,regex_pars = "mu_l\\W",prob_outer = .95) + 
  labs(x="Log Hazard Ratio") + 
  scale_y_discrete(labels=rev(c(levels(mdat$ENTIDAD_UM))),limits=rev)

ggsave("./CC2/jer2/mu_1_intervalsjer2.png",mu_l_intervals_jer2,width = 23.05,height = 17.57,units="cm")


#x = model.matrix(~DIABETES+EPOC+OBESIDAD+HIPERTENSION+DIABETES*OBESIDAD*HIPERTENSION+
#                 SEXO+RENAL_CRONICA,data=mdat)

beta_m_jer2=as_draws_df(fit_jer2$draws("beta"))

out_all_beta_m_jer2 = beta_m_jer2 %>%
  pivot_longer(cols=-c(".chain",".iteration",".draw"),names_to = "index_beta",values_to = "Value") %>%
  mutate(Value=exp(-Value)) %>%
  group_by(index_beta) %>% median_qi(Value) %>% mutate_if(is.numeric, round, 2)

beta_intervals_jer2 = mcmc_intervals(exp(-beta_m_jer2),regex_pars = "beta",prob_outer = .95) +
  ggplot2::labs( x="Hazard Ratio",
                 y="Comorbidity"
                 #,title = "DS Jer2"
  ) +
  scale_y_discrete(#labels=rev(c(colnames(x[,-1]))),
    labels=c("beta[1]"="Diabetes",
             "beta[2]"="COPD",
             "beta[3]"="Obesity",
             "beta[4]"="Hypertension",
             "beta[5]"="Male/Female",
             "beta[6]"="Chronic Kidney",
             "beta[7]"="Diabetes : obesity",
             "beta[8]"="Diabetes : Hypertension",
             "beta[9]"="Obesity : Hypertension",
             "beta[10]"="Diabetes : Obesity : Hypertension"
    ),
    limits=rev)+
  geom_vline(xintercept = 1,lty="dashed",alpha=.3) +
  xlim(c(.74,1.4)) +
  geom_text(
    data = out_all_beta_m_jer2,
    aes(y= index_beta,label = str_glue("[{Value}, {.lower} - {.upper}]"), x = 1.4),
    hjust = "inward"
  )

ggsave("./CC2/jer2/beta_intervalsjer2.png",beta_intervals_jer2,width = 23.05,height = 17.57,units="cm")



#x_hosp=model.matrix(~EPOC+OBESIDAD+RENAL_CRONICA+ASMA+INMUSUPR,data=hdat)

beta_h_m_jer2=as_draws_df(fit_jer2$draws("beta_h"))

out_all_beta_h_m_jer2 = beta_h_m_jer2 %>%
  pivot_longer(cols=-c(".chain",".iteration",".draw"),names_to = "index_beta",values_to = "Value") %>%
  mutate(Value=exp(-Value)) %>%
  group_by(index_beta) %>% median_qi(Value) %>% mutate_if(is.numeric, round, 2)

beta_h_intervals_jer2 = mcmc_intervals(exp(-beta_h_m_jer2),regex_pars = "beta_h",prob_outer = .95) +
  ggplot2::labs( x="Hazard Ratio",
                 y="Comorbidity"
                 #,title = "DS Jer2"
  ) +
  scale_y_discrete(#labels=rev(c(colnames(x[,-1]))),
    labels=c("beta_h[1]"="COPD",
             "beta_h[2]"="Obesity",
             "beta_h[3]"="Chronic Kidney",
             "beta_h[4]"="Asthma",
             "beta_h[5]"="Immunosuppression"
    ),
    limits=rev)+
  geom_vline(xintercept = 1,lty="dashed",alpha=.3) +
  xlim(c(.9,1.25)) +
  geom_text(
    data = out_all_beta_h_m_jer2,
    aes(y= index_beta,label = str_glue("[{Value}, {.lower} - {.upper}]"), x = 1.25),
    hjust = "inward"
  )

ggsave("./CC2/jer2/beta_h_intervalsjer2.png",beta_h_intervals_jer2,width = 23.05,height = 17.57,units="cm")

################
### loo jer2 ###
################

loo_hosp_jer2=loo(fit_jer2$draws("log_lik_hosp"), r_eff = NA)

loo_mort_jer2=loo(fit_jer2$draws("log_lik_mort"), r_eff = NA)



############
### jer1 ###
############

mod_jer1 <- cmdstan_model("./CC2/jer1/ModeloJerQR_quant.stan")

json_data_jer1 <- fromJSON(file="./Cmdstan/jer_1.json")

fit_jer1<- mod_jer1$generate_quantities(c("./CC2/jer1/jer1_1.csv","./CC2/jer1/jer1_2.csv",
                                          "./CC2/jer1/jer1_3.csv"), data = "./Cmdstan/jer_1.json",  
                                        parallel_chains = 3)


######################
### plots ppc jer1 ###
######################

#y_rep_hosp=fit_jer1$draws("y_hosp_tilde")
#y_rep_hosp=as_draws_matrix(y_rep_hosp)
y_rep_mort=fit_jer1$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

ppc_plot_jer1_mort <- ppc_dens_overlay(json_data_jer1$y_mort,y_rep_mort[1:200,])

ggsave("./CC2/jer1/ppc_plot_jer1_mort.png",ppc_plot_jer1_mort,width = 23.05,height = 17.57,units="cm")


###########################
### mcmc intervals jer1 ###
###########################

intervals_jer1=read_cmdstan_csv(files = c("./CC2/jer1/jer1_1.csv","./CC2/jer1/jer1_2.csv","./CC2/jer1/jer1_3.csv"))
#int_jer1_post=as_draws_matrix(intervals_jer1$post_warmup_draws)
int_jer1_post=as_draws_df(intervals_jer1$post_warmup_draws)

mu_l_intervals_jer1 = mcmc_intervals(int_jer1_post,regex_pars =  "mu_l\\W")+
  ggplot2::labs( x="Log Hazard Ratio" 
                 #title = "Mu_l Jer1"
  ) + 
  scale_y_discrete(labels=rev(levels(mdat$ENTIDAD_UM)), limits = rev)

#int_jer1_post_ml=data.frame(int_jer1_post[,55:86])
#colnames(int_jer1_post_ml)=c(levels(mdat$ENTIDAD_UM))
#mu_l_intervals_jer1 <- mcmc_intervals(int_jer1_post_ml,regex_pars = (levels(mdat$ENTIDAD_UM)))+
#  ggplot2::labs( x="log hazard ratio", title = "Mu_l Jer1")
ggsave("./CC2/jer1/mu_1_intervalsjer1.png",mu_l_intervals_jer1,width = 23.05,height = 17.57,units="cm")


################
### loo jer1 ###
################

loo_hosp_jer1=loo(fit_jer1$draws("log_lik_hosp"), r_eff = NA)

loo_mort_jer1=loo(fit_jer1$draws("log_lik_mort"), r_eff = NA)



##############
### sinjer ###
##############

mod_sinjer <- cmdstan_model("./CC2/sinjer/ModeloQR_quant.stan")

json_data_sinjer <- fromJSON(file="./Cmdstan/sin_jer.json")

fit_sinjer<- mod_sinjer$generate_quantities(c("./CC2/sinjer/sin_jer_1.csv","./CC2/sinjer/sin_jer_2.csv",
                                              "./CC2/sinjer/sin_jer_3.csv"), data = "./Cmdstan/sin_jer.json",  
                                            parallel_chains = 3)


########################
### plots ppc sinjer ###
########################

#y_rep_hosp=fit_sinjer$draws("y_hosp_tilde")
#y_rep_hosp=as_draws_matrix(y_rep_hosp)
y_rep_mort=fit_sinjer$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

ppc_plot_sinjer_mort <- ppc_dens_overlay(json_data_sinjer$y_mort,y_rep_mort[1:200,])

ggsave("./CC2/sinjer/ppc_plot_sinjer_mort.png",ppc_plot_sinjer_mort,width = 23.05,height = 17.57,units="cm")


#############################
### mcmc intervals sinjer ###
#############################

#intervals_sinjer=read_cmdstan_csv(files = c("./CC2/sinjer/sin_jer_1.csv","./CC2/sinjer/sin_jer_2.csv",
#                                            "./CC2/sinjer/sin_jer_3.csv"))
#int_sinjer_post=as_draws_df(intervals_sinjer$post_warmup_draws)



##################
### loo sinjer ###
##################

loo_hosp_sinjer=loo(fit_sinjer$draws("log_lik_hosp"), r_eff = NA)

loo_mort_sinjer=loo(fit_sinjer$draws("log_lik_mort"), r_eff = NA)


# loo_hosp_jer2modi=readRDS("./CC2/loo_hosp_jer2modi.rds")
# loo_mort_jer2modi=readRDS("./CC2/loo_mort_jer2modi.rds")
# loo_hosp_jer2=readRDS("./CC2/loo_hosp_jer2.rds")
# loo_mort_jer2=readRDS("./CC2/loo_mort_jer2.rds")
# loo_hosp_jer1=readRDS("./CC2/loo_hosp_jer1.rds")
# loo_mort_jer1=readRDS("./CC2/loo_mort_jer1.rds")


loo=loo_compare(loo_mort_jer2modi,loo_mort_jer2,loo_mort_jer1,loo_mort_sinjer)
loo=as.data.frame(loo)
rownames(loo)=c("jer2", "jer1", "sinjer", "jer2Modi")
write.csv(loo, file="./CC2/loo_comp.csv",row.names = T)
#el modelo jer2modi dice ser peorsito que el jer2
loo_compare(loo_hosp_jer2modi,loo_hosp_jer2,loo_hosp_jer1,loo_hosp_sinjer)
#aqui jer2modi es el peor



#