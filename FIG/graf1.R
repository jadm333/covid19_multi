library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(lubridate)
library(posterior)
library(tidybayes)


fitJer2QRmodi_h=readRDS("fitJer2QRmodi_h.rds")
datos=readRDS("datos.rds")
mdat=datos$muerte
hdat=datos$hosp


beta_m=fitJer2QRmodi_h$draws("beta")
beta_m=as_draws_matrix(beta_m)
beta_m=as.data.frame(beta_m)
colnames(beta_m)=c("COPD","OBESITY","CHRONIC_KIDNEY","ASTHMA", "IMMUSUPR" )

beta_h=fitJer2QRmodi_h$draws("beta_h")
beta_h=as_draws_matrix(beta_h)
beta_h=as.data.frame(beta_h)
colnames(beta_h)=c("OBESITY","CHRONIC_KIDNEY")

muraw_m=fitJer2QRmodi_h$draws("mu_raw_mort")
muraw_m=as_draws_matrix(muraw_m)
muraw_m=as.data.frame(muraw_m)


muraw_h=fitJer2QRmodi_h$draws("mu_raw_hosp")
muraw_h=as_draws_matrix(muraw_h)
muraw_h=as.data.frame(muraw_h)

mu_l1=fitJer2QRmodi_h$draws("mu_l")
mu_l1=as_draws_matrix(mu_l1)
mu_l1=as.data.frame(mu_l1)

colnames(mu_l1)=levels(mdat$ENTIDAD_UM)


mu_l2=fitJer2QRmodi_h$draws("mu_l2")
mu_l2=as_draws_matrix(mu_l2)
mu_l2=as.data.frame(mu_l2)
cml2=colnames(mu_l2)=levels(mdat$SECENT)

y_rep_mort=fitJer2QRmodi_h$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)


y_rep_hosp=fitJer2QRmodi_h$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)

color_scheme_set("gray")
mcmc_areas(
  beta_m,
  regex_pars  =c("COPD","OBESITY","CHRONIC_KIDNEY","ASTHMA", "IMMUSUPR" ), 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
  title = "Beta for Deceases",
  subtitle = ""
)

mcmc_areas(
  beta_h,
  regex_pars=c("OBESITY","CHRONIC_KIDNEY"), 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Beta for Hospitalizations",
    subtitle = ""
  )


mcmc_areas(
  muraw_m,
  regex_pars  =c("mu_raw_mort"), 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_raw for Deceases",
    subtitle = ""
  )


mcmc_areas(
  muraw_h,
  regex_pars  =c("mu_raw_hosp"), 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu raw for Hospitalizations",
    subtitle = ""
  )


mcmc_intervals(
  mu_l1,
  regex_pars  =colnames(mu_l1), 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l for State",
    subtitle = ""
  )


mcmc_intervals(
  mu_l2,
  regex_pars  =cml2[1:42], 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+  ggplot2::labs(
    title = "Mu_l for STATE-SECTOR",
    subtitle = "AS-CS"
  )

mcmc_intervals(
  mu_l2,
  regex_pars  =cml2[43:83], 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l for STATE-SECTOR",
    subtitle = "DF-MN"
  )
mcmc_intervals(
  mu_l2,
  regex_pars  =cml2[84:125],
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l for STATE-SECTOR",
    subtitle = "MS-SL"
  )

mcmc_intervals(
  mu_l2,
  regex_pars  =cml2[126:165],
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l for STATE-SECTOR",
    subtitle = "SP-zs"
  )

qplot(y_rep_hosp,geom="density", alpha=I(1/10),main = "Predictive Posterior Distribution Hosp",xlim=c(-1,50))+
theme_bw() +scale_fill_manual(values=c("black"))

qplot(y_rep_mort,geom="density", main = "Predictive Posterior Distribution Dead",xlim=c(-1,50))+
  theme_bw() +scale_fill_manual(values=c("black"))

plot(density(y_rep_mort), xlim=c(-1,70),main="Predictive Posterior Distribution Dead")
plot(density(y_rep_hosp), xlim=c(-1,50),main="Posterior predictiva Hospitalización Hosp")



  