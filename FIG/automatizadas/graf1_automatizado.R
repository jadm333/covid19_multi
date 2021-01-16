library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(lubridate)
library(posterior)
library(tidybayes)
library(ggplot2)

#fitJer2QRmodi_h=readRDS("FIG/fitJer2QRmodi_h.rds")
fitJer2QRmodi_h=readRDS("fitJer2QRmodi_h.rds")
datos=readRDS("Data/datos.rds")
mdat=datos$muerte
hdat=datos$hosp


carpeta_imagenes <- "FIG/automatizadas/"

############
# Gráficas #
############

color_scheme_set("gray")


### EstadosMuCompl ###

mu_l1=fitJer2QRmodi_h$draws("mu_l")
mu_l1=as_draws_matrix(mu_l1)
mu_l1=as.data.frame(mu_l1)
colnames(mu_l1)=levels(mdat$ENTIDAD_UM)

EstadosMuCompl <- mcmc_intervals(
  mu_l1,
  regex_pars  =colnames(mu_l1),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    x="log hazard ratio"
  )

ggsave(paste0(carpeta_imagenes,"EstadosMuCompl.png"),EstadosMuCompl,width = 23.05, height = 17.57, units = "cm")


### EstadoSectoresMuCompl ###

mu_l2=fitJer2QRmodi_h$draws("mu_l2")
mu_l2=as_draws_matrix(mu_l2)
mu_l2=as.data.frame(mu_l2)
cml2=colnames(mu_l2)=levels(mdat$SECENT)

#library(stringr)

#Chiapas_index <-which(str_detect(cml2,pattern = "Chiapas."))
#Guanajuato_index <-which(str_detect(cml2,pattern = "Guanajuato."))
#Veracruz_index <-which(str_detect(cml2,pattern = "Veracruz."))
#Guerrero_index <-which(str_detect(cml2,pattern = "Guerrero."))
#CDMX_index <-which(str_detect(cml2,pattern = "Mexico City."))
#NL_index <-which(str_detect(cml2,pattern = "Nuevo Leon."))
#SLP_index <-which(str_detect(cml2,pattern = "San Luis Potosi."))


EstadoSectoresMuCompl <- mcmc_intervals(
  mu_l2,
  #regex_pars = cml2[c(Chiapas_index,Guanajuato_index,CDMX_index,NL_index,SLP_index,Veracruz_index)],
  regex_pars  =cml2[c(21:25,48:52,69:73,94:99,120:124,151:155)], 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+  ggplot2::labs(
  x="log hazard ratio"
)

ggsave(paste0(carpeta_imagenes,"EstadoSectoresMuCompl.png"),EstadoSectoresMuCompl,width = 23.05,height = 17.57,units="cm")


#### betamDistHazard ###

beta_m=fitJer2QRmodi_h$draws("beta")
beta_m=as_draws_matrix(beta_m)
beta_m=as.data.frame(beta_m)
colnames(beta_m)=c("COPD","OBESITY","CHRONIC_KIDNEY","ASTHMA", "IMMUSUPR" )

betamDistHazard <- mcmc_areas(
  beta_m,
  regex_pars  =c("COPD","OBESITY","CHRONIC_KIDNEY","ASTHMA", "IMMUSUPR" ),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    x="log hazard ratio"
  )

ggsave(paste0(carpeta_imagenes,"betamDistHazard.png"),betamDistHazard,width = 23.05,height = 17.57, units = "cm")


### betaMIntervalsHazard ###

betaMIntervalsHazard <- mcmc_intervals(
  beta_m,
  regex_pars  =c("COPD","OBESITY","CHRONIC_KIDNEY","ASTHMA", "IMMUSUPR" ),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    x="log hazard ratio"
  )

ggsave(paste0(carpeta_imagenes,"betaMIntervalsHazard.png"),betaMIntervalsHazard,width = 23.05,height = 17.57, units = "cm")


### BetaHDistrHazard ###

beta_h=fitJer2QRmodi_h$draws("beta_h")
beta_h=as_draws_matrix(beta_h)
beta_h=as.data.frame(beta_h)
colnames(beta_h)=c("OBESITY","CHRONIC_KIDNEY")

BetaHDistrHazard <- mcmc_areas(
  beta_h,
  regex_pars=c("OBESITY","CHRONIC_KIDNEY"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    x="log hazard ratio"
  )


ggsave(paste0(carpeta_imagenes,"BetaHDistrHazard.png"),BetaHDistrHazard,width = 23.05,height = 17.57, units = "cm")


### BetaHIntervHazard ###

BetaHIntervHazard <- mcmc_intervals(
  beta_h,
  regex_pars=c("OBESITY","CHRONIC_KIDNEY"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    x="log hazard ratio"
  )

ggsave(paste0(carpeta_imagenes,"BetaHIntervHazard.png"),BetaHIntervHazard,width = 23.05,height = 17.57, units = "cm")

### MuRawMortHazard ###

muraw_m=fitJer2QRmodi_h$draws("mu_raw_mort")
muraw_m=as_draws_matrix(muraw_m)
muraw_m=as.data.frame(muraw_m)

MuRawMortHazard <- mcmc_areas(
  muraw_m,
  regex_pars  =c("mu_raw_mort"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    x="log hazard ratio"
  )

ggsave(paste0(carpeta_imagenes,"MuRawMortHazard.png"),MuRawMortHazard,width = 23.05,height = 17.57, units = "cm")


### MuRawHospHazard ###

muraw_h=fitJer2QRmodi_h$draws("mu_raw_hosp")
muraw_h=as_draws_matrix(muraw_h)
muraw_h=as.data.frame(muraw_h)

MuRawHospHazard <- mcmc_areas(
  muraw_h,
  regex_pars  =c("mu_raw_hosp"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    x="log hazard ratio"
  )

ggsave(paste0(carpeta_imagenes,"MuRawHospHazard.png"),MuRawHospHazard,width = 23.05,height = 17.57, units = "cm")

### PostMort1 ###

y_rep_mort=fitJer2QRmodi_h$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

PostMort1 <- qplot(y_rep_mort,geom="density",xlim=c(-1,50))+
  theme_bw() +scale_fill_manual(values=c("black"))

ggsave(paste0(carpeta_imagenes,"PostMort1.png"),PostMort1,width = 23.05,height = 17.57, units = "cm")

### PostMort2 ###

#plot(density(y_rep_mort), xlim=c(-1,70),main="")


### PostHosp1 ###

y_rep_hosp=fitJer2QRmodi_h$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)

PostHosp1 <- qplot(y_rep_hosp,geom="density", alpha=I(1/10),xlim=c(-1,50))+
theme_bw() +scale_fill_manual(values=c("black"))

ggsave(paste0(carpeta_imagenes,"PostHosp1.png"),PostHosp1,width = 23.05,height = 17.57, units = "cm")

### PostHosp2 ###

#plot(density(y_rep_hosp), xlim=c(-1,50))

#rm(fitJer2QRmodi_h)
