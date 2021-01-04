library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(lubridate)
library(posterior)
library(tidybayes)
est=c("Aguascalientes", "Baja California", "Baja California Sur","Campeche", "Chiapas", 
      "Chihuahua", "Coahuila", "Colima", "CDMX", "Durango", "EdoMex", "Guanajuato",
      "Guerrero", "Hidalgo", "Jalisco", "Michoacan", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
      "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa", "Sonora", "Tabasco",
      "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")

est1=c("Aguascalientes-IMSS","Aguascalientes-ISSSTE","Aguascalientes-PRIVADA","Aguascalientes-SSA_OTROS")

est2=c("Baja California-IMSS","Baja California-ISSSTE","Baja California-PRIVADA",
       "Baja California-ESTATAL","Baja California-SE_MAR_PE","Baja California-SSA_OTROS")

est3=c("Baja California Sur-IMSS","Baja California Sur-ISSSTE",
       "Baja California Sur-SE_MAR_PE","Baja California Sur-SSA_OTROS")

est4=c("Campeche-IMSS", "Campeche-ISSSTE","Campeche-ESTATAL", "Campeche-SE_MAR_PE",
       "Campeche-SSA_OTROS")

est5=c("Chiapas-IMSS","Chiapas-ISSSTE","Chiapas-ESTATAL","Chiapas-SE_MAR_PE",
       "Chiapas-SSA_OTROS")

est6=c("Chihuahua-IMSS", "Chihuahua-ISSSTE", "Chihuahua-SE_MAR_PE",
       "Chihuahua-SSA_OTROS")

est7=c("Coahuila-IMSS", "Coahuila-ISSSTE", "Coahuila-ESTATAL", "Coahuila-SE_MAR_PE",
       "Coahuila-SSA_OTROS" )

est8=c("Colima-IMSS", "Colima-ISSSTE", "Colima-PRIVADA", "Colima-ESTATAL", "Colima-SE_MAR_PE",
       "Colima-SSA_OTROS")

est9=c("CDMX-IMSS", "CDMX-ISSSTE", "CDMX-PRIVADA", "CDMX-ESTATAL", "CDMX-SE_MAR_PE",
       "CDMX-SSA_OTROS")

est10=c("Durango-IMSS", "Durango-ISSSTE", "Durango-SE_MAR_PE",
        "Durango-SSA_OTROS")

est11=c("EdoMex-IMSS", "EdoMex-ISSSTE", "EdoMex-PRIVADA", "EdoMex-SE_MAR_PE", 
        "EdoMex-SSA_OTROS")

est12=c("Guanajuato-IMSS","Guanajuato-ISSSTE",
        "Guanajuato-SE_MAR_PE","Guanajuato-SSA_OTROS")

est13=c("Guerrero-IMSS", "Guerrero-ISSSTE", "Guerrero-PRIVADA", "Guerrero-SE_MAR_PE",
        "Guerrero-SSA_OTROS")

est14=c("Hidalgo-IMSS", "Hidalgo-ISSSTE", "Hidalgo-PRIVADA", "Hidalgo-ESTATAL", "Hidalgo-SE_MAR_PE",
        "Hidalgo-SSA_OTROS")

est15=c("Jalisco-IMSS","Jalisco-ISSSTE","Jalisco-PRIVADA","Jalisco-ESTATAL","Jalisco-SE_MAR_PE",
        "Jalisco-SSA_OTROS")

est16=c("Michoacan-IMSS", "Michoacan-ISSSTE", "Michoacan-PRIVADA", "Michoacan-ESTATAL", 
        "Michoacan-SE_MAR_PE","Michoacan-SSA_OTROS")

est17=c("Morelos-IMSS", "Morelos-ISSSTE", "Morelos-PRIVADA", "Morelos-SE_MAR_PE",
        "Morelos-SSA_OTROS")

est18=c("Nayarit-IMSS", "Nayarit-ISSSTE", "Nayarit-PRIVADA", "Nayarit-SE_MAR_PE",
        "Nayarit-SSA_OTROS")

est19=c("Nuevo León-IMSS", "Nuevo León-ISSSTE", "Nuevo León-PRIVADA", "Nuevo León-ESTATAL", 
        "Nuevo León-SE_MAR_PE", "Nuevo León-SSA_OTROS")

est20=c("Oaxaca-IMSS","Oaxaca-ISSSTE","Oaxaca-PRIVADA","Oaxaca-ESTATAL","Oaxaca-SE_MAR_PE",
        "Oaxaca-SSA_OTROS")

est21=c("Puebla-IMSS","Puebla-ISSSTE","Puebla-PRIVADA","Puebla-ESTATAL","Puebla-SE_MAR_PE",
        "Puebla-SSA_OTROS")

est22=c("Queretaro-IMSS", "Queretaro-ISSSTE", "Queretaro-PRIVADA","Queretaro-SSA_OTROS")

est23=c("Quintana Roo-IMSS","Quintana Roo-ISSSTE","Quintana Roo-PRIVADA","Quintana Roo-ESTATAL",
        "Quintana Roo-SE_MAR_PE","Quintana Roo-SSA_OTROS")

est24=c("San Luis Potosi-IMSS","San Luis Potosi-ISSSTE","San Luis Potosi-PRIVADA",
        "San Luis Potosi-SE_MAR_PE","San Luis Potosi-SSA_OTROS")

est25=c("Sinaloa-IMSS","Sinaloa-ISSSTE","Sinaloa-PRIVADA","Sinaloa-ESTATAL","Sinaloa-SE_MAR_PE",
        "Sinaloa-SSA_OTROS")

est26=c("Sonora-IMSS", "Sonora-ISSSTE", "Sonora-ESTATAL", "Sonora-SE_MAR_PE",
        "Sonora-SSA_OTROS")

est27=c("Tabasco-IMSS","Tabasco-ISSSTE","Tabasco-PRIVADA","Tabasco-ESTATAL","Tabasco-SE_MAR_PE",
        "Tabasco-SSA_OTROS")

est28=c("Tamaulipas-IMSS","Tamaulipas-ISSSTE","Tamaulipas-PRIVADA","Tamaulipas-ESTATAL","Tamaulipas-SE_MAR_PE",
        "Tamaulipas-SSA_OTROS")

est29=c("Tlaxcala-IMSS", "Tlaxcala-ISSSTE", "Tlaxcala-SSA_OTROS")

est30=c("Veracruz-IMSS", "Veracruz-ISSSTE", "Veracruz-PRIVADA",  "Veracruz-SE_MAR_PE", 
        "Veracruz-SSA_OTROS")

est31=c("Yucatan-IMSS","Yucatan-ISSSTE","Yucatan-PRIVADA","Yucatan-ESTATAL","Yucatan-SE_MAR_PE",
        "Yucatan-SSA_OTROS")

est32=c("Zacatecas-IMSS","Zacatecas-ISSSTE","Zacatecas-SE_MAR_PE","Zacatecas-SSA_OTROS")


estsec1=c(est1,est2,est3,est4, est5, est6, est7)
estsec2=c(est8,est9,est10, est11, est12,est13, est14)
estsec3=c(est15,est16, est17, est18, est19,est20)
estsec4=c(est21,est22, est23, est24, est25, est26)
estsec5=c(est27,est28, est29, est30, est31, est32)

      
fitJer2QRmodi_h=readRDS("fitJer2QRmodi_h.rds")


beta_m=fitJer2QRmodi_h$draws("beta")
beta_m=as_draws_matrix(beta_m)
beta_m=as.data.frame(beta_m)
colnames(beta_m)=colnames(x)

beta_h=fitJer2QRmodi_h$draws("beta_h")
beta_h=as_draws_matrix(beta_h)
beta_h=as.data.frame(beta_h)
colnames(beta_h)=colnames(x_hosp)

muraw_m=fitJer2QRmodi_h$draws("mu_raw_mort")
muraw_m=as_draws_matrix(muraw_m)
muraw_m=as.data.frame(muraw_m)


muraw_h=fitJer2QRmodi_h$draws("mu_raw_hosp")
muraw_h=as_draws_matrix(muraw_h)
muraw_h=as.data.frame(muraw_h)

mu_l1=fitJer2QRmodi_h$draws("mu_l")
mu_l1=as_draws_matrix(mu_l1)
mu_l1=as.data.frame(mu_l1)
colnames(mu_l1)=est


mu_l2=fitJer2QRmodi_h$draws("mu_l2")
mu_l2=as_draws_matrix(mu_l2)
mu_l2=as.data.frame(mu_l2)
colnames(mu_l2)=c(estsec1,estsec2,estsec3,estsec4,estsec5)

y_rep_mort=fitJer2QRmodi_h$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)


y_rep_hosp=fitJer2QRmodi_h$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)

color_scheme_set("gray")
mcmc_areas(
  beta_m,
  regex_pars  =c(colnames(x)), 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
  title = "Beta para muertes",
  subtitle = ""
)

mcmc_areas(
  beta_h,
  regex_pars  =c(colnames(beta_h)), 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Beta para hospitalizaciones",
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
    title = "Mu raw para muertes",
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
    title = "Mu raw para hospitalizaciones",
    subtitle = ""
  )


mcmc_intervals(
  mu_l1,
  regex_pars  =est, 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l por estado",
    subtitle = ""
  )


mcmc_intervals(
  mu_l2,
  regex_pars  =estsec1, 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+  ggplot2::labs(
    title = "Mu_l Por estado-Sector",
    subtitle = "Hasta Coahuila"
  )

mcmc_intervals(
  mu_l2,
  regex_pars  =estsec2, 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l Por estado-Sector",
    subtitle = "Hasta Hidalgo"
  )
mcmc_intervals(
  mu_l2,
  regex_pars  =estsec3, 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l Por estado-Sector",
    subtitle = "Hasta Oaxaca"
  )
mcmc_intervals(
  mu_l2,
  regex_pars  =estsec4, 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l Por estado-Sector",
    subtitle = "Hasta Sonora"
  )
mcmc_intervals(
  mu_l2,
  regex_pars  =estsec5, 
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)+
  ggplot2::labs(
    title = "Mu_l Por estado-Sector",
    subtitle = "Hasta Zacatecas"
  )

qplot(y_rep_hosp,geom="density", alpha=I(1/10),main = "Predictive Posterior Distribution Hosp",xlim=c(-1,50))+
theme_bw() +scale_fill_manual(values=c("black"))

qplot(y_rep_mort,geom="density", main = "Predictive Posterior Distribution Dead",xlim=c(-1,50))+
  theme_bw() +scale_fill_manual(values=c("black"))

plot(density(y_rep_mort), xlim=c(-1,70),main="Predictive Posterior Distribution Dead")
plot(density(y_rep_hosp), xlim=c(-1,50),main="Posterior predictiva Hospitalización Hosp")



  