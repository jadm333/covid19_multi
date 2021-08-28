#Aquí se genera df2
source("./helpers/creation_df2.R")

library(cmdstanr)
#library(tidyverse)
library(bayesplot)
#library(lubridate)
library(posterior)
library(tidybayes)


##############################################################
#Modelo
##############################################################


set.seed(12345)

hosp=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),!is.na(EDAD),!is.na(SEXO),
                    tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                    !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR),tiempo_hosp>1) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()

muerte=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),evento==0,!is.na(EDAD),!is.na(SEXO),
                      tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                      !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR)) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()

#saveRDS(list(hosp=hosp,muerte=muerte),"./Data/datos.rds")

x=model.matrix(~DIABETES+EPOC+OBESIDAD+HIPERTENSION+DIABETES*OBESIDAD*HIPERTENSION+
                 SEXO+RENAL_CRONICA+EDAD,data=muerte)
x_hosp=model.matrix(~EPOC+OBESIDAD+RENAL_CRONICA+ASMA+INMUSUPR+SEXO+EDAD,data=hosp)



x=x[,-1]
x_hosp=x_hosp[,-1]


inits1=list(mu_raw_mort=-1.5,alpha_raw=0.01)


for (i in 1:3){
  write_stan_json(inits1,file = paste0("Cmdstan/inits_",i,".json"))
}



############################
#Sin jerarquia
############################

sin_jer=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(hosp$tiempo_hosp),
  y_hosp=as.numeric(hosp$tiempo_hosp),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

write_stan_json(sin_jer,file = "Cmdstan/sin_jer.json")



############################
#Con jerarquia 1
############################

jer_1=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(hosp$tiempo_hosp),
  y_hosp=as.numeric(hosp$tiempo_hosp),
  Gniv1=length(levels(muerte$ENTIDAD_UM)),
  Niv1=as.numeric(muerte$ENTIDAD_UM),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

write_stan_json(jer_1,file = "Cmdstan/jer_1.json")



############################
#Con jerarquia 2
############################

jer_2=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(hosp$tiempo_hosp),
  y_hosp=as.numeric(hosp$tiempo_hosp),
  Gniv1=length(levels(muerte$ENTIDAD_UM)),
  Gniv2=length(levels(muerte$SECTOR)),
  Niv1=as.numeric(muerte$ENTIDAD_UM),
  Niv2=as.numeric(muerte$SECTOR),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)


write_stan_json(jer_2,file = "Cmdstan/jer_2.json")



############################
#Con jerarquia 2 modif 
############################

jer_2modi_h=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(hosp$tiempo_hosp),
  y_hosp=as.numeric(hosp$tiempo_hosp),
  Gniv1=length(levels(muerte$ENTIDAD_UM)),
  Gniv2=length(levels(muerte$SECENT)),
  Niv1=as.numeric(muerte$ENTIDAD_UM),
  Niv2=as.numeric(muerte$SECENT),
  #Gnivh1=length(levels(hosp$ENTIDAD_UM)),
  #Gnivh2=length(levels(hosp$SECENT)),
  #Nivh1=as.numeric(hosp$ENTIDAD_UM),
  #Nivh2=as.numeric(hosp$SECENT),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

write_stan_json(jer_2modi_h,file = "Cmdstan/jer_2modi.json")



##### Extras #######

generateFit = T

if(generateFit){
  
  mod_jer2modi <- cmdstan_model("../covid19_multi/CC2/jer2modi/ModeloJer2QRhosp_quant.stan")
  
  json_data_jer2modi <- fromJSON(file="./Cmdstan/jer_2modi.json")
  
  fit_jer2modi <- mod_jer2modi$generate_quantities(c("./CC2/jer2modi/jer2modi_1.csv","./CC2/jer2modi/jer2modi_2.csv",
                                                     "./CC2/jer2modi/jer2modi_3.csv"), data = "./Cmdstan/jer_2modi.json",
                                                   parallel_chains = 3)
  
  fit_jer2modi$save_object(file = "Fit/random2mod.rds")
}



generateLong = F


fitJer2QRmodi_h=readRDS("Fit/fitJer2QRmodi_h.rds")

y_rep_hosp=fitJer2QRmodi_h$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)

y_rep_mort=fitJer2QRmodi_h$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

ppc_dens_overlay(jer_2modi_h$y_hosp, y_rep_hosp[1:200, ])
ppc_dens_overlay(jer_2modi_h$y_mort, y_rep_mort[1:200, ])

t=fitJer2QRmodi_h$summary(c("mu_l2_h","mu_raw_hosp"))

fitQR$save_object(file = "Fit/fitQR.rds")
fitJerQR$save_object(file = "Fit/fitJerQR.rds")
fitJer2QR$save_object(file = "Fit/fitJer2QR.rds")
fitJer2QRmodi_h$save_object(file = "Fit/fitJer2QRmodi_h.rds")


longFormat_y_rep_hosp <- gather_draws(as_draws_df(fitJer2QRmodi_h$draws()),y_hosp_tilde[id]) %>% ungroup()

longFormat_y_rep_mort <- gather_draws(as_draws_df(fitJer2QRmodi_h$draws()),y_mort_tilde[id]) %>% ungroup()

Estados <- muerte %>% mutate(id=row_number()) %>% select(ENTIDAD_UM,id)

longFormat_y_rep_hosp <- left_join(Estados,longFormat_y_rep_hosp,by=c("id"="id")) %>%
  write_csv("~/Documents/Github/covid19_epi/data/longFormat_y_rep_hosp.csv")

longFormat_y_rep_mort <- left_join(Estados,longFormat_y_rep_mort,by=c("id"="id")) %>%
  write_csv("~/Documents/Github/covid19_epi/data/longFormat_y_rep_mort.csv")
