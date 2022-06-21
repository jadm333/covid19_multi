#Aqu? se genera df2
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

#Correr esto si queremos los databases hosp y muerte con la informacion del semaforo
#de acuerdo a inicio de sintomas
source("helpers/df2_sem.R")
#Correr esto si no nos interesa incluir la informacion del semaforo apra hosp y muertes
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
                 SEXO+RENAL_CRONICA+EDAD,# +sem,
               data=muerte)

x_hosp=model.matrix(~EPOC+OBESIDAD+RENAL_CRONICA+ASMA+INMUSUPR+SEXO+EDAD,#+sem,
                    data=hosp)



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

############################
#Con jerarquia 3 modif 
############################

jer_3modi=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(hosp$tiempo_hosp),
  y_hosp=as.numeric(hosp$tiempo_hosp),
  Gniv1=length(levels(muerte$year)),
  Gniv2=length(levels(muerte$YEARENT)),
  Gniv3=length(levels(muerte$YEARSECENT)),
  Niv1=as.numeric(muerte$year),
  Niv2=as.numeric(muerte$YEARENT),
  Niv3=as.numeric(muerte$YEARSECENT),
  #Gnivh1=length(levels(hosp$ENTIDAD_UM)),
  #Gnivh2=length(levels(hosp$SECENT)),
  #Nivh1=as.numeric(hosp$ENTIDAD_UM),
  #Nivh2=as.numeric(hosp$SECENT),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

write_stan_json(jer_3modi,file = "Cmdstan/jer_3modi.json")



##### Extras #######

