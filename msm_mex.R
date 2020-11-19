library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(rstan)
library(bayesplot) 


color_scheme_set("viridis")


df=read_csv("data/201104COVID19MEXICO.csv",na=c("","NA","97","98","99","9999-99-99"))
corte=ymd("2020-11-04")


df2=df %>% distinct(ID_REGISTRO,.keep_all = T) %>%
  filter(CLASIFICACION_FINAL==3,TIPO_PACIENTE==2) %>%
  mutate(
    SECTOR = as.factor(case_when(
      SECTOR==4 | SECTOR==5 ~ "IMSS",
      SECTOR==6 ~ "ISSSTE",
      SECTOR==9 ~ "PRIVADA",
      SECTOR==8 | SECTOR==10 | SECTOR==11 ~ "SE_MAR_PE",
      TRUE                      ~ "SSA_OTROS"
    )),evento= case_when(
      is.na(FECHA_DEF) ~ 1,
      TRUE                      ~ 0
    )) %>% mutate(tiempo_hosp=as.numeric(ymd(FECHA_INGRESO)-ymd(FECHA_SINTOMAS))+.1,FECHA_DEF=if_else(is.na(FECHA_DEF),corte,ymd(FECHA_DEF)),
               tiempo_muerte=as.numeric(FECHA_DEF-ymd(FECHA_INGRESO))+.1,ENTIDAD_UM=as.factor(ENTIDAD_UM),SECTOR=as.factor(SECTOR),
               DIABETES=as.factor(DIABETES),EPOC=as.factor(EPOC),OBESIDAD=as.factor(OBESIDAD),ASMA=as.factor(ASMA),INMUSUPR=as.factor(INMUSUPR),
               HIPERTENSION=as.factor(HIPERTENSION),ORIGEN=as.factor(ORIGEN),SEXO=as.factor(SEXO),
               INTUBADO=as.factor(INTUBADO),RENAL_CRONICA=as.factor(RENAL_CRONICA),
               SECENT=factor(paste(ENTIDAD_UM,SECTOR,sep = ".")))


##############################################################
#Modelo
##############################################################

muerte=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),evento==0,
                      tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                      !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR)) %>% 
  filter(FECHA_INGRESO<="2020-07-01")

x=model.matrix(~DIABETES+EPOC+OBESIDAD+HIPERTENSION+DIABETES*OBESIDAD*HIPERTENSION+
                  SEXO+RENAL_CRONICA,data=muerte)
x_hosp=model.matrix(~EPOC+OBESIDAD+RENAL_CRONICA+ASMA+INMUSUPR,data=muerte)

x2=model.matrix(~DIABETES+EPOC+OBESIDAD+HIPERTENSION+DIABETES*OBESIDAD*HIPERTENSION+
                 SEXO+RENAL_CRONICA+SECTOR,data=muerte) #cambiar el orden de la referencia (si, no)

x=x[,-1]
x_hosp=x_hosp[,-1]


x2=x2[,-1]



options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


inits1=list(list(mu_raw_mort=-1.5,alpha_raw=0.01),
           list(mu_raw_mort=-1.5,alpha_raw=0.01))


########################################################
#Modelos de la trayectoria Muerte
########################################################
############################
#Sin jerarquia
############################

sin_jer=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(muerte$tiempo_hosp),
  y_hosp=as.numeric(muerte$tiempo_hosp),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

mfit1<-stan(
  file="Stan/ModeloQR.stan",
  data=sin_jer,chains = 2,iter = 3000,init = inits1,
  control = list(adapt_delta = 0.8)
)

pairs(mfit1,pars = "theta")

mcmc_areas(
  mfit1,
  regex_pars = c("beta"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)

############################
#Con jerarquia 1
############################

jer_1=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(muerte$tiempo_hosp),
  y_hosp=as.numeric(muerte$tiempo_hosp),
  Gniv1=length(levels(muerte$ENTIDAD_UM)),
  Niv1=as.numeric(muerte$ENTIDAD_UM),
  x=x2,
  M=ncol(x2),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

inits2=list(list(mu_raw_mort=-2.5,alpha_raw=0.01),
           list(mu_raw_mort=-2.5,alpha_raw=0.01))

mfit2<-stan(
  file="Stan/ModeloJerQR.stan",
  data=jer_1,chains = 2,iter = 3000,init = inits2,
  control = list(adapt_delta = 0.8)
)

pairs(mfit2,pars=c("mu_l_raw"))

mcmc_trace(posterior2,regex_pars =c("beta"))
mcmc_intervals(
  mfit2,
  regex_pars = c("beta"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)

############################
#Con jerarquia 2
############################

jer_2=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(muerte$tiempo_hosp),
  y_hosp=as.numeric(muerte$tiempo_hosp),
  Gniv1=length(levels(muerte$ENTIDAD_UM)),
  Gniv2=length(levels(muerte$SECTOR)),
  Niv1=as.numeric(muerte$ENTIDAD_UM),
  Niv2=as.numeric(muerte$SECTOR),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

inits3=list(list(mu_raw_mort=-2.5,alpha_raw=0.01),
            list(mu_raw_mort=-2.5,alpha_raw=0.01))

mfit3<-stan(
  file="Stan/ModeloJer2QR.stan",
  data=jer_2,chains = 2,iter = 3000,init = inits3,
  control = list(adapt_delta = 0.8)
)

mcmc_intervals(
  mfit3,
  regex_pars = c("mu_l"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)

############################
#Con jerarquia 2 modif
############################

jer_2modi=list(
  N=length(muerte$tiempo_muerte),
  y_mort=as.numeric(muerte$tiempo_muerte),
  N2=length(muerte$tiempo_hosp),
  y_hosp=as.numeric(muerte$tiempo_hosp),
  Gniv1=length(levels(muerte$ENTIDAD_UM)),
  Gniv2=length(levels(muerte$SECENT)),
  Niv1=as.numeric(muerte$ENTIDAD_UM),
  Niv2=as.numeric(muerte$SECENT),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

inits4=list(list(mu_raw_mort=-2.5,alpha_raw=0.01),
            list(mu_raw_mort=-2.5,alpha_raw=0.01))

mfit4<-stan(
  file="Stan/ModeloJer2QR.stan",
  data=jer_2modi,chains = 2,iter = 3000,init = inits4,
  control = list(adapt_delta = 0.8)
)


mcmc_intervals(
  mfit4,
  regex_pars = c("mu_l"),
  prob = 0.8,
  prob_outer = 0.95,
  point_est = "median"
)


pairs(mfit2,pars = c("mu_l_raw[1]","mu_l_raw[29]"))
