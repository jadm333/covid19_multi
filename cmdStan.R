library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(lubridate)
library(posterior)
library(tidybayes)

#set_cmdstan_path(path="C:/Users/marco/cmdstan")

df=read_csv("Data/201212COVID19MEXICO.csv",na=c("","NA","97","98","99","9999-99-99"))
corte=ymd("2020-12-12")


df2=df %>% distinct(ID_REGISTRO,.keep_all = T) %>%
  filter(CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL==2 | CLASIFICACION_FINAL==3,
         TIPO_PACIENTE==2) %>%
  mutate(
    SECTOR = as.factor(case_when(
      SECTOR==4 | SECTOR==5 ~ "IMSS",
      SECTOR==6 ~ "ISSSTE",
      SECTOR==9 ~ "PRIVADA",
      SECTOR==3 | SECTOR==7 ~ "ESTATAL",
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

hosp=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),
                    tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                    !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR)) 

muerte=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),evento==0,
                      tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                      !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR))

x=model.matrix(~DIABETES+EPOC+OBESIDAD+HIPERTENSION+DIABETES*OBESIDAD*HIPERTENSION+
                 SEXO+RENAL_CRONICA,data=muerte)
x_hosp=model.matrix(~EPOC+OBESIDAD+RENAL_CRONICA+ASMA+INMUSUPR,data=hosp)

ggplot(hosp,aes(x=tiempo_hosp)) + geom_density() + facet_wrap(~SECTOR)

x=x[,-1]
x_hosp=x_hosp[,-1]


inits1=list(list(mu_raw_mort=-1.5,alpha_raw=0.01),
            list(mu_raw_mort=-1.5,alpha_raw=0.01),
            list(mu_raw_mort=-1.5,alpha_raw=0.01))

write_stan_json(inits1,file = "Cmdstan/initis1.json")


########################################################
#Modelos de la trayectoria Muerte
########################################################
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

modQR <- cmdstan_model(stan_file = "Stan/ModeloQR_reduce.stan",cpp_options=list(stan_threads=TRUE))

fitQR <- modQR$sample(data=sin_jer,
                      init=inits1,
                      chains = 3,
                      parallel_chains = 3,
                      threads_per_chain = 3,
                      iter_warmup = 750,
                      iter_sampling = 750)

y_rep_hosp=fitQR$draws("y_hosp_tilde")
y_rep_hosp=as_draws_matrix(y_rep_hosp)

y_rep_mort=fitQR$draws("y_mort_tilde")
y_rep_mort=as_draws_matrix(y_rep_mort)

ppc_dens_overlay(sin_jer$y_hosp, y_rep_hosp[1:200, ])
ppc_dens_overlay(sin_jer$y_mort, y_rep_mort[1:200, ])

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
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

inits2=list(list(mu_raw_mort=-2.5,alpha_raw=0.01),
            list(mu_raw_mort=-2.5,alpha_raw=0.01),
            list(mu_raw_mort=-2.5,alpha_raw=0.01))

write_stan_json(jer_1,file = "Cmdstan/jer_1.json")
write_stan_json(inits2,file = "Cmdstan/inits2.json")

modJerQR <- cmdstan_model(stan_file = "Stan/ModeloJerQR_reduce.stan",cpp_options=list(stan_threads=TRUE))

fitJerQR <- modJerQR$sample(data=jer_1,
                      init=inits2,
                      chains = 3,
                      parallel_chains = 3,
                      threads_per_chain = 3,
                      iter_warmup = 750,
                      iter_sampling = 750)

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
            list(mu_raw_mort=-2.5,alpha_raw=0.01),
            list(mu_raw_mort=-2.5,alpha_raw=0.01))

write_stan_json(jer_2,file = "Cmdstan/jer_2.json")
write_stan_json(inits2,file = "Cmdstan/inits3.json")


modJer2QR <- cmdstan_model(stan_file = "Stan/ModeloJer2QR_reduce.stan",cpp_options=list(stan_threads=TRUE))

fitJer2QR <- modJer2QR$sample(data=jer_2,
                      init=inits3,
                      chains = 3,
                      parallel_chains = 3,
                      threads_per_chain = 3,
                      iter_warmup = 750,
                      iter_sampling = 750)


############################
#Con jerarquia 2 modif no correr
############################

# jer_2modi=list(
#   N=length(muerte$tiempo_muerte),
#   y_mort=as.numeric(muerte$tiempo_muerte),
#   N2=length(muerte$tiempo_hosp),
#   y_hosp=as.numeric(muerte$tiempo_hosp),
#   Gniv1=length(levels(muerte$ENTIDAD_UM)),
#   Gniv2=length(levels(muerte$SECENT)),
#   Niv1=as.numeric(muerte$ENTIDAD_UM),
#   Niv2=as.numeric(muerte$SECENT),
#   x=x,
#   M=ncol(x),
#   x_hosp=x_hosp,
#   M_hosp=ncol(x_hosp)
# )
#
# inits4=list(list(mu_raw_mort=-2.5,alpha_raw=0.01),
#             list(mu_raw_mort=-2.5,alpha_raw=0.01),
#             list(mu_raw_mort=-2.5,alpha_raw=0.01))
#
#
# fitJer2QRmodi<- modJer2QR$sample(data=jer_2modi,
#                               init=inits4,
#                               chains = 3,
#                               parallel_chains = 3,
#                               threads_per_chain = 3,
#                               iter_warmup = 750,
#                               iter_sampling = 750)
#
# y_rep_hosp=fitJer2QRmodi$draws("y_hosp_tilde")
# y_rep_hosp=as_draws_matrix(y_rep_hosp)
#
# y_rep_mort=fitJer2QRmodi$draws("y_mort_tilde")
# y_rep_mort=as_draws_matrix(y_rep_mort)
#
# ppc_dens_overlay(jer_2modi$y_hosp, y_rep_hosp[1:200, ])
# ppc_dens_overlay(jer_2modi$y_mort, y_rep_mort[1:200, ])

############################
#Con jerarquia 2 modif alpha
############################

jer_2modi_h=list(
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

inits5=list(list(mu_raw_mort=-2.5,alpha_raw=0.01),
            list(mu_raw_mort=-2.5,alpha_raw=0.01),
            list(mu_raw_mort=-2.5,alpha_raw=0.01))

modJer2QR_h <- cmdstan_model(stan_file = "Stan/ModeloJer2QRhosp_reduce.stan",cpp_options=list(stan_threads=TRUE))

fitJer2QRmodi_h <- modJer2QR_h$sample(data=jer_2modi_h,
                                      init=inits5,
                                      chains = 3,
                                      parallel_chains = 3,
                                      threads_per_chain = 3,
                                      iter_warmup = 750,
                                      iter_sampling = 750)

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

fitJer2QRmodi_h=readRDS("Fit/fitJer2QRmodi_h.rds")

longFormat_y_rep_hosp <- gather_draws(as_draws_df(fitJer2QRmodi_h$draws()),y_hosp_tilde[id]) %>% ungroup()

longFormat_y_rep_mort <- gather_draws(as_draws_df(fitJer2QRmodi_h$draws()),y_mort_tilde[id]) %>% ungroup()

Estados <- muerte %>% mutate(id=row_number()) %>% select(ENTIDAD_UM,id)

longFormat_y_rep_hosp <- left_join(Estados,longFormat_y_rep_hosp,by=c("id"="id")) %>%
  write_csv("~/Documents/Github/covid19_epi/data/longFormat_y_rep_hosp.csv")

longFormat_y_rep_mort <- left_join(Estados,longFormat_y_rep_mort,by=c("id"="id")) %>%
  write_csv("~/Documents/Github/covid19_epi/data/longFormat_y_rep_mort.csv")
