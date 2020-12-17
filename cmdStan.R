library(cmdstanr)
library(tidyverse)
library(bayesplot) 
library(lubridate)
library(posterior)

#set_cmdstan_path(path="C:/Users/marco/cmdstan")

df=read_csv("Data/201212COVID19MEXICO.csv",na=c("","NA","97","98","99","9999-99-99"))
corte=ymd("2020-11-04")


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

muerte=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),evento==0,
                      tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                      !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR)) %>% filter(FECHA_DEF<"2020-05-01") %>%
  as.data.frame()

x=model.matrix(~DIABETES+EPOC+OBESIDAD+HIPERTENSION+DIABETES*OBESIDAD*HIPERTENSION+
                 SEXO+RENAL_CRONICA,data=muerte)
x_hosp=model.matrix(~EPOC+OBESIDAD+RENAL_CRONICA+ASMA+INMUSUPR,data=muerte)

ggplot(muerte,aes(x=tiempo_hosp)) + geom_density() + facet_wrap(~SECTOR)

x=x[,-1]
x_hosp=x_hosp[,-1]


inits1=list(list(mu_raw_mort=-1.5,alpha_raw=0.01),
            list(mu_raw_mort=-1.5,alpha_raw=0.01),
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



Jer2draws <- fitJer2QRmodi_h$draws()

#################### COMENTARIO: #################################
### SI CARGAS EL OBJETO fitJer2QRmodi_h.rds, YA NO SE          ###
### TIENE ACCESO DIRECTO A LAS ENTRADAS DEL VECTOR             ###
### y_hosp_tildde y y_mort_tilde. HAY QUE HACERLO MANUALMENTE. ###        ###
##################################################################

Jer2draws[,,"y_hosp_tilde"]

ID_estados <- sprintf("%02d", 1:32)

muertos_por_Estado <- list()
for(i in 1:32){
  muertos_por_Estado[[i]] <- which(muerte$ENTIDAD_UM == ID_estados[i])
  
}

#Aquí tenemos a la entrada correspondiente de y_hosp_tilde[1] en el array Jer2draws
firstEntry_y_hosp_tilde <- which(dimnames(Jer2draws)[[3]] == "y_hosp_tilde[1]")
#Aquí tenemos a la entrada correspondiente de y_mort_tilde[1] en el array Jer2draws
firstEntry_y_mort_tilde <- which(dimnames(Jer2draws)[[3]] == "y_mort_tilde[1]")

#Esta librería es para permutar las muestras por estado
library(gtools)
y_mort_tilde_porEstado <- list()
y_hosp_tilde_porEstado <- list()

for(j in 1:32){
  #Esto es para filtrar por entradas a "y_mort_tilde"
  y_mort_tilde_porEstado[[j]] <- permute(as.vector(Jer2draws[,,muertos_por_Estado[[j]] + firstEntry_y_mort_tilde - 1]))
  #Esto es para filtrar por entradas a "y_hosp_tilde"
  y_hosp_tilde_porEstado[[j]] <- permute(as.vector(Jer2draws[,,muertos_por_Estado[[j]] + firstEntry_y_hosp_tilde - 1]))
}


names(y_mort_tilde_porEstado) <- c("AS","BC","BS","CC","CL","CM","CS","CH","DF","DG","GT","GR","HG","JC","MC","MN","MS","NT","NL","OC","PL","QT","QR","SP","SL","SR","TC","TS","TL","VZ","YN","ZS")
names(y_hosp_tilde_porEstado) <- c("AS","BC","BS","CC","CL","CM","CS","CH","DF","DG","GT","GR","HG","JC","MC","MN","MS","NT","NL","OC","PL","QT","QR","SP","SL","SR","TC","TS","TL","VZ","YN","ZS")

#Alternativa para el nombre de las muestras
#names(y_mort_tilde_porEstado) <- c("AGUASCALIENTES","BAJA CALIFORNIA","BAJA CALIFORNIA SUR",
#                                "CAMPECHE","COAHUILA","COLIMA","CHIAPAS","CHIHUAHUA",
#                                "CIUDAD DE MEXICO","DURANGO","GUANAJUATO","GUERRERO",
#                                "HIDALGO","JALISCO","MEXICO","MICHOACAN","MORELOS","NAYARIT",
#                                "NUEVO LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA ROO",
#                                "SAN LUIS POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS",
#                                "TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")