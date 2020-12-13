library(cmdstanr)

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


modQR <- cmdstan_model(stan_file = "Stan/ModeloQR.stan",cpp_options=list(stan_threads=TRUE))

fitQR <- modQR$sample(data=sin_jer,
                      init=inits1,
                      chains = 2,
                      parallel_chains = 2,
                      threads_per_chain = 2,
                      iter_warmup = 1000,
                      iter_sampling = 1000)
