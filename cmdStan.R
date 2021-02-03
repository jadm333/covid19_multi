####

cambioCVE <- function(df){
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="01","AS"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="02","BC"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="03","BS"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="04","CC"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="07","CS"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="08","CH"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="09","DF"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="05","CL"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="06","CM"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="10","DG"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="11","GT"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="12","GR"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="13","HG"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="14","JC"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="15","MC"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="16","MN"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="17","MS"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="18","NT"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="19","NL"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="20","OC"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="21","PL"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="22","QT"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="23","QR"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="24","SP"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="25","SL"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="26","SR"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="27","TC"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="28","TL"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="29","TS"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="30","VZ"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="31","YN"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="32","ZS"))
  return(df)
}
cambioCVE_nombreEstado_completo <- function(df){
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="01","Aguascalientes"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="02","Baja California"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="03","Baja California Sur"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="04","Campeche"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="07","Chiapas"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="08","Chihuahua"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="09","Mexico City"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="05","Coahuila"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="06","Colima"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="10","Durango"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="11","Guanajuato"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="12","Guerrero"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="13","Hidalgo"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="14","Jalisco"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="15","Mexico"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="16","Michoacan"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="17","Morelos"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="18","Nayarit"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="19","Nuevo Leon"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="20","Oaxaca"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="21","Puebla"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="22","Queretaro"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="23","Quintana Roo"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="24","San Luis Potosi"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="25","Sinaloa"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="26","Sonora"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="27","Tabasco"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="28","Tlaxcala"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="29","Tamaulipas"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="30","Veracruz"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="31","Yucatan"))
  df <- df %>% mutate(ENTIDAD_UM=replace(ENTIDAD_UM,ENTIDAD_UM=="32","Zacatecas"))
  return(df)
}

#####

library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(lubridate)
library(posterior)
library(tidybayes)


df=read_csv("Data/210107COVID19MEXICO.csv",na=c("","NA","97","98","99","9999-99-99"))
corte=ymd("2021-01-07")

#df=cambioCVE(df)
df=cambioCVE_nombreEstado_completo(df)

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
                  SECENT=factor(paste(ENTIDAD_UM,SECTOR,sep = " ")))


##############################################################
#Modelo
##############################################################


set.seed(12345)

hosp=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),
                    tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                    !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR),tiempo_hosp>1) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()

muerte=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),evento==0,
                      tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                      !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR)) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()



x=model.matrix(~DIABETES+EPOC+OBESIDAD+HIPERTENSION+DIABETES*OBESIDAD*HIPERTENSION+
                 SEXO+RENAL_CRONICA,data=muerte)
x_hosp=model.matrix(~EPOC+OBESIDAD+RENAL_CRONICA+ASMA+INMUSUPR,data=hosp)



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
  N2=length(muerte$tiempo_hosp),
  y_hosp=as.numeric(muerte$tiempo_hosp),
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
  Gnivh1=length(levels(hosp$ENTIDAD_UM)),
  Gnivh2=length(levels(hosp$SECENT)),
  Nivh1=as.numeric(hosp$ENTIDAD_UM),
  Nivh2=as.numeric(hosp$SECENT),
  x=x,
  M=ncol(x),
  x_hosp=x_hosp,
  M_hosp=ncol(x_hosp)
)

write_stan_json(jer_2modi_h,file = "Cmdstan/jer_2modi.json")



##### Extras #######


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
