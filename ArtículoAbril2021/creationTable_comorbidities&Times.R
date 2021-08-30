source("./helpers/creation_df2.R")

# También se puede cargar
#df2 = read.csv("./df2_20210617.csv")

library(tableone)

set.seed(12345)

hosp_na=df2 %>% filter(tiempo_hosp>1) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()

muerte_na=df2 %>% filter(
  tiempo_muerte>=0,evento == 0) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()


##########################
### tableOne para hosp ###
##########################

myVars_h = c("tiempo_hosp",
           "EPOC",
           "OBESIDAD",
           "RENAL_CRONICA",
           "ASMA",
           "INMUSUPR",
           "SEXO",
           "EDAD"
           )

t_h = CreateTableOne(vars=myVars_h,strata="SECTOR",data=hosp_na,factorVars = c("EPOC",
                                                                               "OBESIDAD",
                                                                               "RENAL_CRONICA",
                                                                               "ASMA",
                                                                               "INMUSUPR",
                                                                               "SEXO"))
t2_h = as.data.frame(print(t_h, formatOptions = list(big.mark = ","),nonnormal = T,missing=T,test=F))

colnames(t2_h) = c("State managed",
                 "IMSS",
                 "ISSSTE",
                 "Private healthcare provider",
                 "SEDENA/SEMAR/PEMEX",
                 "SSA",
                 "Missing data")

rownames(t2_h) = c("n",
                 "Time from symptoms to hospitalization (median [IQR])",
                 "COPD = NO (%)",
                 "Obesity = NO (%)",
                 "Chronic Kidney = NO (%)",
                 "Asthma = NO (%)",
                 "Immunosuppression = NO (%)",
                 "Female = NO (%)",
                 "Age = NO (%)"
                 )

#write.csv(t2_h,"./ArtículoAbril2021/tableOne-comorbidities-hosp.csv")


############################
### tableOne para muerte ###
############################

myVars_m = c("tiempo_muerte",
             "DIABETES",
             "EPOC",
             "OBESIDAD",
             "HIPERTENSION",
             "SEXO",
             "RENAL_CRONICA",
             "EDAD"
             )



t_m = CreateTableOne(vars=myVars_m,strata="SECTOR",data=muerte_na,factorVars = c("DIABETES",
                                                                                 "EPOC",
                                                                                 "OBESIDAD",
                                                                                 "HIPERTENSION",
                                                                                 "SEXO",
                                                                                 "RENAL_CRONICA"))
t2_m = as.data.frame(print(t_m, formatOptions = list(big.mark = ","),nonnormal = T,missing=T,test=F))

colnames(t2_m) = c("State managed",
                 "IMSS",
                 "ISSSTE",
                 "Private healthcare provider",
                 "SEDENA/SEMAR/PEMEX",
                 "SSA",
                 "Missing data")

rownames(t2_m) = c("n",
                 "Time from hospitalization to death (median [IQR])",
                 "Diabetes = NO (%)",
                 "COPD = NO (%)",
                 "Obesity = NO (%)",
                 "Hypertension = NO (%)",
                 "Female = NO (%)",
                 "Chronic Kidney = NO (%)",
                 "Age = NO (%)"
                 )

#write.csv(t2_m,"./ArtículoAbril2021/tableOne-comorbidities-muerte.csv")



### Deprecated ###

#write.csv(t2,"./ArtículoAbril2021/tableOne_comorbidities&Times.csv")