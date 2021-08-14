source("./helpers/creation_df2.R")

library(tableone)

colnames(df2)
myVars = c("tiempo_hosp",
           "tiempo_muerte",
           "DIABETES",
           "EPOC",
           "OBESIDAD",
           "HIPERTENSION",
           "SEXO",
           "RENAL_CRONICA",
           "EDAD",
           "ASMA",
           "INMUSUPR"
           )


t = CreateTableOne(vars=myVars,strata="SECTOR",data=df2)
t2 = as.data.frame(print(t, formatOptions = list(big.mark = ","),nonnormal = T,missing=T,)) %>% 
  select(-c(test,p))

colnames(t2) = c("State managed",
                 "IMSS",
                 "ISSSTE",
                 "Private healthcare provider",
                 "SEDENA/SEMAR/PEMEX",
                 "SSA",
                 "Missing data")

rownames(t2) = c("n",
                 "Time from symptoms to hospitalization (median [IQR])",
                 "Time from hospitalization to death (median [IQR])",
                 "Diabetes = NO (%)",
                 "COPD = NO (%)",
                 "Obesity = NO (%)",
                 "Hypertension = NO (%)",
                 "Female = NO (%)",
                 "Chronic Kidney = NO (%)",
                 "Age = NO (%)",
                 "Asthma = NO (%)",
                 "Immunosuppression = NO (%)"
                 )

#write.csv(t2,"./ArtículoAbril2021/tableOne_comorbidities&Times.csv")
