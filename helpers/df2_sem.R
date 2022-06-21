library(readxl)
library(lubridate)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)

source("./helpers/cambioCVE.R")
### Load in case a version of df2 is available ###
#df2 = read_csv("Data/df2.csv")

### If not a df2 version available, then run code below (will take some time to load) ###
source("./helpers/creation_df2.R")

hosp=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),!is.na(EDAD),!is.na(SEXO),
                    tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                    !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR),tiempo_hosp>1) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()

muerte=df2 %>% filter(!is.na(DIABETES),!is.na(OBESIDAD),!is.na(HIPERTENSION),evento==0,!is.na(EDAD),!is.na(SEXO),
                      tiempo_muerte>=0,tiempo_hosp>=0,!is.na(EPOC),!is.na(RENAL_CRONICA),
                      !is.na(SECTOR),!is.na(ASMA),!is.na(INMUSUPR)) %>% 
  group_by(ENTIDAD_UM,SECTOR) %>% sample_frac(0.20) %>% ungroup()


ranges = list("C3:T34",#"B2:T34",
              "W3:AW34",#"W2:AW34",
              "AY3:BC34"#"AY2:BC34"
)

ranges_colnames = list("B2:T2",#"B2:T34",
                       "W2:AW2",#"W2:AW34",
                       "AY2:BC2"#"AY2:BC34"
)

sem_data <- lapply(ranges, function(each_range){
  read_xlsx("Data/sem_epi20-22.xlsx",sheet = "Hoja1", 
            range = each_range, na = c("", "-"),col_names = F
  )
}) %>% reduce(cbind)

sem_colnames <- lapply(ranges_colnames, function(each_range){
  read_xlsx("Data/sem_epi20-22.xlsx",sheet = "Hoja1", 
            range = each_range, na = c("", "-"),col_names = F
  )
}) %>% reduce(cbind)

colnames(sem_colnames) = 1:dim(sem_colnames)[2]

sem_colnames = sem_colnames %>% pivot_longer(-c(`1`), names_to = "col.number",values_to = "date") %>% 
  mutate(date = ymd(date)) %>% 
  select(date) %>% 
  pull()

colnames(sem_data) = sem_colnames

estados = read_xlsx("Data/sem_epi20-22.xlsx",sheet = "Hoja1", 
                    range = "B2:B34", na = c("", "-"))

sem_data = cbind(estados,sem_data)

# Lo que esta comentado corresponde a errores y modfiicacaciones del archivo original
# descomentar si se va a usar el archivo original y comentar lo equivalente
sem_data_long = sem_data %>% pivot_longer(-Estado,names_to="FECHA_SEM",values_to="sem") %>%
  rename(ENTIDAD_UM = Estado) %>% 
  mutate(ENTIDAD_UM = case_when( ENTIDAD_UM=="Aguascalientes" ~ "01",
                                 ENTIDAD_UM=="Baja California" ~ "02",
                                 ENTIDAD_UM=="Baja California Sur" ~ "03",
                                 ENTIDAD_UM=="Campeche" ~ "04",
                                 ENTIDAD_UM=="Chiapas" ~ "07",
                                 # ENTIDAD_UM=="Chiuhuahua" ~ "08", # en el excel esta mal escrito
                                 ENTIDAD_UM=="Chihuahua" ~ "08", # en el excel esta mal escrito
                                 ENTIDAD_UM=="CDMX" ~ "09",
                                 ENTIDAD_UM=="Coahuila" ~ "05",
                                 ENTIDAD_UM=="Colima" ~ "06",
                                 ENTIDAD_UM=="Durango" ~ "10",
                                 ENTIDAD_UM=="Guanajuato" ~ "11",
                                 ENTIDAD_UM=="Guerrero" ~ "12",
                                 ENTIDAD_UM=="Hidalgo" ~ "13",
                                 ENTIDAD_UM=="Jalisco" ~ "14",
                                 # ENTIDAD_UM=="Estado de México" ~ "15",
                                 ENTIDAD_UM=="Estado de Mexico" ~ "15",
                                 # ENTIDAD_UM=="Michoacán" ~ "16",
                                 ENTIDAD_UM=="Michoacan" ~ "16",
                                 ENTIDAD_UM=="Morelos" ~ "17",
                                 ENTIDAD_UM=="Nayarit" ~ "18",
                                 # ENTIDAD_UM=="Nuevo León" ~ "19",
                                 ENTIDAD_UM=="Nuevo Leon" ~ "19",
                                 ENTIDAD_UM=="Oaxaca" ~ "20",
                                 ENTIDAD_UM=="Puebla" ~ "21",
                                 # ENTIDAD_UM=="Querétaro" ~ "22",
                                 ENTIDAD_UM=="Queretaro" ~ "22",
                                 ENTIDAD_UM=="Quintana Roo" ~ "23",
                                 # ENTIDAD_UM=="San Luis Potosí" ~ "24",
                                 ENTIDAD_UM=="San Luis Potosi" ~ "24",
                                 ENTIDAD_UM=="Sinaloa" ~ "25",
                                 ENTIDAD_UM=="Sonora" ~ "26",
                                 ENTIDAD_UM=="Tabasco" ~ "27",
                                 ENTIDAD_UM=="Tlaxcala" ~ "28",
                                 ENTIDAD_UM=="Tamaulipas" ~ "29",
                                 ENTIDAD_UM=="Veracruz" ~ "30",
                                 # ENTIDAD_UM=="Yucatán" ~ "31",
                                 ENTIDAD_UM=="Yucatan" ~ "31",
                                 ENTIDAD_UM=="Zacatecas"  ~ "32"
  ),
  FECHA_SEM = ymd(FECHA_SEM))

sem_data_long = cambioCVE(sem_data_long)

week_sem_finder = function(date, se_dates = sem_colnames){
  index = sum(!date < sem_colnames)
  if(index != 0){
    return(sem_colnames[index])
  }
  else{
    return(as.Date("2020-01-01"))
  }
}

hosp = hosp %>% 
  rowwise() %>% 
  mutate(FECHA_SEM = week_sem_finder(FECHA_SINTOMAS)) %>% 
  left_join(sem_data_long,by = c("FECHA_SEM","ENTIDAD_UM")) %>% 
  replace_na(list(sem = 1)) %>% 
  mutate(year = factor(format(FECHA_SINTOMAS,format="%Y")),
         semestre = if_else(month(FECHA_SINTOMAS)<=6,
                            paste0(year,"-I"),paste0(year,"-II")),
         semestre = factor(semestre,levels = c("2020-I","2020-II","2021-I")),
         YEARENT=factor(paste(year,ENTIDAD_UM,sep = " ")),
         SEMENT=factor(paste(semestre,ENTIDAD_UM,sep = " ")),
         YEARSECENT=factor(paste(year,ENTIDAD_UM,SECTOR,sep = " ")),
         SEMSECENT=factor(paste(semestre,ENTIDAD_UM,SECTOR,sep = " ")))

muerte = muerte %>% 
  rowwise() %>% 
  mutate(FECHA_SEM = week_sem_finder(FECHA_SINTOMAS)) %>% 
  left_join(sem_data_long,by = c("FECHA_SEM","ENTIDAD_UM")) %>% 
  replace_na(list(sem = 1)) %>% 
  mutate(year = factor(format(FECHA_SINTOMAS,format="%Y")),
         semestre = if_else(month(FECHA_SINTOMAS)<=6,
                            paste0(year,"-I"),paste0(year,"-II")),
         semestre = factor(semestre,levels = c("2020-I","2020-II","2021-I")),
         YEARENT=factor(paste(year,ENTIDAD_UM,sep = " ")),
         SEMENT=factor(paste(semestre,ENTIDAD_UM,sep = " ")),
         YEARSECENT=factor(paste(year,ENTIDAD_UM,SECTOR,sep = " ")),
         SEMSECENT=factor(paste(SEMENT,SECTOR,sep = " ")))



# Puede ser util si se quieren ordenar los factores
# c(
#   "AS", 
#   "BC", 
#   "BS", 
#   "CC", 
#   "CH", 
#   "CL", 
#   "CM", 
#   "CS", 
#   "DF", 
#   "DG", 
#   "GR", 
#   "GT",
#   "HG", 
#   "JC",
#   "MC", 
#   "MN", 
#   "MS", 
#   "NL", 
#   "NT", 
#   "OC", 
#   "PL", 
#   "QR", 
#   "QT", 
#   "SL",
#   "SP", 
#   "SR", 
#   "TC", 
#   "TL", 
#   "TS", 
#   "VZ", 
#   "YN", 
#   "ZS")