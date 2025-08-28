library(rsm)
library(readxl)
library(tidyverse)
#datos_raw <- read_excel("C:/Users/arric/OneDrive - alumni.unav.es/Master/TFM/Resultados/RSM/RSM_AzulMetileno.xlsx", 
#sheet = "Final")
datos <- read_excel("Resultados/RSM/RSM_AzulMetileno.xlsx", 
                        sheet = "RSM")
#------------------------------------------

modelo_p <- rsm(PORCENTAJE_CORR~FO(Xantana_c, Cinamico_c, HCl_c), data=datos_raw)
summary(modelo_p)
modelo_p <- rsm(PORCENTAJE_CORR~SO(XANTANA, CINAMICO, HCL), data=datos_coded)
summary(modelo_p)


#----------------------


