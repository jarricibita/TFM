library(mixexp)
library(tidyverse)
library(readxl)

Datos <- read_excel("Mezclas.xlsx")

Design <- Datos |> 
  select("ITACONICO", "CITRICO", "CINAMICO")

y <- Datos$`HINCHADO FOSFATO`
y <- Datos$`HINCHADO HCl`
y <- Datos$`Q HCl`
y <- Datos$`Q FOSFATO`

#----------------------------
modelo_lineal <- MixModel(Design, "y", mixcomps = c("ITACONICO","CITRICO","CINAMICO"), model = 1)
modelo_cuadratico <- MixModel(Design, "y", mixcomps = c("ITACONICO","CITRICO","CINAMICO"), model = 2)
modelo_cubico <- MixModel(Design, "y", mixcomps = c("ITACONICO","CITRICO","CINAMICO"), model = 3)
modelo_cuartico <- MixModel(Design, "y", mixcomps = c("ITACONICO","CITRICO","CINAMICO"), model = 4)

summary(modelo_lineal)
shapiro.test(modelo_lineal$residuals)
summary(modelo_cuadratico)
shapiro.test(modelo_cuadratico$residuals)
summary(modelo_cuartico)
shapiro.test(modelo_cuartico$residuals)

anova(modelo_lineal, modelo_cuadratico)
anova(modelo_cuadratico, modelo_cuartico)

#--------------------------
