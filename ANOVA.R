library(readxl)
library(tidyverse)
library(patchwork)

#--------------------------
#LIMPIAR DATOS
datos <- read_excel("ANOVA.xlsx", 
                            sheet = "Resultados")

datos$ENTRECRUZANTE <- as.factor(datos$ENTRECRUZANTE)
datos$CATALIZADOR <- as.factor(datos$CATALIZADOR)
datos$TIEMPO_UV <- as.factor(datos$TIEMPO_UV)

#-------------------
#MODELOS
VARIABLE <- "HINCHADO"
VARIABLE <- "PORCENTAJE"

modelo_simplificado <- aov(
  as.formula(paste(VARIABLE, "~ ENTRECRUZANTE * CATALIZADOR + 
                                 ENTRECRUZANTE * TIEMPO_UV + 
                                 CATALIZADOR * TIEMPO_UV")),
  data = datos
)
summary(modelo_simplificado)


par(mfrow=c(2,2))
plot(modelo_simplificado)

TukeyHSD(modelo_simplificado, "ENTRECRUZANTE")
TukeyHSD(modelo_simplificado, "CATALIZADOR")
TukeyHSD(modelo_simplificado, 'TIEMPO_UV')

#------------
#GRÁFICOS
pd <- position_dodge(width = 0.4)

colores <- c("HCl" = "#E41A1C", "Na2HPO4" = "#4DAF4A", "No catalyst" = "#377EB8")

# p1: Hinchado
p1 <- ggplot(datos, aes(x = ENTRECRUZANTE, y = HINCHADO, color = CATALIZADOR, group = CATALIZADOR)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = pd) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, position = pd) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = pd, alpha = 0.7) +
  scale_color_manual(values = colores,
                     labels = c(
                       "Na2HPO4" = expression(Na[2]*HPO[4]),
                       "HCl" = "HCl",
                       "No catalyst" = "No catalyst"
                     )) +
  labs(x = "Crosslinker", y = "Swelling degree", color = "Catalyst") +
  theme_bw(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

# p2: Porcentaje
p2 <- ggplot(datos, aes(x = ENTRECRUZANTE, y = PORCENTAJE, color = CATALIZADOR, group = CATALIZADOR)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = pd) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, position = pd) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = pd, alpha = 0.7) +
  scale_color_manual(values = colores) +  
  labs(x = "Crosslinker", y = "Sorption (%)") +
  theme_bw(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",  
    axis.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

figura <- (p1 | p2) +
  plot_layout(guides = "collect")+
  plot_annotation(
    title = "Crosslinker-catalyst interaction",
    theme = theme(
      text = element_text(family = "serif"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
    )
  )

ggsave(
  filename = "crosslinker_catalyst_interaction.png",
  plot = figura,
  width = 10,     
  height = 5.5,
  dpi = 1200       
)

#--------------------
#CONDICIONES MODELO
resst <- rstandard(modelo_simplificado)
plot(resst~fitted(modelo_simplificado))
qqnorm(resst)
qqline(resst)
shapiro.test(resst)

