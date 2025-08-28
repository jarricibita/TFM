library(tidyverse)
library(readxl)
library(car)
library(broom)
Datos <- read_excel("Tabla_MLR.xlsx", sheet="Filtrados", skip = 2)

Datos$COCIENTE_LAVADO <- as.numeric(Datos$COCIENTE_LAVADO)

Variables <- Datos |> 
  select(GOMA, CL, CATALIZADOR, LUZ, PI, COCIENTE_LAVADO)

escalado <- as.data.frame(scale(Variables))

modelo <- lm(COCIENTE_LAVADO~GOMA+CL+CATALIZADOR+LUZ+PI, data=escalado)

summary(modelo)

autoplot(modelo)

shapiro.test(rstandard(modelo))

vif(modelo)

confint(modelo)

###-----------------------------
#GRAFICO
coefs <- tidy(modelo, conf.int = TRUE)
coefs <- coefs[coefs$term != "(Intercept)", ]
coefs$term <- dplyr::recode(coefs$term,
                            "GOMA" = "Gum",
                            "CL" = "Crosslinker",
                            "CATALIZADOR" = "Catalyst",
                            "LUZ" = "UV-light",
                            "PI" = "Photoinitiator"
)

ggplot(coefs, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 4, color = "#1B4F72") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "#1B4F72", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(-0.6, 1), expand = expansion(mult = c(0.02, 0.05))) +
  theme_classic(base_size = 16, base_family = "serif") +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(size = 17, face = "bold", hjust = 0),
    panel.grid = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Estimated coefficient (± 95% CI)",
    title = "Weighted effect of each variable on crosslinking density"
  )

ggsave("coef_plot.pdf", width = 7, height = 5, dpi = 300)
ggsave("coef_plot.png", dpi = 1200, width = 7, height = 5, bg = "white")

#---------------------------


