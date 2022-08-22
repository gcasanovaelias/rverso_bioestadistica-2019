# Packages ----

library(tidyverse)
library(broom)

# Datos ----
# Cuanto duermen los distintos mamiferos

data("msleep")

#  ¿Los mamíferos de distintos ordenes duermen distinta cantidad de horas? ----

msleep %>% group_by(order) %>% 
  summarise(sleep.time.mean = mean(sleep_total),
            sleep.time.sd = sd(sleep_total, na.rm = T),
            N = n())

ggplot(msleep, aes(x = order, y = sleep_total)) +
  geom_boxplot(aes(color = order), outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  scale_y_log10()

aov(sleep_total ~ order, data = msleep)

#  ¿Los mamíferos con distinta dieta duermen distinta cantidad de horas? ----

msleep %>% group_by(vore) %>% 
  summarise(sleep.time.mean = mean(sleep_total),
            sleep.time.sd = sd(sleep_total, na.rm = T),
            N = n())

ggplot(msleep, aes(x = vore, y = sleep_total)) +
  geom_boxplot(aes(color = vore), outlier.shape = NA) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Tenemos que generar una nueva base de datos para que no contenga valores NA

Vore = msleep %>% filter(!is.na(vore))

ggplot(Vore, aes(x = vore, y = sleep_total)) +
  geom_boxplot(aes(color = vore), outlier.shape = NA) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Como los omnivoros son muy distintos los sacare del analisis
Vore2 = msleep %>% filter(!is.na(vore) & vore != 'omni')

ggplot(Vore2, aes(x = vore, y = sleep_total)) +
  geom_boxplot(aes(color = vore), outlier.shape = NA) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# ¿Hay homogeneidad de varianza?

bartlett.test(sleep_total ~ vore, data = Vore)
  #p-value = 0.1041, se cumple H0. H0: var1 = var 2( homogeneidad de varianza)

bartlett.test(sleep_total ~ vore, data = Vore2)
  #p-value = 0.8237, se cumple H0

# Normalidad de los residuos ----
#  Hay que haber hecho el anova
#  Cuando los datos son normales los predichos son las medias

AnovaVore <-aov(sleep_total ~ vore, data = Vore2)

# Generar histogramas

residuales <- AnovaVore$residuals

hist(residuales)
  # bins: agrupaciones de valores
  # hist() tiene incorporado un algoritmo que indica en cuantas bins debieses dividir tus datos


# qqplot

qqnorm(residuales)
qqline(residuales)

#test de shapiro

shapiro.test(residuales)
  # p-value < 0.05. Se rechaza H0, los datos no poseen una distribución normal
  #  

# Resumen de ANOVA ----

summary(AnovaVore)

# muchas funciones de R entregan listas por default lo cual es un mal formato para hacer reportes, broom nos permite trabajar con dataframes

glance(AnovaVore)

tabla1 <- tidy(AnovaVore)

# Guardar resultados

write_csv(x = tabla1, file = 'Tabla1.csv')

# residuales c/ broom

tabla2 <- augment(AnovaVore)

hist(tabla2$.resid)