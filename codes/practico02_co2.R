# Packages ----
install.packages('plotly')
library(tidyverse)
library(plotly)

# Leer desde github ----

read.csv('https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/AyudantiaStatsPres/Clase2/EducacionChile.csv')

EducacionChile <- read_csv('https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/AyudantiaStatsPres/Clase2/EducacionChile.csv')

EducacionChile %>% 
  group_by(Administration) %>% 
  select(Average.PSU, 
         Average.NEM, 
         Number.of.records) %>% 
  summarise_all(funs(mean)) %>% View()

EducacionChile %>% 
  group_by(Administration) %>% 
  summarise(Mean.PSU = mean(Average.PSU, na.rm = T),
            SD.PSU = sd(Average.PSU, na.rm = T),
            N =n())
  
# Media pesada
  # Hay elementos que van a tener más peso en el promedio 

EducacionChile %>% 
  group_by(Administration) %>% 
  summarise(Mean.PSU = weighted.mean(x = Average.PSU,
                                     w = Number.of.records, #w: vector de los pesos
                                     na.rm = T), 
            SD.PSU = sd(Average.PSU, na.rm = T),
            N =n())

# Cambiar los nombres aceptables por R ----

colnames(EducacionChile) = make.names(EducacionChile)

# Visualizacion de datos con ggplot2 ----

ggplot(EducacionChile, aes(x = Administration, y = Average.PSU)) + 
  geom_boxplot()

ggplot(EducacionChile, aes(x = Administration, y = Average.PSU)) + 
  geom_boxplot(outlier.shape = NA)

# Boxplot
  #* linea gruesa: mediana
  #* caja: cuantiles 25% y 75%
  #* bigotes: intervalo de confianza del 95%
  #* outliers

g <- ggplot(EducacionChile, aes(x = Administration, y = Average.PSU)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Administration,
                  size = Number.of.records)) + 
  theme(legend.position = 'bottom')

# plotly 

ggplotly(g)

###################################################
################### Actividad 2 ###################
###################################################

# Captacion de CO2 en plantas ----

data("CO2") #tidyverse

# Observaciones de la base de datos

#  General ----

# La base de datos original posee pseudorepeticiones
CO2real <- CO2 %>% 
  group_by(Plant, Type, Treatment) %>% 
  summarise(Captacion.mean = mean(uptake))

CO2 %>% 
  group_by(Type, Treatment) %>% 
  summarise(Captacion.mean = mean(uptake))

CO2 %>% 
  group_by(Plant) %>%
  select(Plant, conc, uptake) %>% 
  summarise_all(funs(mean))

#  Según planta ----

CO2 %>% 
  group_by(Plant) %>%
  summarise(Captacion.mean = mean(uptake),
            Captacion.sd = sd(uptake),
            N = n())

ggplot(CO2, aes(x = Plant, y = uptake, color = Plant)) + 
  geom_boxplot(outlier.shape = NA)

ggplot(CO2, aes(x = Plant, y = uptake, fill = Plant)) + 
  geom_boxplot(outlier.shape = NA)

#  Según variedad de la planta ----

CO2 %>% 
  group_by(Type) %>%
  summarise(Captacion.mean = mean(uptake),
            Captacion.sd = sd(uptake),
            N = n())

ggplot(CO2, aes(x = Type, y = uptake)) + 
  geom_boxplot(fill = c('dark red', 'dark blue'), outlier.shape = NA)

#  Según tratamiento ----

CO2 %>% 
  group_by(Treatment) %>%
  summarise(Captacion.mean = mean(uptake),
            Captacion.sd = sd(uptake),
            N = n())

ggplot(CO2, aes(x = Treatment, y = uptake)) + 
  geom_boxplot(color = c('dark green', 'dark orange'), outlier.shape = NA)

# Observar 3 variables en el gráfico

ggplot(CO2, aes(x = Treatment, y = uptake)) + 
  geom_boxplot(aes(fill = Type))

ggplot(CO2, aes(x = Treatment, y = uptake, fill = Type)) + 
  geom_boxplot()

