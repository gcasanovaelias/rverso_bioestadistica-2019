# Paquetes ----
library(tidyverse)

# vectores ----
x <- seq(from = 0, to = 100, by = 10)
x[5]
x[5:7]
x[-(5:7)]

# data frames ----
data("iris")
class(iris)
View(iris)
iris$Petal.Length

# Pipeline ----

sqrt(log(mean(c(2:10))))

c(2:10) %>% mean() %>% log() %>% sqrt()

# tidy data ----

#  summarise() y group_by() ----

summarise(iris, media_petalo = mean(Petal.Length))

iris %>% group_by(Species)
                  
iris %>% group_by(Species) %>% summarise(mean(Petal.Length))

mean <- iris %>% group_by(Species) %>% summarise(mean.petal = mean(Petal.Length))

mean <- iris %>% group_by(Species) %>% 
  summarise(mean.petal = mean(Petal.Length),
            sd.petal = sd(Petal.Length),
            N = n())

#  summarize_all() ----
  #le aplica la funcion a todas las variables (columnas)

iris %>% group_by(Species) %>% summarise_all(funs(mean))

iris %>% group_by(Species) %>% summarise_all(funs(mean, sd))

#  filter ----

iris %>% group_by(Species) %>% 
  filter(Petal.Length > 3) %>% 
  summarise(N = n())

iris %>% group_by(Species) %>% 
  filter(Petal.Length > 3 | Sepal.Length > 2) %>% 
  summarise(N = n())

#  select() ----
  #nasa no existe

class(nasa)

nasa2 <- as.data.frame(nasa)

colnames(nasa2)

nasa2 <- as.data.frame(nasa) %>% 
  dplyr::select(month, year, ozone, temperature) %>% 
  group_by(month, year) %>% #ordenar por mes y despues por año
  summarise_all(funs(mean))

# Ejercicios ----

data("storms")

class(storms)

storms %>% colnames()

storms$status %>% unique()
  
# s/ modificiones

storms %>% 
  filter(status == 'hurricane') %>% 
  group_by(year) %>% 
  select(wind,hu_diameter) %>% 
  summarise_all(funs(mean))

# filtrando NA

storms %>% 
  filter(status == 'hurricane', !is.na(hu_diameter)) %>% 
  group_by(year) %>% 
  select(wind,hu_diameter) %>% 
  summarise_all(funs(mean))

# removiendo NA (ra.rm = T)

x <- c(2,4,6,8,NA)
mean(x)
mean(x, na.rm = T)

storms2 <- storms %>% 
  filter(status == 'hurricane') %>% 
  group_by(year) %>% 
  select(wind,hu_diameter) %>% 
  summarise_all(funs(mean), na.rm = T)

# Graficando ----

ggplot(storms2, aes(x = year, y = wind)) +
  geom_point() +
  geom_path() + 
  theme_classic()
  


