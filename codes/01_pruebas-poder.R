# Apuntes ----

# Poder: 1 - beta, confianza de que los fuegos son detectados
#* beta: probablidad de que no suene la alarma cuando hay fuego

# ¿Cuantos individuos necesitamos para hacer este estudio? n

# Para calcular el poder necesitamos la varianza estimada y para esta ultima necesitamo haber hecho un estudio preliminar
# ¿Cual es la diferencia que queremos encontrar? delta

# TERMINOS
#* k: número de grupos (dietas)
#* alpha: probabilidad de cometer error tipo I (H0 es verdadera y se rechaza)
#* beta: probabilidad de cometer error tipo II (H0 falsa y se aprueba). Relacionada con la potencia de la prueba (1-beta)
#* potencia de la prueba: probabilidad de rechazar ña hipotesis nula cuando es falsa (1-beta)
#* delta: mínima diferencia que queremos encontrar entre grupos (2 kg). Si las diferencias fuesen menor que 2 pasarían de ser percibidas
#* sigma: desviación estandar. 3.14
#* n: número de individuos POR GRUPO. Se tiene que ir tanteando

# Packages ----

install.packages("pwr2")
library(pwr2)
library(tidyverse)

# Estimación de poder ----

dietSample <- read_csv('https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/AyudantiaStatsPres/Clase4/dietsample.csv')

# 3 dietas. Entre mayor numero de grupo vamos a requerir de una mayor cantidad de individuos para obtener el mismo poder
# alpha = 0.05

# desviación estándar9

sd(dietSample$weightLoss)

augment(aov(weightLoss ~ Diet, data = dietSample))$.resid %>% sd()

aov(weightLoss ~ Diet, data = dietSample) %>% 
  residuals() %>%
  sd()

# beta = 0.8, poder = (1 - beta) = 0.2
# ¿delta minimo? ¿2kg?
#  cuanto más alto el delta significa mayor inversión
#  no hay valores fijos o previamente armados en la literatura, depende del investigador.
#  ususalmente es muy dificil de determinar a no ser que se cuente con elevado conocimiento del objeto o grupo de investigado

# pwr2 ----
# TENEMOS QUE PENSAR CUAL ES LA DIFERENCIA QUE QUEREMOS CALCULAR

pwr2::pwr.1way(k = 3,n = 5,alpha = 0.05,delta = 2,sigma = 3.14)
# pwr.1way(): nos entrega un poder calculado, se realiza anova de 1 vía
# tenemos que ir ingresando parámetros y valores
  #* k: número de grupos (dietas)
  #* alpha: probabilidad de cometer error tipo I (H0 es verdadera y se rechaza)
  #* beta: probabilidad de cometer error tipo II (H0 falsa y se aprueba). Relacionada con la potencia de la prueba (1-beta)
    #* potencia de la prueba: probabilidad de rechazar ña hipotesis nula cuando es falsa (1-beta)
  #* delta: mínima diferencia que queremos encontrar entre grupos (2 kg). Si las diferencias fuesen menor que 2 pasarían de ser percibidas
  #* sigma: desviación estandar. 3.14
  #* n: número de individuos POR GRUPO. Se tiene que ir tanteando 

  #* power = 0.1147236

pwr2::pwr.1way(k = 3,n = 70,alpha = 0.05,delta = 2,sigma = 3.14)
# a medida que aumenta el n aumenta el poder, ususalmente buscamos un poder del orden de 90%

pwr2::pwr.1way(k = 3,n = 70,alpha = 0.05,delta = 5,sigma = 3.14)
# si delta aumenta esto significa que el efecto es mayor y se requiere de menor demostración (requiere menos detalle), 
  # tambien depende del valor de sigma (sd) que posea el grupo/objeto, si sigma (sd) < delta entonces eso permite obtener mayor poder (se requiere menos muestras para determinar diferencias en un grupo con variacion tan pequeña)
# por lo que si delta (diferencia minima que quiero investigar) aumenta, tambien aumenta el poder con la misma cantidad de individuos

# ¿Qué significa un poder de 0.8? ----
#  la p(x) de tener un error del tipo 2 es de un 20%. Es decir, un 20% de probabilidades de decir que existen diferencias entre las dietas cuando en realidad no las hay
#  beta es el inverso de alfa

########### 2da parte #############

# Prueba t de Student ----
# Pruebas de t para 1 o 2 muestras 

#  Prueba de 1 muestra ----

# H0: media de la poblacion = media teórica

data("faithful") # Guardaparques: géiser hace erupción c/ hora
# eruptions: duración en minutos de la erupción
# waiting: cuantos minutos hay que esperar entre erupciones

# media teorica = 60 min
# ¿confiamos en ese valor o lo ponemos a prueba?

t.test(x = faithful$waiting,mu = 60,alternative = 'two.sided')
# two.sided: determinar si las medias son distintas
# less / greater: determinar si la media es mayor o menor a la teorica

# p-value < 2.2e-16 H0 se rechaza (la probabilidad de cometer un error del tipo I es muy baja)
# la media verdadera no es igual a 60 min, guardaparques miente

# 95 percent confidence interval:69.27418 72.51994

# Ejercicio ----

data("airquality") # Partículas de O3 para cada día de mayo a septiembre de 1973 entre 13:00 a 15:00

# Disminuir a la mitad los pasajes del metro de NY los meses que en promedio tengan sobre 55 ppb
# Comprobar estadísticamente qué meses tienen promedios sobre 55

airquality %>% group_by(Month) %>% 
  summarise(O3.mean = mean(Ozone,na.rm = T),
            O3.sd = sd(Ozone,na.rm = T))

julio <- airquality %>% 
  filter(Month == 7)

agosto <- airquality %>% 
  filter(Month == 8)

septiembre <- airquality %>% 
  filter(Month == 9)

t.test(x = julio$Ozone,mu = 55,alternative = 'greater') # H0: media julio =< 55
  # Sí es mayor que 55

t.test(x = agosto$Ozone,mu = 55,alternative = 'greater')
  # Sí es mayor que 55

t.test(x = septiembre$Ozone,mu = 55,alternative = 'greater')


