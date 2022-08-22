# Probabilidades inicio ----

# Probabilidad de que salga un 6 en un dado

p_dado6 <- 1/6

# Probabilidad de que salga tres veces 6 al tirar un dado

p_dado6^3

# Probabilidad de que no salga un 6

p_dadoNo6 <- (1-1/6)

# Probabilidad de que dos eventos no ocurran; al lanzar cinco veces un dado nunca obtengamos el valor 6

p_dadoNo6^5

# Probabilidad de que algo ocurra al menos una vez

1-(p_dadoNo6^5) # Al lanzar cinco veces un dado hay un 60% de que al menos una vez salga un valor de 6

# Bonferroni ----

# Cuando se hace una prueba se define un alfa el cual es la probabilidad de cometer un error del tipo I

alfa <- 0.05 # Probabilidad de equivocación (tipo I) en un evenmto

1-alfa #Probabilidad de no equivocarse en un evento

(1-alfa)^5 #Probabilidad de no equivocarse ninguna vez en cinco eventos

1-(1-alfa)^5 #Probabilidad de equivocarse al menos una vez en cinco eventos

# Bonferroni intenta corregir ese aumento en la probabilidad de equivocarse al menos una vez al ir aumentando los eventos (exponente) al igual que otros tests pareados
# Tiene un costo en el poder, va a aumentar el error del tipo II

# Bonferroni ajusta el valor de p para poder disminuir el alza de errores del tipo I

# pairwise.t.test(): función que nos permite trabajar con test pareados
#* x: vector de los valores respuestas
#* g: vector de variable agrupamiento para x
#* p.adjust.method: método a utilizar para ajustar el valor de p
 #* 'none', 'bonferroni', etc.

# Para realizar un test pareado debemos partir con un ANOVA o con algún otro test que indique si hay o no diferencias

summary(aov(Sepal.Width ~ Species,data = iris))

 # Sí existen diferencias significativas por lo que podemos proceder con los tests pareados
 # No nos dice entre cuales hay diferencias y es en esto que podemos emplear pairwise.t.teest()

#Comparaciones pareadas

pairwise.t.test(x = iris$Sepal.Width,
                g = iris$Species,
                p.adjust.method = 'none')
 # Tres comparaciones con p-values

 # Test conservador: presentan p-values más altos (H0 se rechaza con p-values >0.05)
  # Bonferroni es el test más conservador, es decir, es más dificil que una prueba salga significativa

# Test de Tukey ----

anova1 <- aov(Sepal.Length ~ Species,data = iris)

TukeyHSD(anova1)

# ANOVA anidado ----

# Tenemos un factor jerárquicamente dentro de otro
# Individuos dentro de una especie
# Hojas dentro de un árbol
# En anova si el factor B esta anidado dentro de A tenemos A/B
# Ejemplo individuos en la base de datos CO2

ANOVAUptake <- aov(data = CO2, 
                   uptake ~ Treatment + Type/Plant)

broom::tidy(ANOVAUptake)
