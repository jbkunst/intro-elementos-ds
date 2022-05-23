# Solucion al ejercicio
# https://jkunst.com/usach-ingemat-intro-elementos-ds-202201/slides/02-AED-Viz-ggplot2.html#15

# 1. Cargue los paquetes datos, ggplot2 y dplyr.
library(datos)
library(ggplot2)
library(dplyr)

# 2. Ejecute glimpse(vuelos).
glimpse(vuelos)

# 3. Objtenga una muestra de 10.000 registros para responder las preguntas utilizando la funcion sample_n.
vuelos_min <- sample_n(vuelos, 10000)

# 4. ¿Cuántos filas/columnas tienen los datos?
dim(vuelos_min)

# 5. ¿Cuántos datos son numéricos?
vuelos_min |> 
  select(where(is.numeric)) |>
  ncol()



# 6. Explore la relación entre distancia y tiempo_vuelo.
p1 <- ggplot(vuelos_min) +
  geom_point(aes(distancia, tiempo_vuelo), alpha = 0.1)

p1

p2 <- ggplot(vuelos_min) +
  geom_density2d(aes(distancia, tiempo_vuelo))

p2

# 7. ¿Qué otras preguntas tienes? ¿Como podríamos obtener QUE vuelo es el más largos?
vuelos_min |> 
  filter(distancia > 4500) |> 
  count(origen, destino)



# 8. Reutiliza el código del ejemplo paso a paso para utilizar la función facet_wrap con estos datos.
ggplot(vuelos_min, aes(distancia, tiempo_vuelo)) +
  geom_point(alpha = 0.005) +
  geom_density_2d(alpha = 0.5) +
  facet_wrap(vars(mes))











