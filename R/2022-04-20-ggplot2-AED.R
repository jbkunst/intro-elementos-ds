library(datos)
library(ggplot2)
library(dplyr)

glimpse(vuelos)

# ncol(vuelos)
# dim(vuelos)
# nrow(vuelos)
# count(vuelos)

# Con la función sample obtendremos una muestra aleatoria
# asi será más rápido en responder ggplot2
vuelos_min <- sample_n(vuelos, 10000)

ggplot(vuelos_min) +
  geom_point(aes(distancia, tiempo_vuelo), alpha = 0.1)

ggplot(vuelos_min) +
  geom_density2d(aes(distancia, tiempo_vuelo))


# algo más llamativo
ggplot(vuelos_min, aes(distancia, tiempo_vuelo)) +
  geom_point(alpha = 0.005) +
  geom_density_2d_filled(alpha = 0.5)



# Podemos ver la relación/tendencia con un suavizamiento
ggplot(vuelos_min) +
  geom_point(aes(distancia, tiempo_vuelo), alpha = 0.1) +
  geom_smooth(aes(distancia, tiempo_vuelo))

ggplot(vuelos_min, aes(distancia, tiempo_vuelo)) +
  geom_point(alpha = 0.1, color = "darkred") +
  geom_smooth(color = "gray60")

# Se observa heterocedasticidad.
# Esto es que la varianza no es constante.












