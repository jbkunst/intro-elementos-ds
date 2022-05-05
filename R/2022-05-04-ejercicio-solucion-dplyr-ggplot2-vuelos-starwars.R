# 1 -----------------------------------------------------------------------
library(tidyverse)
library(datos)

# Lo siguiente se hace con CTRL + SHIFT + R
# 2 -----------------------------------------------------------------------
glimpse(vuelos) # Datos de vuelos

vuelos |> 
  count(origen)

# de la tabla `vuelos` contamos el número de apariciones 
# de cada valor de la columna `destino`
vuelos |> 
  count(destino)

glimpse(aeropuertos)

# glimpse(aerolineas)


# P: ¿Cuales son los destinos más comunes?
# R: *contar* las apariciones/repetición de cada valor de la columna `destino`
vuelos |> 
  count(destino)

vuelos |> 
  group_by(destino) |> 
  summarise(n = n())

destinos_comunes <- vuelos |> 
  count(destino, sort = TRUE)

ggplot(destinos_comunes) +
  geom_col(aes(destino, n))

# geom_* agregar geometrias (agregar formas)
# coord_*
ggplot(destinos_comunes) +
  geom_col(aes(destino, n)) +
  # transformamos a un grafico de `barras` o `de filas`
  coord_flip()

# alguien 1, se quejó (no diré nombre) sobre que no sabe exactamente 
# que valor es
# alguien 2, mencionó que se podría agregar más 
ggplot(destinos_comunes, aes(destino, n)) +
  geom_col() +
  # transformamos a un grafico de `barras` o `de filas`
  # alguien 1
  geom_text(aes(label = n)) +
  # alguien 2
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  coord_flip()


# factores: es un elementos particular de R
# `forcats` es un anagrama de `factors`
# fct_*

destinos_comunes <- destinos_comunes |> 
  mutate(destino = fct_inorder(destino))

destinos_comunes

ggplot(destinos_comunes, aes(destino, n)) +
  geom_col() +
  coord_flip() 


destinos_comunes_top <- destinos_comunes |> 
  # como están ordenados
  filter(row_number() <= 15)

# paquete para formatear numeros (entre otras cosas)
library(scales)

comma(123123)

ggplot(destinos_comunes_top, aes(destino, n)) +
  geom_col(fill = "gray60") +
  geom_text(aes(label = comma(n)), vjust = -1, color = 'gray50', size = 3.5) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Destinos más comunes desde los aeropuestos de NYC",
    x = "Destino",
    y = "Cantidad de vuelos desde NYC",
    captions = "Datos del año 2003"
  ) 


# volver a la información geograficos
# queremos un grafico de burbuja geográfico
# necesitamos:
# - ubicacion (x, y)
# - variable para graficar tamanio
# - (en UN DATA FRAME, para usar geom_point)
destinos_comunes
View(aeropuertos)

# destinos_comunes

destinos_comunes <- left_join(
  destinos_comunes, 
  aeropuertos, 
  by = c("destino" = "codigo_aeropuerto")
  )


# cag/sticker
ggplot(destinos_comunes) +
  geom_point(aes(longitud, latitud, size = n), alpha = 0.6, color = "gray60") +
  scale_size_continuous(trans = "sqrt", range = c(1, 4))

usa <- map_data("state") |> 
  as_tibble()

ggplot(usa) +
  geom_line(aes(long, lat))

ggplot(usa) +
  geom_path(aes(long, lat))

ggplot(usa) +
  geom_path(aes(long, lat, group = group))

ggplot(usa) +
  geom_polygon(aes(long, lat, group = group), color = "darkred", fill = "yellow")



ggplot() +
  geom_point(
    data = destinos_comunes,
    aes(longitud, latitud, size = n),
    alpha = 0.6, 
    color = "gray60"
    ) +
  scale_size_continuous(trans = "sqrt", range = c(1, 4)) +
  geom_polygon(
    data = usa,
    aes(long, lat, group = group),
    color = "transparent",
    fill = "gray90"
    ) +
  theme_void()


ggplot() +
  geom_polygon(
    data = usa,
    aes(long, lat, group = group),
    color = "gray95",
    fill = "gray90"
  ) +
  geom_point(
    data = destinos_comunes,
    aes(longitud, latitud, size = n),
    alpha = 0.6, 
    color = "gray60"
  ) +
  scale_size_continuous(trans = "sqrt", range = c(1, 4)) +
  theme_void()


ggplot() +
  geom_polygon(
    data = usa,
    aes(long, lat, group = group),
    color = "gray95",
    fill = "gray90"
  ) +
  geom_point(
    data = destinos_comunes,
    aes(longitud, latitud, size = n, color = n),
    alpha = 0.6
  ) +
  scale_color_viridis_c(option = "A") +
  scale_size_continuous(trans = "sqrt", range = c(1, 4)) +
  theme_void()



# 3 -----------------------------------------------------------------------




