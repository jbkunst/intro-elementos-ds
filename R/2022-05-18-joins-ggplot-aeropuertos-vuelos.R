# Vuelva a los datos de vuelos y aeropuertos
# y grafique en el mapa los segmentos que
# corresponden a los viajes entre un areopuerto y el otro.
library(tidyverse)
library(datos)


glimpse(vuelos)

aeropuertos


# simplificar vuelos ------------------------------------------------------
vuelos2 <- vuelos |> 
  select(origen, destino)

vuelos2

# simplificar aeropuertos -------------------------------------------------
aeropuertos2 <- aeropuertos |> 
  select(codigo_aeropuerto, latitud, longitud)

aeropuertos2


# resumir vuelos contando los pares ---------------------------------------
vuelos2 |> 
  group_by(origen, destino) |> 
  summarise(cantidad = n())

vuelos_od <- vuelos2 |> 
  count(origen, destino, sort = TRUE) 

vuelos_od




# graficar ----------------------------------------------------------------
aeropuertos2

ggplot(aeropuertos2) +
  geom_point(aes(x = longitud, y = latitud))


# para graficar segmentos, necesitamos 4 campos
# coordenadas de origen y coordenadas de destino
vuelos_od

aeropuertos2

vuelos_od <- left_join(
  vuelos_od, 
  aeropuertos2, 
  by = c("origen" = "codigo_aeropuerto")
  )

# estudiemos la operación que necesitamos
# para pegar las coordenadas que nos faltan
vuelos_od <- left_join(
  vuelos_od, 
  aeropuertos2, 
  by = c("destino" = "codigo_aeropuerto"),
  suffix = c("_origen", "_destino")
) 

glimpse(vuelos_od)



ggplot(vuelos_od) +
  geom_segment(
    aes(x = longitud_origen, y = latitud_origen,
        xend = longitud_destino, yend = latitud_destino )
    ) 

ggplot(vuelos_od) +
  geom_segment(
    aes(x = longitud_origen, y = latitud_origen,
        xend = longitud_destino, yend = latitud_destino,
        alpha = n,
        color = n
        )
  ) 

ggplot(aeropuertos2) +
  geom_point(aes(x = longitud, y = latitud))

# como juntamos los graficos cuando vienen desde
# dos tablas o más

usa <- map_data("state") |> 
  as_tibble()

ggplot() +
  geom_polygon(
    aes(long, lat, group = group),
    data = usa,
    color = "gray80",
    fill = "gray99"
    ) +
  # aeropuertos
  geom_point(
    aes(x = longitud, y = latitud),
    shape = 21,
    color = "gray60",
    fill = "gray90",
    alpha = 0.5,
    size = 1,
    data = aeropuertos2
    ) +
  geom_segment(
    aes(x = longitud_origen, y = latitud_origen,
        xend = longitud_destino, yend = latitud_destino,
        alpha = n,
        color = n
        ),
    data = vuelos_od
  ) +
  scale_color_viridis_c(guide = "none") +
  scale_alpha(guide = "none") +
  xlim(-135, -60) +
  ylim(20, 50) +
  theme_void() +
  coord_map() 


ggplot() +
  geom_polygon(
    aes(long, lat, group = group),
    data = usa,
    color = "gray80",
    fill = "gray99"
  ) +
  # aeropuertos
  geom_point(
    aes(x = longitud, y = latitud),
    shape = 21,
    color = "gray60",
    fill = "gray90",
    alpha = 0.5,
    size = 1,
    data = aeropuertos2
  ) +
  geom_segment(
    aes(x = longitud_origen, y = latitud_origen,
        xend = longitud_destino, yend = latitud_destino,
        alpha = n,
        color = n
    ),
    # curvature = -0.2,
    data = vuelos_od
  ) +
  scale_color_viridis_c(guide = "none") +
  scale_alpha(guide = "none") +
  xlim(-135, -60) +
  ylim(20, 50) +
  theme_void() 

cbind(c(rep(0,6),rep(1,6)),c(1:6,1:6))



















