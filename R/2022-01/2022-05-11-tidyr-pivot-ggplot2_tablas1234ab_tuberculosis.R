library(datos)
library(tidyverse)
library(scales)

# tabla1 ------------------------------------------------------------------
tabla1 <- tabla1 |> 
  mutate(
    tasa = casos/poblacion,
    tasa_fmt = percent(tasa, accuracy = 0.001)
    )

tabla1

# tabla2 ------------------------------------------------------------------
tabla2 |> 
  pivot_wider(
    names_from = tipo,
    values_from = cuenta
  ) |> 
  mutate(
    tasa = casos/población,
    tasa_fmt = percent(tasa, accuracy = 0.001)
  )

# tabla3 ------------------------------------------------------------------
tabla3 |> 
  separate(
    tasa,
    c("casos", "población"),
    sep = "/",
    convert = TRUE
  ) |> 
  mutate(
    tasa = casos/población,
    tasa_fmt = percent(tasa, accuracy = 0.001)
  )

# tablas4 -----------------------------------------------------------------
# casos
tabla4am <- tabla4a |> 
  pivot_longer(
    cols = c(`1999`, `2000`),
    names_to = "anio",
    values_to = "casos"
  )

tabla4am

tabla4bm <- tabla4b |> 
  pivot_longer(
    cols = c(`1999`, `2000`),
    names_to = "anio",
    values_to = "población"
  )

tabla4am
tabla4bm

tabla4 <- left_join(
  tabla4am,
  tabla4bm,
  by = c("pais", "anio")
)

tabla4 |> 
  mutate(
    tasa = casos/población,
    tasa_fmt = percent(tasa, accuracy = 0.001)
  )


# grafico -----------------------------------------------------------------
tabla1

library(ggrepel)

ggplot(tabla1, aes(x = anio, y = tasa, color = pais)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  geom_label_repel(aes(label = tasa_fmt), size = 4, force = 20) +
  scale_x_continuous(breaks = c(1999, 2000)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
  theme_minimal()













