# 1 -----------------------------------------------------------------------
library(tidyverse)

df <- tibble(
  x = runif(1000, -1, 1),
  y = runif(1000, -1, 1)
)

# 2 -----------------------------------------------------------------------
ggplot(df) +
  geom_point(aes(x, y))


# 3 -----------------------------------------------------------------------
df <- df |>
  mutate(r = x^2 + y^2)

df

# 4 -----------------------------------------------------------------------
df <- df |>
  mutate(r2 = if_else(r > 1, "A", "B"))

df

# 5 -----------------------------------------------------------------------
ggplot(df) +
  geom_point(aes(x, y, color = r2))

ggplot(df) +
  geom_point(aes(x, y, color = r)) +
  scale_color_viridis_c()

# 6 -----------------------------------------------------------------------
dresumida <- df |>
  group_by(r2) |>
  summarise(
    conteo = n()
  )|>
  mutate(p = conteo/sum(conteo)) 

dresumida |> 
  mutate(valor = 4 * p)


# 7 -----------------------------------------------------------------------
df <- df |> 
  mutate(es_b = if_else(r2 == "B", 1, 0)) 

df

df <- df |> 
  mutate(convergencia = cummean(es_b)) 

df

# 8 -----------------------------------------------------------------------
df <- df |> 
  mutate(fila = row_number()) 

df

ggplot(df) +
  geom_line(aes(fila, 4 * convergencia)) +
  geom_hline(yintercept = pi, color = "red")
