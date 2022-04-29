library(tidyverse)

# como se crean dataframes/tablas en R

datos <- tibble(
  x = c(1, 2, 5),
  n = c("a", "b", "c"),
  z = c(2, 4, 8)
)

datos

ggplot(datos) +
  geom_point(aes(x, z))

# r de random
# unif unifome
runif(10)

hist(runif(10000, min = -1, max = 1))


d_unif <- tibble(
  x = runif(500, min = -1, max = 1),
  y = runif(500, min = -1, max = 1)
)

ggplot(d_unif) +
  geom_point(aes(x, y))

log(sin(exp(1)))

1 |> exp() |> sin() |> log()


d_unif |> 
  mutate(r = x + y) |> 
  arrange(r) |> 
  filter(x < 0.5) |> 
  select(r, y)
