library(tidyverse)

data <- tibble(
  direccion = c(
    "Bdo OHiggins #4506, Santiago",
    "Avenida Providencia #123, Providencia",
    "Av. Siempre Viva #744",
    "Riverside número 228",
    "Riverside número 432, EstatieneComuna",
    "Calle Elm n° 1344"
    )
  )

data |> 
  separate(
    direccion,
    c("direccion", "comuna"),
    sep = ","
  ) |> 
  separate(
    direccion,
    c("calle", "numero"),
    sep = "#|número|numero|n°"
  ) |> 
  mutate(
    comuna = str_trim(comuna),
    numero = as.numeric(str_trim(numero)),
    calle  = str_trim(calle),
    tipo = str_extract(calle, "Avenida|avenida|Av.|Calle|Psje")
  )
