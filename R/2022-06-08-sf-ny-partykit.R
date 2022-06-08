library(tidyverse) # ? por que dplyr, ggplot2, readr

ruta <- "https://raw.githubusercontent.com/jadeyee/r2d3-part-1-data/master/part_1_data.csv"

data <- readr::read_csv(ruta, skip = 2)

data

# Predecir la ubicación de la propiedad/casa en función de 
# camas, baños, precio, antiguedad, pies cuadrado, elevación  
glimpse(data)

data |> 
  count(in_sf)

data <- data |> 
  mutate(in_sf = ifelse(in_sf == 1, "SF", "NY"))

# histograma --------------------------------------------------------------
ggplot(data) +
  geom_histogram(aes(elevation, fill = in_sf)) +
  facet_grid(vars(in_sf))



# puntos ------------------------------------------------------------------
ggplot(data) +
  geom_point(aes(x = price_per_sqft, y = elevation, color = in_sf))


ggplot(data) +
  geom_point(aes(x = price_per_sqft, y = elevation, color = in_sf)) +
  scale_x_continuous(labels =  ~scales::comma(.x, prefix = "$", suffix = "ft²"))

 
# arbol -------------------------------------------------------------------
# install.packages("partykit")
# 
library(partykit)


# no tiene que existist chr sino fct!
data <- data |> 
  mutate(in_sf = factor(in_sf))


# como se crea/ajuste/inventa/planta/etc un arbol
ctree(in_sf ~ ., data = data)

# En general "todos los modelos" requieren de estos 2 elementos
# Formula: Expresa que lo que uno desea modelar/predecir en funcion "~"
# de variables. "." significa todo el resto de las columnas de data

# "Todos los modelos": Lineales, Arboles, Random Forest, Gradient Boosting Models

# por tanto es lo mismo que:
ctree(
  in_sf ~ beds + bath + price + year_built + sqft + 
    price_per_sqft + elevation,
  data = data
  )


arbol <- ctree(in_sf ~ ., data = data)

arbol

plot(arbol)


# "Todos los modelos": poseen la función 
# 
# predict(modelo, newdata = nuevosdatos, type = "")
# modelo
# nuevos datos
# type: es el tipo de predicción

dnuevos <- data |> 
  select(-in_sf) |> 
  sample_n(3)

dnuevos

dnuevos2 <- tibble(
  beds = 10,
  baths = 5,
  price = 8995000, 
  year_built = 2010,
  sqft = 2000,
  price_per_sqft = price/sqft,
  elevation = 25
)

dnuevos2

predict(arbol, newdata = dnuevos)

dnuevos <- dnuevos |> 
  mutate(prediccion_in_sf = predict(arbol, newdata = dnuevos))

dnuevos

predict(arbol, newdata = dnuevos)

predict(arbol, newdata = dnuevos, type = "response")

predict(arbol, newdata = dnuevos, type = "prob")

predict(arbol, newdata = dnuevos, type = "prob")[, 2]

predict(arbol, newdata = dnuevos, type = "node")



dnuevos <- dnuevos |> 
  mutate(
    prediccion_in_sf = predict(arbol, newdata = dnuevos),
    prediccion_in_sf_nodo = predict(arbol, newdata = dnuevos, type = "node"),
    prediccion_in_sf_prob = predict(arbol, newdata = dnuevos, type = "prob")[, 2]
    )


dnuevos





