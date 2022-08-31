library(datos)
library(tidyverse)
library(celavi)

mtautos
glimpse(mtautos)

# 1. Ajustar disintos modelos predictivos a los datos
#    Intentando predecir la variable millas (millas por galon, redimiento)
#    em funcion del resto de los datos.

# 2. En general los modelos se ajustan de la siguiente forma
# nombre_funcion(formula, data = datos, parametros adicionales)

# millas ~ ., es lo mismo que deicr millas en funcion del resto de variables

# arbol -------------------------------------------------------------------
library(partykit)

mod_arbol <- ctree(millas ~ ., data = mtautos)

plot(mod_arbol)

# random forest -----------------------------------------------------------
library(randomForest)
# library(ranger)

mod_randomforest <- randomForest(millas ~ ., data = mtautos, ntree = 200)

plot(mod_randomforest)

# nnet --------------------------------------------------------------------
library(nnet)

mod_redneuronal <-nnet(millas ~ ., data = mtautos, size = 10, linout = TRUE, skip = TRUE, MaxNWts = 10000, maxit = 1000)

plot(mod_redneuronal)

# modelo lineal -----------------------------------------------------------
mod_reglin <- lm(millas ~ ., data = mtautos)

summary(mod_reglin)

mod_reglin <- step(mod_reglin)

summary(mod_reglin)


# ver que modelo es mejor en terminos de predicitibilidad -----------------
# 
# En el caso de hace predicciones, la sintaxis
# predict(modelo, newdata = datos, tipo_prediccion)
# 
mtautos_min <- head(mtautos, 5)

glimpse(mtautos_min)

predict(mod_arbol, newdata = mtautos_min)

predict(mod_randomforest, newdata = mtautos_min)

predict(mod_redneuronal, newdata = mtautos_min)

predict(mod_reglin, newdata = mtautos_min)

mtautos <- mtautos |> 
  mutate(
    pred_arbol = predict(mod_arbol, newdata = mtautos),
    pred_randomforest = predict(mod_randomforest, newdata = mtautos),
    pred_redneuronal = predict(mod_redneuronal, newdata = mtautos),
    pred_reglin = predict(mod_reglin, newdata = mtautos)
  )

glimpse(mtautos)

# en general en tareas de regresion la métrica más usada es
# RMSE: Root mean square error
# Error cuadrático medio

mtautos |> 
  mutate(error_arbol = millas - pred_arbol) |> 
  summarise(
    rmse_arbol = sqrt(mean(error_arbol^2))
  )

mtautos |> 
  mutate(
    error_arbol = millas - pred_arbol,
    error_randomforest = millas - pred_randomforest,
    error_redneuronal = millas - pred_redneuronal,
    error_reglin = millas - pred_reglin
    ) |> 
  summarise(
    rmse_arbol = sqrt(mean(error_arbol^2)),
    rmse_randomforest = sqrt(mean(error_randomforest^2)),
    rmse_redneuronal = sqrt(mean(error_redneuronal^2)),
    rmse_reglin = sqrt(mean(error_reglin^2))
  )

mtautos |> 
  select(millas, contains("pred")) |> 
  pivot_longer(cols = -millas) |> 
  mutate(error = millas - value) |> 
  group_by(name) |> 
  summarise(error = sqrt(mean(error^2)))


# comparacion modelos -----------------------------------------------------
plot(mod_arbol)

summary(mod_reglin)

mod_randomforest

# importancia de variable
# importancia en funcion de la permutacion de variables
# 
# el calculo, o los resultados obtenidos son comparables entre modelos
# 

vi_rl <- variable_importance(
  mod_reglin, 
  data = mtautos,
  variables = c("peso", "velocidad", "transmision"),
  iterations = 10
  )

plot(vi_rl) +
  scale_y_continuous(name = "RMSE", limits = c(0, NA))


vi_nnet <-  variable_importance(
  mod_redneuronal, 
  data = mtautos,
  iterations = 10
)

plot(vi_nnet)

vi_arbol <- variable_importance(
  mod_arbol, 
  data = mtautos,
  iterations = 10,
  predict_function = partykit::predict.party
)

plot(vi_arbol) +
  scale_y_continuous(name = "RMSE", limits = c(0, NA))


vi_rf <- variable_importance(
  mod_randomforest, 
  data = mtautos,
  iterations = 10,
  variables = names(mtautos_min)
)


plot(vi_rf) +
  scale_y_continuous(name = "RMSE", limits = c(0, NA))


plot(vi_rl, vi_arbol, vi_rf, vi_nnet) +
  scale_y_continuous(name = "RMSE", limits = c(0, NA))

mtautos  |> select(1:6) |> head(5)







