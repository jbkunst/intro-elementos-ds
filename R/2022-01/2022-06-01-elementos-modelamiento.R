# setup -------------------------------------------------------------------
library(klassets) # remotes::install_github("jbkunst/klassets")
library(tidyverse)

# ejemplo de libro --------------------------------------------------------
df <- klassets::sim_xy(
  n = 500,
  x_dist = purrr::partial(runif, min = 0, max = 100),
  error_dist = purrr::partial(rnorm, sd = 5)
)

plot(df)

# generar un modelo
# y = beta0 + beta1 * y
df_modelos <- tribble(
  ~intercepto, ~pendiente, ~nombre,
  0,   1.2, "jbk",
  2, 0.6, "CA"
)

df_modelos

plot(df) +
  geom_abline(
    data = df_modelos,
    aes(slope  = pendiente, intercept = intercepto,  color = nombre),
    size = 1.2
  )

# ver el ajuste del modelo en los datos (en los cuales se modeló)
# obtener la estimacion del valor y utlizando modelo(x)
df

df <- df |> 
  mutate(
    y_jbk = 0 + 1.2 * x,
    y_CA  = 2 + 0.6 * x
  )

df

# comparar el valor REAL con el valor estimado por el modelo
# (aca tenemos 2 modelos)
df <- df |> 
  mutate(
    error_jbk = y - y_jbk,
    error_CA = y - y_CA
  )

df

# Tengo muuuuchos errores (residuos)
# Las idea es resumirlos, para tener un indicador fácil de comoar
df |> 
  summarise(
    ecm_jbk = mean(error_jbk^2),
    ecm_CA = mean(error_CA^2)
  ) |> 
  mutate(ecm_jbk/ecm_CA)


# Existe uno mejor?
# 
# min sum (y_i - y_mod_i)^2
# 
# min sum (y_i - (beta0 + beta1 * x_i))^2 = f(beta0, beta1)
mod <- lm(y ~ x, data = df)

df_modelos <- df_modelos |> 
  add_row(intercepto = 3.173,
          pendiente = 0.5015, 
          nombre = "lm" )

df_modelos

plot(df) +
  geom_abline(
    data = df_modelos,
    aes(slope  = pendiente, intercept = intercepto,  color = nombre),
    size = 1.2
  )

df |> 
  mutate(y_lm = predict(mod))

