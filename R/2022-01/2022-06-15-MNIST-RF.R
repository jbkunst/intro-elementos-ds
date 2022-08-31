# setup -------------------------------------------------------------------
library(tidyverse)


# data --------------------------------------------------------------------
data <- read_csv(unz("data/MNIST.zip", "train.csv"))
data


 # AED ---------------------------------------------------------------------
dmin <- data |> 
  # filter(label %in% c(1, 7)) |> 
  sample_n(16)

dmin2 <- dmin %>% 
  mutate(id = row_number(), .before = 1) %>% 
  gather(pixel, intensidad, -label, -id) %>% 
  mutate(pixel = str_remove(pixel, "pixel")) %>% 
  mutate(pixel = as.numeric(pixel)) %>% 
  arrange(id, label, pixel) %>% 
  group_by(id) %>% 
  mutate(
    x = rep(1:28, times = 28),
    y = rep(28:1, each = 28)
  )

ggplot(dmin2) +
  geom_tile(aes(x, y, fill = intensidad)) +
  facet_wrap(vars(id, label)) 

ggplot(dmin2) +
  geom_tile(aes(x, y, fill = intensidad)) +
  facet_wrap(vars(id, label))  +
  scale_fill_gradient(low = "white", high = "black") +
  theme_minimal()


# arbol iluso -------------------------------------------------------------
library(partykit)

data

mod_ilu <- ctree(
  # label ~ ., 
  factor(label) ~ ., 
  data = data, 
  control = ctree_control(maxdepth = 4)
  ) #  :O

plot(mod_ilu)


plot(ctree(factor(label) ~ ., data = data, control = ctree_control(maxdepth = 1)))

plot(ctree(factor(label) ~ ., data = data, control = ctree_control(maxdepth = 2))) 



plot(mod_ilu, gp = gpar(fontsize = 4)) 

pixeles_importantes <- c(350, 597, 489, 543, 708, 486, 347)

mod_ilu



dpiximp <- tibble(
  x = rep(1:28, times = 28),
  y = rep(28:1, each = 28)
) |> 
  mutate(pixel = row_number() - 1) |> 
  mutate(es_importante = pixel %in% pixeles_importantes) |> 
  filter(es_importante)

dpiximp

ggplot(dmin2) +
  geom_tile(aes(x, y, fill = intensidad)) +
  facet_wrap(vars(id, label))  +
  scale_fill_gradient(low = "white", high = "gray70") +
  theme_minimal() +
  geom_tile(
    aes(x, y), color = "darkred", 
    fill = "transparent",
    data = dpiximp
    )

# 

data |> 
  mutate(pred_arbol = predict(mod_ilu, newdata = data), .before = 2) |> 
  count(label, pred_arbol) |> 
  spread(pred_arbol, n)

dcond <- data |> 
  mutate(pred_arbol = predict(mod_ilu, newdata = data), .before = 2) |> 
  count(label, pred_arbol) |> 
  mutate(label = factor(label)) 

dcond <- dcond |> 
  complete(label, pred_arbol)

dcond |> 
  spread(pred_arbol, n)
  
dcond <- dcond |> 
  mutate(p = n/sum(n, na.rm = TRUE))


sum(c(6, 7, 9, NA))

sum(c(6, 7, 9, NA), na.rm = TRUE)

ggplot(dcond) +
  geom_tile(aes(x = pred_arbol, y = label, fill = n)) +
  # scale_fill_gradient(low = "transparent", high = "darkred") +
  scale_fill_viridis_c() +
  theme_minimal()

# que porcentaje de casos estuvieron bien clasificados
dcond |> 
  filter(label == pred_arbol) |> 
  summarise(sum(p, na.rm = TRUE))

# bkn version
dcond |> 
  group_by(label == pred_arbol) |> 
  summarise(sum(p, na.rm = TRUE))

# RL abarca otro tipo de problema
# RL: tarea predicción
# Arbol: tarea clasificacion, tarea prediccion


plot(
  ctree(factor(label) ~ ., 
        data = head(data, 100),
        control = ctree_control(maxdepth = 1))
  )

# random forest -----------------------------------------------------------

# n X p: n observaciones, p predictore 
# en terminos de columnas sqrt(p)
# en terminos de observaciones, es una muestra boostrap, muestra con reemplazamiento
# c(1, 1, 2, 2, 2, 9)
sample(1:10) |> table()
sample(1:10, replace = TRUE) |> table()


library(ranger)

mod <- ranger(label ~ .,
              data = data,
              num.trees = 500,
              verbose = TRUE, 
              num.threads = 3, 
              importance = "impurity"
              )
mod


predict(mod, data = head(dtest, 6)) 
predict(mod, data = head(dtest, 6))$predictions

dtest <- dtest %>% 
  mutate(pred = predict(mod, data = dtest)$predictions)

dtest <- dtest %>% 
  select(pred, label, everything())

dtest %>% select(1:5)

dmet <- dtest %>% 
  count(label, pred) %>% 
  group_by(label) %>% 
  mutate(p = n/sum(n)) %>% 
  select(label, pred, p)

dmet %>% 
  spread(pred, p)

ggplot(dmet) +
  geom_tile(aes(as.factor(label), pred, fill = p))

dmet %>% 
  filter(label != pred) %>% 
  ggplot() +
  geom_tile(aes(as.factor(label), pred, fill = p)) +
  scale_fill_viridis_c(option = "B")

imp <- importance(mod)

dfimp <- data_frame(
  pixel = names(imp),
  imp = as.numeric(imp),
  x = rep(1:28, times = 28),
  y = rep(28:1, each = 28)
)

ggplot(dfimp) +
  geom_tile(aes(x, y, fill = imp)) +
  scale_fill_viridis_c(option = "B") +
  theme_void() +
  theme(legend.position = "none")