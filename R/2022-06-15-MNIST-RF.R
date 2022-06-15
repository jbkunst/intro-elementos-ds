# setup -------------------------------------------------------------------
library(tidyverse)


# data --------------------------------------------------------------------
data <- read_csv(unz("data/MNIST.zip", "train.csv"))
data


# AED ---------------------------------------------------------------------
dmin <- sample_n(data, 9)

dmin2 <- dmin %>% 
  mutate(id = row_number(), .before = 1) %>% 
  gather(pixel, intensidad, -label, -id) %>% 
  mutate(pixel = str_remove(pixel, "pixel")) %>% 
  mutate(pixel = as.numeric(pixel)) %>% 
  arrange(label, pixel) %>% 
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

mod_ilu <- ctree(
  # label ~ ., 
  factor(label) ~ ., 
  data = data, 
  control = ctree_control(maxdepth = 4)
  ) #  :O

plot(mod_ilu)

plot(mod_ilu, gp = gpar(fontsize = 5)) 

data_test <- data_test %>%
  mutate(mod_ilu_pred = predict(mod_ilu, newdata = data_test))




# random forest -----------------------------------------------------------
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