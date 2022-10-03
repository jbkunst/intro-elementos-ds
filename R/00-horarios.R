library(tidyverse)
library(showtext)
library(patchwork)

theme_set(theme_minimal(base_size = 12))
font_add_google("IBM Plex Sans", "ibm")
showtext_auto()


# Archivo -> Compartir -> Publicar em la web
data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSzJekUcOTtAN_wbeQnIcYGEARv9ucfXIayk-vJc45iaMOBYwYlo8te4RBhCdCu0-zDuGOn2lMR5l5e/pub?gid=9120210&single=true&output=csv")

glimpse(data)

data

dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes")

fns_detectoras <- map(dias, function(d){
  
  partial(str_detect, pattern = d)
  
}) |> 
  set_names(dias)


dres <- data |> 
  select(starts_with("Seleccione")) |> 
  gather(modulo, dias) |> 
  mutate(modulo = str_extract(modulo, "[0-9]{1,2}:[0-9]{1,2} - [0-9]{1,2}:[0-9]{1,2}")) |> 
  mutate(across(dias, .fns = fns_detectoras, .names = "{.fn}")) |> 
  select(-dias) |> 
  gather(dia, indicador, -modulo) |> 
  mutate(indicador = replace_na(indicador, FALSE)) |> 
  mutate(across(c(modulo, dia), .fns = fct_inorder)) |> 
  group_by(modulo, dia) |> 
  summarise(votos = sum(indicador)) |> 
  ungroup()


pp <- ggplot(dres, aes(dia, fct_rev(modulo))) +
  geom_tile(aes(fill = votos)) +
  geom_text(aes(label = votos), color = "white") + 
  scale_fill_viridis_c(
    begin = 0.1, end = 0.95, 
    na.value = "transparent",
    breaks = scales::pretty_breaks(5),
    limits = c(0, 7)
  ) +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) + 
  theme(legend.key.height = unit(1.5, "cm"))

dres <- data |> 
  select(starts_with("Horario Zomm")) |> 
  gather(modulo, dias) |> 
  mutate(modulo = str_extract(modulo, "[0-9]{1,2}:[0-9]{1,2} - [0-9]{1,2}:[0-9]{1,2}")) |> 
  mutate(across(dias, .fns = fns_detectoras, .names = "{.fn}")) |> 
  select(-dias) |> 
  gather(dia, indicador, -modulo) |> 
  mutate(indicador = replace_na(indicador, FALSE)) |> 
  mutate(across(c(modulo, dia), .fns = fct_inorder)) |> 
  group_by(modulo, dia) |> 
  summarise(votos = sum(indicador)) |> 
  ungroup()


pz <- ggplot(dres, aes(dia, fct_rev(modulo))) +
  geom_tile(aes(fill = votos)) +
  geom_text(aes(label = votos), color = "white") + 
  scale_fill_viridis_c(
    begin = 0.1, end = 0.95, 
    na.value = "transparent",
    breaks = scales::pretty_breaks(5),
    limits = c(0, 7)
  ) +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) + 
  theme(legend.key.height = unit(1.5, "cm"))



(pp + labs(title = "Presencial")) + 
  (pz + labs(title = "Online")) +
  plot_layout(guides = "collect") 
