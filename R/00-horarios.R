library(tidyverse)

# texto comentado
# library(showtext)
# showtext_auto()
# font_add_google("IBM Plex Sans")

data <- read_csv("https://raw.githubusercontent.com/jbkunst/usach-ingemat-intro-elementos-ds-202201/main/data/202201%20-%20Intro%20DS.csv")

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


ggplot(dres, aes(dia, fct_rev(modulo))) +
  geom_tile(aes(fill = votos)) +
  geom_text(aes(label = votos), color = "white") + 
  scale_fill_viridis_c(
    limits = c(1, NA), begin = 0.1, end = 0.95, 
    na.value = "transparent",
    breaks = scales::pretty_breaks(5)
    ) +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) + 
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
  
