# packages ----------------------------------------------------------------
library(tidyverse)
library(spotifyr)


# data --------------------------------------------------------------------
# http://meetingwords.com/GV7CCr7oKR

data <- "usuario playlist
nkmeln0t0go2m2vc4gx5ldr27 duchita
silvasaavedrai2013 80's
luke.rutherford.1999 TWICE/EG/RV/Anime
4a1v71wm05ynhpbytzk33mf7j Shi 
Astorgato AA
jbkunst SocialNetworks
9ijft8lahgcenze5pyol0dqgv motivación"

data

data <- read_delim(data)
data <- mutate(data, playlist = str_trim(playlist))
data

Sys.setenv(SPOTIFY_CLIENT_ID = '5e97191efa69427f9c7f59a0d8a2bebf')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '0a216fdb384b4a3a84f5d0857627a0b2')

access_token <- get_spotify_access_token()

get_spotify_authorization_code(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
  scope = scopes()
)


# test --------------------------------------------------------------------
get_user_playlists(user_id = "Astorgato") %>% glimpse()
get_user_playlists(user_id = "nkmeln0t0go2m2vc4gx5ldr27")
get_user_playlists(user_id = "9ijft8lahgcenze5pyol0dqgv") %>% glimpse()


playlist_id <- get_user_playlists(user_id = "nkmeln0t0go2m2vc4gx5ldr27") |>
  glimpse() |> 
  filter(name == "duchita") |> 
  glimpse() |> 
  pull(id)

playlist_id


# pl <- get_playlist(playlist_id = playlist_id)
# pl_tracks <- as_tibble(pl$tracks$items)
# pl_tracks
# glimpse(pl_tracks)

# same
pl_tracks <- get_playlist_tracks(playlist_id)

glimpse(pl_tracks)

pl_tracks$track.id 

pl_tracks$track.album.release_date

features <- get_track_audio_features(pl_tracks$track.id)

# https://developer.spotify.com/documentation/web-api/reference/#/operations/get-several-audio-features

glimpse(features)


# iterar con map ----------------------------------------------------------
x <- c(1, 4, 56, 99)

exp(x)

for(i in 1:4){
  print(exp(x[i]))
}

map(x, exp)

datos <- pmap(data, function(usuario = "Astorgato", playlist = "AA"){
  
  message("voy a descargar ", usuario)
  
  playlist_id <- get_user_playlists(user_id = usuario) |>
    filter(name == playlist) |> 
    pull(id)
  
  pl_tracks <- get_playlist_tracks(playlist_id)
  
  dim(pl_tracks)
  
  features <- get_track_audio_features(pl_tracks$track.id)
  
  d <- features %>% 
    mutate(usuario = usuario, .before = 1) %>% 
    mutate(
      anio = as.numeric(str_extract(pl_tracks$track.album.release_date, "[0-9]{4}")),
      .after = 1
      )
  
  d
  
})


datos <- bind_rows(datos)

count(datos, usuario)


datos %>% 
  select(where(is.numeric))


# umap --------------------------------------------------------------------
datosnum <- datos %>% 
  select(where(is.numeric))

library(umap)

dumap <- umap(datosnum)

dumapdf <- dumap$layout %>% 
  as.data.frame()

datos <- bind_cols(datos, dumapdf)

glimpse(datos)

datos %>% count(usuario)

ggplot(datos) +
  geom_point(aes(V1, V2, color = usuario)) +
  scale_color_viridis_d()


ggplot(datos) +
  geom_point(aes(V1, V2), color = "gray90", data = dumapdf) +
  geom_point(aes(V1, V2, color = tempo)) +
  facet_wrap(vars(usuario)) +
  scale_color_viridis_c() +
  theme_minimal()


