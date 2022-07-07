# packages ----------------------------------------------------------------
library(tidyverse)
library(spotifyr)


# data --------------------------------------------------------------------
# http://meetingwords.com/GV7CCr7oKR

data <- "usuario playlist
jbkunst Top
jbkunst SocialNetworks
lrdsmella duchita
ISS® Sunshine ♥
electronmaxi TWICE/EG/RV/Anime
Taliric Shi 
Astorgato Uwu"

data

data <- read_delim(data)

data

Sys.setenv(SPOTIFY_CLIENT_ID = '5e97191efa69427f9c7f59a0d8a2bebf')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '0a216fdb384b4a3a84f5d0857627a0b2')

access_token <- get_spotify_access_token()


# test --------------------------------------------------------------------
playlist_id <- get_user_playlists(user_id = "jbkunst") |>
  glimpse() |> 
  filter(name == "SocialNetworks") |> 
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




