band_members
band_instruments
band_instruments2

# band_members y band_instruments
left_join(
  band_members,
  band_instruments, 
  by = "name"
)

# band_members y band_instruments2
band_members
band_instruments2

left_join(
  band_members,
  band_instruments2,
  by = c("name" = "artist")
)

