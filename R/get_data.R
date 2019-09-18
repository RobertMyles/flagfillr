# get data for continent flag datasets:
get_continent_data <- function(continent, res){
  if(res == "small"){
    data <- rnaturalearth::countries110 %>%
      st_as_sf() %>%
      select(name, iso = iso_a2, continent, geometry) %>%
      filter(continent == {{continent}}) %>%
      mutate(iso = tolower(iso))
  } else{
    data <- rnaturalearthhires::countries10 %>% st_as_sf() %>%
      select(name = NAME, iso = ISO_A2, continent = CONTINENT, geometry) %>%
      filter(continent == {{continent}}) %>%
      mutate(iso = tolower(iso))
  }
  data <- as.data.table(data)
  return(data)
}


# get data for state flag datasets:
get_states_data <- function(country){
  data <- rnaturalearthhires::states10 %>%
    st_as_sf() %>%
    select(country = admin, name, geometry) %>%
    mutate(country = tolower(country), name = tolower(name)) %>%
    filter(country == {{country}}, !is.na(name)) %>%
    mutate(name = stri_trans_general(name, "Latin-ASCII"),
           name = gsub(" ", "_", name)) %>%
    as.data.table()
  return(data)
}
