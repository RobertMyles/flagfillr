#' @export
country_list <- function(){
  rnaturalearth::countries110 %>% st_as_sf() %>% pull(name) %>% unique() %>% sort()
}
#' @export
country_list_states <- function(){
  rnaturalearthhires::states10 %>% st_as_sf() %>% pull(country) %>% unique() %>% sort()
}
# data for countries:
get_country_data <- function(country, res){
  if(res == "small"){
    data <- rnaturalearth::countries110
  } else{
    data <- rnaturalearth::countries10
  }
  data <- data %>% sf::st_as_sf() %>%
    dplyr::select(name, iso = iso_a2, geometry) %>%
    mutate(name = tolower(name)) %>%
    dplyr::filter(name == UQ(country)) %>%
    mutate(iso = tolower(iso))
}
# get data for continent flag datasets:
get_continent_data <- function(continent, res){
  if(res == "small"){
    data <- rnaturalearth::countries110
  } else{
    data <- rnaturalearth::countries10
  }
   data <- data %>% sf::st_as_sf() %>%
    dplyr::select(name, iso = iso_a2, continent, geometry) %>%
    dplyr::filter(continent == UQ(continent)) %>%
    mutate(iso = tolower(iso))
}

# get data for state flag datasets:
get_states_data <- function(country){
  data <- rnaturalearthhires::states10 %>% sf::st_as_sf() %>%
    dplyr::select(country = admin, name, iso = iso_a2, geometry) %>%
    dplyr::mutate(country = tolower(country),
                  name = tolower(name)) %>%
    dplyr::filter(country == UQ(country), !is.na(name)) %>%
    dplyr::mutate(name = stringi::stri_trans_general(name, "Latin-ASCII"),
                  name = gsub(" ", "_", name))
}

# filter flags, check they match up with data iso codes:
flag_filter <- function(flags_dir, data){
  flags_dir %>% gsub("\\.png", '', .) %>%
    .[which(. %in% data$iso)] %>%
    as_data_frame() %>%
    rename(iso = value)
}

# read PNGS
png_readr <- function(data, type, pixels){
  flags <- paste0(data$iso, ".png")
  if(type == "country"){
    flags <- paste0(type, "-flags/png", pixels, "px/", flags)
  } else{
    flags <- paste0(type, "-flags/", flags)
  }
  for(i in 1:nrow(data)){
    data$flag_image[[i]] <- readPNG(source = flags[[i]])
  }
  return(data)
}

finalize <- function(data){
  output <- flag_fillr(data)
  message("Combining flags and data...\n")
  message("Creating plot...")
  flag_plotr(output)
}

messager <- function(res, pixels){
  if(res == "large"){
    if(pixels == "1000"){
      message("Using large-scale data with high res. This may take a while...")
    }
  }
}

process <- function(data, size, res, flags_dir, type){
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  flags_dir <- dir(paste0(type, "-flags/png", pixels, "px/"))
  messager(res, pixels)
  country_list <- flag_filter(flags_dir, data)
  data <- suppressMessages(
    left_join(data, country_list) %>%
      mutate(flag_image = list(array(NA, c(1, 1, 3))))
  )
  data <- png_readr(data, type = type, pixels)
  return(data)
}
