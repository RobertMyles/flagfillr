#' @title list of countries for flagfillr functions
#' @export
country_list <- function(){
  rnaturalearth::countries110 %>% st_as_sf() %>% pull(name) %>% unique() %>% sort()
}
#' @title list of countries for flag_fillr_state() function
#' @export
country_list_states <- function(){
  rnaturalearthhires::states10 %>% st_as_sf() %>% pull(country) %>% unique() %>% sort()
}


flag_filter <- function(flags_dir, data){
  flags_dir %>% gsub("\\.png", '', .) %>%
    .[which(. %in% data$iso)] %>%
    as_data_frame() %>%
    rename(iso = value)
}

png_readr <- function(data, country, type, pixels){

  if(type == "country"){
    flags <- paste0(data$iso, ".png")
    flags <- paste0(type, "-flags/png", pixels, "px/", flags)
  } else{
    country <- gsub(" ", "-", country)
    flags <- paste0(data$name, ".png")
    flags <- paste0(type, "-flags/", country, "-flags/", flags)
  }
  for(i in 1:nrow(data)){
    data$flag_image[[i]] <- readPNG(source = flags[[i]])
  }
  return(data)
}

messager <- function(res, pixels){
  if(res == "large"){
    if(pixels == "1000"){
      message("Using large-scale data with high res. This may take a while...")
    }
  }
}

process <- function(data, size, res, flags_dir, type, country){
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  flags_dir <- dir(paste0(type, "-flags/png", pixels, "px/"))
  messager(res, pixels)
  country_list <- flag_filter(flags_dir, data)
  data <- suppressMessages(
    left_join(data, country_list) %>%
      mutate(flag_image = list(array(NA, c(1, 1, 3))))
  )
  data <- png_readr(data, country, type = type, pixels)
  return(data)
}

finalize <- function(data){
  output <- flag_fillr(data)
  message("Combining flags and data...\n")
  message("Creating plot...")
  flag_plotr(output)
}

## goddamn you global variables check
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "geometry", ".id", "ADMIN", "CONTINENT", "ISO_A2",
    "L2", "NAME", "X", "Y", "add_row", "admin", "color", "country",
    "flags_dir", "iso", "iso_a2", 'mutate_all', "mutate_at", "name", "value"))
