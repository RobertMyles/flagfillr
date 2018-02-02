#' @title list of countries for flagfillr functions
#' @description Returns a list of suitable countries.
#' @export
country_list <- function(){
  rnaturalearth::countries110 %>% st_as_sf() %>% pull(name) %>% unique() %>% sort()
}
#' @title list of countries for flag_fillr_state() function
#' @description Returns a list of suitable countries.
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



flag_loader <- function(pixels){

  if(pixels == "100") x <- get(data("flag_list_100px"))
  if(pixels == "250") x <- get(data("flag_list_250px"))
  return(x)
  #if(pixels == "1000") load("flag_list_1000px.rda")  too big :-(
}

flag_loader_states <- function(country){
  country <- gsub(" ", "-", country)
  if(country == "argentina"){
    x <- get(data("flag_list_argentina"))
  } else if(country == "australia"){
    x <- get(data("flag_list_australia"))
  } else if(country == "brazil"){
    x <- get(data("flag_list_brazil"))
  } else if(country == "canada"){
    x <- get(data("flag_list_canada"))
  } else if(country == "germany"){
    x <- get(data("flag_list_germany"))
  } else if(country == "netherlands"){
    x <- get(data("flag_list_netherlands"))
  } else if(country == "united-states-of-america"){
    x <- get(data("flag_list_united_states_of_america"))
  } else{
    stop("Lower-level flags not available for this country.")
  }
  return(x)
}

png_readr <- function(country, pixels, type){

  ## flags don't match up, need to do user_data as well
  if(type == "country"){
    x <- flag_loader(pixels)
  } else{
    x <- flag_loader_states(country)
  }
  return(x)
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
  data$flag_image <- png_readr(data, country, pixels)

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
