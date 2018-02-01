#' @export
country_list <- function(){
  rnaturalearth::countries110 %>% st_as_sf() %>% pull(name) %>% unique() %>% sort()
}
#' @export
country_list_states <- function(){
  rnaturalearthhires::states10 %>% st_as_sf() %>% pull(country) %>% unique() %>% sort()
}

# filter flags, check they match up with data iso codes:
flag_filter <- function(flags_dir, data){
  flags_dir %>% gsub("\\.png", '', .) %>%
    .[which(. %in% data$iso)] %>%
    as_data_frame() %>%
    rename(iso = value)
}

# read PNGS
png_readr <- function(data, country, type, pixels){
  
  if(type == "country"){
    flags <- paste0(data$iso, ".png")
    flags <- paste0(type, "-flags/png", pixels, "px/", flags)
  } else{
    flags <- paste0(data$name, ".png")
    flags <- paste0(type, "-flags/", country, "-flags/", flags)
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
