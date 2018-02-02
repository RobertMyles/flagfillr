#' @author Robert Myles McDonnell, \email{robertmylesmcdonnell@gmail.com}
#' @title Use flags as a fill argument in a ggplot2 map
#' @description flagfillr is a small set of convenience functions for
#' you to use flags as fills in a ggplot2 map.
#' @param continent (\code{character}). Data come from the rnaturalearth package.
#' @param resolution resolution of rnaturalearth data.
#' @param size size of png image.
#' @examples
#' \dontrun{
#' flag_fillr_continent( continent = "Africa", resolution = "small", size = 100)
#' }
#' @export
flag_fillr_continent <- function(continent = c("North America", "Asia", "Africa",
                                               "Europe", "South America",
                                               "Oceania"),
                                 Europe_no_Russia = TRUE,
                                 resolution = c("small", "large"),
                                 size = c("100", "250", "1000")
){
  continent <- match.arg(continent, choices = c("North America", "Asia", "Africa",
                                                "Europe", "South America",
                                                "Oceania"))
  # oceania looks like junk
  res <- match.arg(resolution, choices = c("small", "large"))
  data <- get_continent_data(continent, res)
  type <- "country"
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  flag_image <- png_readr(country = NULL, pixels, type)
  flag_filterz <- gsub("\\.png", "", names(flag_image))
  isos <- data$iso
  messager(res, pixels)
  flagz <- dplyr::data_frame(
    iso = flag_filterz,
    flag_image = flag_image
  ) %>%
    dplyr::filter(iso %in% isos)
  suppressMessages(data <- full_join(data, flagz) %>%
                     dplyr::filter(!is.na(iso)) %>%
                     st_as_sf())
  if(Europe_no_Russia == TRUE){
    data <- data %>% dplyr::filter(name != "Russia")
  }

  finalize(data)
}

# get data for continent flag datasets:
get_continent_data <- function(continent, res){
  if(res == "small"){
    data <- rnaturalearth::countries110 %>% sf::st_as_sf() %>%
      dplyr::select(name, iso = iso_a2, continent, geometry) %>%
      dplyr::filter(continent == UQ(continent)) %>%
      mutate(iso = tolower(iso))
  } else{
    data <- rnaturalearthhires::countries10 %>% sf::st_as_sf() %>%
      dplyr::select(name = NAME, iso = ISO_A2, continent = CONTINENT, geometry) %>%
      dplyr::filter(continent == UQ(continent)) %>%
      mutate(iso = tolower(iso))
  }
}
