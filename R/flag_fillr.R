#' @import rnaturalearth
#' @import rnaturalearthhires
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom dplyr as_tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr pull
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom rlang UQ
#' @importFrom png readPNG
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr map2
#' @importFrom purrr map_lgl
#' @importFrom sf st_bbox
#' @importFrom sf st_coordinates
#' @importFrom sf st_as_sf
#' @importFrom sf st_join
#' @importFrom magrittr set_colnames
#' @importFrom magrittr "%>%"
#' @importFrom scales rescale
#' @importFrom tidyr gather
#' @importFrom tidyr nest
#' @importFrom sp point.in.polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom stringi stri_trans_general
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
                                               "Oceania", "Antarctica",
                                               "Seven seas (open ocean)"),
                                 resolution = c("small", "large"),
                                 size = c("100", "250", "1000")
                                 ){
  continent <- match.arg(continent, choices = c("North America", "Asia", "Africa",
                                                "Europe", "South America",
                                                "Oceania", "Antarctica",
                                                "Seven seas (open ocean)"))
  res <- match.arg(resolution, choices = c("small", "large"))
  data <- get_continent_data(continent, res)
  data <- process(data, size, res, flags_dir, type = "country")
  finalize(data)
}

#' @title Use flags as a fill argument in a ggplot2 map
#' @param country \code{character}. For a list of countries, run
#'  \code{country_list()} in the R console.
#' @export
flag_fillr_country <- function(country = "", resolution = c("small", "large"),
                               size = c("100", "250", "1000")){
  country <- tolower(country)
  res <- match.arg(resolution, choices = c("small", "large"))
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  data <- get_country_data(country, res)
  data <- process(data, size, res, flags_dir, type = "country")
  finalize(data)
}


#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_states} uses state-level flags as a fill for a
#' particular country.
#' @param country \code{character}. For a list of countries, run
#'  \code{country_list_states()} in the R console. (This extra function is needed
#'  due to inconsistencies in the country names across rnaturalearth.)
#'  @param remove_non_mainland filter out far-flung territories from the data?
#' @examples
#' \dontrun{
#' flag_fillr_states(country = "Brazil", resolution = "small", size = 250)
#' }
#' @export
flag_fillr_states <- function(country = "", remove_non_mainland = TRUE){
  country <- tolower(country)
  country2 <- gsub(" ", "-", country)
  flags_dir <- dir(paste0("state-flags/", country2, "-flags/"))

  data <- get_states_data(country, res)
  # some tidying up:
  if(remove_non_mainland == TRUE){
    # d3 with projections has a better way of doing this
    if(country == "united states of america"){
      data <- data %>%
        dplyr::filter(!name %in% c("district_of_columbia", "alaska", "hawaii"))
    }
    if(country == "netherlands"){
      data <- data %>%
        dplyr::filter(!name %in% c("saba", "st._eustatius"))
    }
  }

  country_list <- flag_filter(flags_dir, data)
  data <- suppressMessages(
    left_join(data, country_list) %>%
      mutate(flag_image = list(array(NA, c(1, 1, 3))))
  )
  flag_filterz <- gsub("\\.png", "", flags_dir)
  data <- data %>%
    dplyr::filter(name %in% flag_filterz)
  png_readr(data, type = "states")
  finalize(data)
}

#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_data} uses state-level flags as a fill for a
#' particular country.
#' @examples
#' \dontrun{
#' flag_fillr_data()
#' }
#' @export
flag_fillr_data <- function(data, partner_col = NULL,
                            resolution = c("small", "large"),
                            type = c("country", "state"),
                            country = NULL, size = c("100", "250", "1000"),
                            countries_col = NULL, states_col = NULL){
  # data needs state/country; partner/image (map to iso codes); 
  res <- match.arg(resolution, choices = c("small", "large"))
  type = match.arg(type, choices = c("country", "state"))
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  
  if(!is.null(countries_col)){
    type <- "country"
    countries <- countries_col
  } else if(!is.null(states_col)){
    type <- "state"
    states <- states_col
  }
  
  if(is.null(partner_col)){
    stop("Need partner info")
  }
  partner <- partner_col
  
  # shortcut if data is already sf
  
  # data
  
  if(type == "states"){ ############# do states
    country <- tolower(country)
    df <- get_states_data(country)
    }
  
  if(type == "country"){
    countries <- tolower(countries)
    partner <- tolower(partner)
    if(res == "small"){
      df <- rnaturalearth::countries110
    } else{
      df <- rnaturalearth::countries10
    }
    df_main <- df %>% sf::st_as_sf() %>%
      dplyr::select(name, iso = iso_a2, geometry) %>%
      mutate(name = tolower(name)) %>%
      dplyr::filter(name %in% countries) %>%
      mutate(iso = tolower(iso))
    
    df_user <- data %>% 
      mutate(countries = tolower(countries),
             partners = tolower(partners))
    
    df_partner <- df %>% st_as_sf() %>%
      dplyr::select(name, iso = iso_a2) %>%
      mutate(name = tolower(name)) %>%
      dplyr::filter(name %in% partner) %>% 
      mutate(iso = tolower(iso)) %>% 
      select(partners = name, iso_partner = iso) %>% 
      as_data_frame()
    suppressMessages(
      DF <- full_join(df_main, df_user, by = c("name" = "countries")) %>% 
      as_data_frame() %>% 
      full_join(df_partner) %>% st_as_sf()
    )
    
    # flags based on iso_partner:
    flags_dir <- dir(paste0(type, "-flags/png", pixels, "px/"))
    country_list <- flags_dir %>% gsub("\\.png", '', .) %>%
      .[which(. %in% DF$iso_partner)] %>%
      as_data_frame() %>%
      rename(iso = value)
    DF <- suppressMessages(
      left_join(DF, country_list) %>%
        mutate(flag_image = list(array(NA, c(1, 1, 3))))
    )
    flags <- paste0(DF$iso_partner, ".png")
    flags <- paste0(type, "-flags/png", pixels, "px/", flags)
    for(i in 1:nrow(DF)){
      DF$flag_image[[i]] <- readPNG(source = flags[[i]])
    }
    finalize(DF)
  }
  
}
