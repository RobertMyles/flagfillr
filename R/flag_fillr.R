#' @import rnaturalearth
#' @import rnaturalearthhires
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
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
flag_fillr_data <- function(data, type = c("continent", "country", "state")){

}
