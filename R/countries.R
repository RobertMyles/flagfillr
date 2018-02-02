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
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate_at
#' @importFrom dplyr filter
#' @importFrom rlang UQ
#' @importFrom grDevices rgb
#' @importFrom rlang UQS
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
#' @title Use flags as a fill argument in a ggplot2 map
#' @description Plot a map of a country using flags as fills.
#' @param country \code{character}. For a list of countries, run
#'  \code{country_list()} in the R console.
#' @param resolution detail of rnaturalearth data.
#' @param size size of flag image.
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

# data for countries:
get_country_data <- function(country, res){
  if(res == "small"){
    data <- rnaturalearth::countries110 %>% sf::st_as_sf() %>%
      dplyr::select(name, iso = iso_a2, geometry)
  } else{
    data <- rnaturalearthhires::countries10 %>% sf::st_as_sf() %>%
      dplyr::select(name = NAME, iso = ISO_A2, geometry)
  }
  data <- data %>%
    mutate(name = tolower(name)) %>%
    dplyr::filter(name == UQ(country)) %>%
    mutate(iso = tolower(iso))
  czech <- length(row.names(data))
  if(czech < 1) stop("Country not found. Try `country_list()`")
  return(data)
}
