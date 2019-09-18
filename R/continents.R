#' @author Robert Myles McDonnell, \email{robertmylesmcdonnell@gmail.com}
#' @title Use flags to plot a ggplot2 map
#' @description flagfillr is a small set of convenience functions for
#' you to use flags as fills in a ggplot2 map. It will plot the map for you.
#' @param continent (\code{character}). Data come from the rnaturalearth package.
#' @param resolution resolution of rnaturalearth data.
#' @param size size of png image.
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
### #' @import rlang
#' @import data.table
#' @importFrom stringr str_remove
#' @importFrom stringi stri_trans_general
#' @importFrom sf st_as_sf
#' @examples
#' \dontrun{
#' flag_fillr_continent( continent = "Africa", resolution = "small", size = "100")
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
  data <- get_continent_data(continent, res) %>% as.data.table()
  type <- "country"
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  flag_image <- png_readr(country = NULL, pixels, type)
  flag_filterz <- gsub("\\.png", "", names(flag_image))
  isos <- data$iso
  messager(res, pixels)

  flagz <- data.table(
    iso = flag_filterz,
    flag_image = flag_image
  ) %>% .[(iso %in% isos)]

  data <- data[flagz, on = "iso"] ## why do I have to assign here?

  if(Europe_no_Russia == TRUE){
    data <- data[name != "Russia"]
  }

  finalize(data)
}
