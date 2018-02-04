#' @title Use flags as a fill to plot a ggplot2 map
#' @description \code{flag_fillr_states} uses state-level flags as a fill for a
#' particular country.
#' @param country \code{character}. For a list of countries, run
#' \code{country_list_states()} in the R console. (This extra function is needed
#' due to inconsistencies in the country names across rnaturalearth.) For the USA, you can
#' use the full name or 'us'/'usa'. See notes.
#' @param mainland_only filter out far-flung territories from the data?
#' @note Not all countries currently have flags available in the package, or will ever have, since
#' many don't have lower-administrative-level flags. Check the 'state-flags' folder on the github
#' repo for this project (https://github.com/RobertMyles/flagfillr) to see what's currently available.
#' @examples
#' \dontrun{
#' flag_fillr_states(country = "Brazil")
#' }
#' @export
flag_fillr_states <- function(country = NULL, mainland_only = TRUE){
  pixels <- ""
  country <- tolower(country)
  if(country == "united states" | country == "us" | country == "usa"){
    country <- "united states of america"
  }
  data <- get_states_data(country)

  # some tidying up:
  if(mainland_only == TRUE){
    # d3 with projections has a better way of doing this

    flag_image <- png_readr(country, pixels, type = "state")
    flag_filterz <- gsub("\\.png", "", names(flag_image))
    flagz <- dplyr::data_frame(
      country = country,
      name = flag_filterz,
      flag_image = flag_image
    )
    data <- data %>% dplyr::filter(name %in% flag_filterz)
    suppressMessages(data <- full_join(data, flagz) %>% st_as_sf())
    if(country == "united states of america"){
      data <- data %>%
        dplyr::filter(!name %in% c("district_of_columbia", "alaska", "hawaii"))
    }
    if(country == "netherlands"){
      data <- data %>%
        dplyr::filter(!name %in% c("saba", "st._eustatius"))
    }

    finalize(data)
  } else{
    flag_image <- png_readr(country, pixels, type = "state")
    flag_filterz <- gsub("\\.png", "", names(flag_image))
    flagz <- dplyr::data_frame(
      country = country,
      name = flag_filterz,
      flag_image = flag_image
    )
    data <- data %>% dplyr::filter(name %in% flag_filterz)
    suppressMessages(data <- full_join(data, flagz) %>% st_as_sf())
    finalize(data)
  }

}

# get data for state flag datasets:
get_states_data <- function(country){
  data <- rnaturalearthhires::states10 %>% sf::st_as_sf() %>%
    dplyr::select(country = admin, name, geometry) %>%
    mutate(country = tolower(country), name = tolower(name)) %>%
    dplyr::filter(country == UQ(country), !is.na(name)) %>%
    mutate(name = stringi::stri_trans_general(name, "Latin-ASCII"),
           name = gsub(" ", "_", name))
}
