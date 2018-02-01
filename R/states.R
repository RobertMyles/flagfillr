#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_states} uses state-level flags as a fill for a
#' particular country.
#' @param country \code{character}. For a list of countries, run
#' \code{country_list_states()} in the R console. (This extra function is needed
#' due to inconsistencies in the country names across rnaturalearth.) See notes.
#' @param mainland_only filter out far-flung territories from the data?
#' @note Not all countries currently have flags available in the package, or will ever have, since
#' many don't have lower-administrative-level flags. Check the 'state-flags' folder on the github
#' repo for this project (https://github.com/RobertMyles/flagfillr) to see what's currently available.
#' @examples
#' \dontrun{
#' flag_fillr_states(country = "Brazil", resolution = "small", size = 250)
#' }
#' @export
flag_fillr_states <- function(country = NULL, mainland_only = TRUE){

  country <- tolower(country)
  country2 <- gsub(" ", "-", country)
  flags_dir <- dir(paste0("state-flags/", country2, "-flags/"))
  data <- get_states_data(country)

  # some tidying up:
  if(mainland_only == TRUE){
    # d3 with projections has a better way of doing this
    if(country == "united states of america"){
      data <- data %>%
        dplyr::filter(!name %in% c("district_of_columbia", "alaska", "hawaii"))
    }
    if(country == "netherlands"){
      data <- data %>%
        dplyr::filter(!name %in% c("saba", "st._eustatius"))
    }

    flag_filterz <- gsub("\\.png", "", flags_dir)
    data <- data %>% dplyr::filter(name %in% flag_filterz)
    data <- png_readr(data, type = "state", country)
    finalize(data)
  } else{
    flag_filterz <- gsub("\\.png", "", flags_dir)
    data <- data %>% dplyr::filter(name %in% flag_filterz)
    data <- png_readr(data, type = "state", country)
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
           name = gsub(" ", "_", name),
           flag_image = list(array(NA, c(1, 1, 3))))
}
