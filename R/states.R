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
flag_fillr_states <- function(country = NULL, mainland_only = TRUE) {
  pixels <- ""
  kountry <- tolower(country)

  if (kountry == "united states" | kountry == "us" | kountry == "usa") {
    kountry <- "united states of america"
  }
  data <- get_states_data(kountry)

  # some tidying up:
  if (mainland_only == TRUE) {
    # d3 with projections has a better way of doing this

    flag_image <- png_readr(kountry, pixels, type = "state")
    flag_filterz <- gsub("\\.png", "", names(flag_image))

    flagz <- data.table(
      country = kountry,
      name = flag_filterz,
      flag_image = flag_image
    )
    data <- data[country == kountry][name %in% flag_filterz] ## why assign
    data <- data[flagz, on = "name"] %>% st_as_sf()## why assign

    if (kountry == "united states of america") {
      data <- data[!name %in% c("district_of_columbia", "alaska", "hawaii")]## why assign
    }
    if(kountry == "netherlands"){
      data <- data[!name %in% c("saba", "st._eustatius")]## why assign
    }

    finalize(data)
  } else {
    flag_image <- png_readr(country, pixels, type = "state")
    flag_filterz <- gsub("\\.png", "", names(flag_image))
    flagz <- data.table(
      country = kountry,
      name = flag_filterz,
      flag_image = flag_image
    )
    data <- data[name %in% flag_filterz] ## why assign
    data <- data[flagz, on = "name"] %>% st_as_sf()
    finalize(data)
  }
}

