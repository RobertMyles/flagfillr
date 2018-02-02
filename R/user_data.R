#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_data} uses user-supplied data to get flags as a fill for a map of a
#' particular country.
#' @param data A \code{data.frame}, see details.
#' @param country country name. If \code{country} is \code{NULL}, you must supply a \code{country_col}.
#' @param partner_col column of partner countries. See details.
#' @param resolution detail of rnaturalearth data.
#' @param size size of flag image.
#' @param type will you be plotting flags on countries or inside countries, on the states?
#' @param state_col column of state names. Must be full names, see details.
#' @param country_col column of country names. See details.
#' @param mainland_only remove non-contiguous landmasses? Will remove places
#' like Alaska, Hawaii, etc.
#' @details This function accepts a data.frame that must contain at least two columns:
#' one for the base territory (i.e. the area that will be mapped), and another
#' for the names of the countries whose flags will be used as fills in the map.
#' If the goal is to fill states inside of one country, the base territory column
#' should be called "state"; if you want to map entire countries with one fill,
#' it should be called "country". The column of the flags to be used should be called
#' "partner".
#' If you would rather not rename the columns in your data, you can specify the
#' 'partner' (\code{partner_col}), 'state' (\code{state_col}), and 'country'
#' (\code{country_col}) columns. These must be the actual columns, written in
#' the format \code{df$state}, where \code{df} is your data.frame and
#' \code{state} is the 'state' column (with whatever name you have for it). For
#' information on acceptable country names, see \code{country_list()}.
#' @examples
#' \dontrun{
#'  us_data <- data.frame(
#'    state = state.name, # I love R!
#'    partner = c(rep("Mexico", 15), rep("Canada", 25), rep("China", 8), rep("Japan", 2)),
#'    stringsAsFactors = FALSE
#'   )
#'   flag_fillr_data(us_data, country = "United States of America", type = "state", size = "250")
#' }
#' @export
flag_fillr_data <- function(data = NULL, country = NULL,
                            partner_col = NULL,
                            resolution = c("small", "large"),
                            type = c("country", "state"),
                            size = c("100", "250", "1000"),
                            state_col = NULL,
                            country_col = NULL,
                            mainland_only = TRUE){

  res <- match.arg(resolution, choices = c("small", "large"))
  type <- match.arg(type, choices = c("country", "state"))
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  kols <- colnames(data) %>% tolower()
  if(!is.null(country)){
    country <- country
    if(country == "United States" | country == "US" | country == "USA"){
      country <- "United States of America"
    }
  } else{
    country <- country_col
  }

  # idea: use flags of countries inside states of a country -- type "state"
  # or a flag of another country in a country -- type country
  # or flags of other countries inside various countries - type country

  # checks
  if(is.null(data)){
    stop("Need data!")
  }
  if(type == "state"){
    available <- c("argentina", "australia", "brazil", "canada", "germany", "netherlands",
                   "united states of america")
    country <- tolower(country)
    x <- ifelse(!country %in% available, 1,country)
    if(x == 1){
      stop("Flags for this country are not available yet. If the country does indeed have lower-level flags, please feel free to contribute a folder of the images to the repo of this project at github.com/RobertMyles/flagfillr.")
      }
    }
  if(is.null(country) & is.null(country_col)){
    stop("Need to specify a country! Where are we going to plot this thing? The moon?\n  You can use `country` or `country_col` to avoid seeing this annoying message.")
  }
  if(is.null(partner_col) & !TRUE %in% !grepl("partner", kols)){
    stop("Need a partner...grab 'em by the hand")
  }
  if(type == "state" & is.null(state_col) & !TRUE %in% grepl("state", kols)){
    stop("Missing a column of states. YOU LOSE!")
  } else if(type == "country" & is.null(country) & is.null(country_col) & !TRUE %in% grepl("country", kols)){
    stop("Missing a country column...")
  }

  # assignment
  country <- country %>% tolower()
  if(is.null(partner_col)){
    partner <- data$partner %>% tolower()
  } else{
    partner <- partner_col %>% tolower()
  }
  if(type == "state" & !is.null(state_col)){
    state <- state_col %>% tolower()
  } else if(type == "state" & is.null(state_col)){
    state <- data$state %>% tolower()
  }

  # get data:
  if(type == "country"){
    if(res == "small"){
      df <- rnaturalearth::countries110 %>%
        st_as_sf() %>% as_data_frame() %>%
        select(country = admin, geometry) %>%
        mutate(country = tolower(country)) %>%
        dplyr::filter(country == UQ(country))

      df_partner <- rnaturalearth::countries110 %>%
        st_as_sf() %>% as_data_frame() %>%
        select(partner = admin, iso = iso_a2) %>%
        mutate(partner = tolower(partner), iso = tolower(iso)) %>%
        dplyr::filter(partner %in% UQ(partner))
    } else{
      df <- rnaturalearthhires::countries10 %>%
        st_as_sf() %>% as_data_frame() %>%
        select(country = ADMIN, geometry) %>%
        mutate(country = tolower(country)) %>%
        dplyr::filter(country == UQ(country))

      df_partner <- rnaturalearthhires::countries10 %>%
        st_as_sf() %>% as_data_frame() %>%
        select(partner = ADMIN, iso = ISO_A2) %>%
        mutate(partner = tolower(partner), iso = tolower(iso)) %>%
        dplyr::filter(partner %in% UQ(partner))
    }
  } else if (type =="state"){
    df <- rnaturalearthhires::states10 %>%
      st_as_sf() %>% as_data_frame() %>%
      select(country = admin, state = name, geometry) %>%
      mutate(country = tolower(country), state = tolower(state)) %>%
      dplyr::filter(country == UQ(country))

    df_partner <- rnaturalearth::countries110 %>%
      st_as_sf() %>% as_data_frame() %>%
      select(partner = admin, iso = iso_a2) %>%
      mutate(partner = tolower(partner), iso = tolower(iso)) %>%
      dplyr::filter(partner %in% UQ(partner))
  }

  if(country == "netherlands" & mainland_only == TRUE){
    df <- df %>%
      dplyr::filter(!state %in% c("saba", "st. eustatius"))
  }
  if("hong kong" %in% partner){
    df_partner <- df_partner %>%
      add_row(partner = "hong kong", iso = "hk")
  }
  # get flags:
  flag_image <- png_readr(country, pixels, type = "country") ## need to get partners
  flag_filterz <- gsub("\\.png", "", names(flag_image))
  messager(res, pixels)
  isos <- df_partner$iso
  flagz <- dplyr::data_frame(
    country = country,
    iso = flag_filterz,
    flag_image = flag_image
  ) %>% dplyr::filter(iso %in% isos)

  suppressMessages(df_partner <- full_join(df_partner, flagz))
  data <- data %>% mutate_all(tolower) %>%
    mutate_all(stri_trans_general, "Latin-ASCII")
  suppressMessages(data <- full_join(data, df_partner))

  if(type=="state"){
    suppressMessages(df <- df %>%
                       dplyr::filter(!is.na(state)) %>%
                       mutate_at(.vars = c("state", "country"), stri_trans_general, "Latin-ASCII") %>%
                       full_join(data))
  } else{
    suppressMessages(df <- df %>%
                       mutate_at(.vars = c("country"), stri_trans_general, "Latin-ASCII") %>%
                       full_join(data))
  }


  if(country == "united states of america" & mainland_only == TRUE){
    df <- dplyr::filter(df, !state %in% c("hawaii", "district of columbia", "alaska"))
  }
  df <- df %>% st_as_sf()
  finalize(df)
}
