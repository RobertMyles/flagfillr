#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_data} uses user-supplied data to get flags as a fill for a map of a
#' particular country.
#' @param data
#' @param country if country is NULL, you must have country_col
#' @param partner_col
#' @details This function accepts a data.frame that must contain at least two columns: one for
#' the base territory (i.e. the area that will be mapped), and another for the names of the countries
#' whose flags will be used as fills in the map. If the goal is to fill states inside of one country,
#' the base territory column should be called "state"; if you want to map entire countries with one fill,
#' it should be called "country". The column of the flags to be used should be called "partner".
#' If you would rather not rename the columns in your data, you can specify the 'partner'
#' (\code{partner_col}), 'state' (\code{state_col}), and 'country' (\code{country_col}) columns.
#' These must be the actual columns, written in the format \code{df$state}, where \code{df} is your
#' data.frame and \code{state} is the 'state' column (with whatever name you have for it).
#' @examples
#' \dontrun{
#' df <- data.frame(
#'  state = c("Drenthe", "Flevoland", "Frisian", "Gelderland", "Groningen",
#'            "Limburg", "Noord-Brabant", "Noord-Holland", "Overijissel",
#'            "Utrecht", "Zeeland", "Zuid-Holland"),
#'  partner = c("Germany", "United Kingdom", "Germany", "United States",
#'              "Denmark", "Norway", "Sweden", "Germany", "Belgium",
#'              "France", "United Kingdom", "France"),
#'  stringsAsFactors = FALSE
#' )
#' flag_fillr_data(df, country = "Netherlands", size = "250")
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
    available <- dir("state-flags/") %>% gsub("flags", "", .) %>%
      gsub("-", " ", .) %>% trimws()
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
  flags_dir <- dir(paste0("country-flags/png", pixels, "px/"))
  iso_list <- flags_dir %>% gsub("\\.png", '', .) %>%
    .[which(. %in% df_partner$iso)] %>%
    as_data_frame() %>%
    rename(iso = value) %>% pull()

  df_partner <- df_partner %>% dplyr::filter(iso %in% iso_list) %>%
    mutate_all(stri_trans_general, "Latin-ASCII") %>%
    mutate(flag_image = list(array(NA, c(1, 1, 3))))

  data <- data %>% mutate_all(tolower) %>%
    mutate_all(stri_trans_general, "Latin-ASCII")

  if(type=="state"){
    suppressMessages(df <- df %>%
                       dplyr::filter(!is.na(state)) %>%
                       mutate_at(.vars = c("state", "country"), stri_trans_general, "Latin-ASCII") %>%
                       full_join(data) %>%
                       full_join(df_partner))
  } else{
    suppressMessages(df <- df %>%
                       mutate_at(.vars = c("country"), stri_trans_general, "Latin-ASCII") %>%
                       full_join(data) %>%
                       full_join(df_partner))
  }


  flags <- paste0(df$iso, ".png")
  flags <- paste0("country-flags/png", pixels, "px/", flags)

  for(i in 1:nrow(df)){
    df$flag_image[[i]] <- readPNG(source = flags[[i]])
  }
  if(country == "united states of america" & mainland_only == TRUE){
    df <- dplyr::filter(df, !state %in% c("hawaii", "district of columbia", "alaska"))
  }
  df <- df %>% st_as_sf()
  finalize(df)
}
