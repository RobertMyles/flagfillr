#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_data} uses user-supplied data to get flags as a fill for a map of a
#' particular country.
#' @param data
#' @param country
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
                            country_col = NULL){

  res <- match.arg(resolution, choices = c("small", "large"))
  type <- match.arg(type, choices = c("country", "state"))
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  kols <- colnames(data) %>% tolower()

  # idea: use flags of countries inside states of a country
  # or flags of other countries in a country
  # have to specify country (territory for the map) {country}{type -- fill a country's states or a country?}
  # have to specify which flags get used {partner}{iso?}
  # res, size

  # checks
  if(is.null(data)){
    stop("Need data!")
  }
  if(type == "state"){
    available <- dir("state-flags/") %>% gsub("flags", "", .) %>%
      gsub("-", " ", .) %>% trimws()
    country <- tolower(country)
    if(!country %in% available){
      stop("Flags for this country are not available yet. If the country does indeed have lower-level flags, please feel free to contribute a folder of the images to the repo of this project at github.com/RobertMyles/flagfillr.")
    }
  }


  if(is.null(country)){
    stop("Need to specify a country! Where are we going to plot this thing? The moon?")
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
    flags_dir <- dir(paste0("country-flags/png", pixels, "px/"))
    if(res == "small"){
      df <- rnaturalearth::countries110 %>%
        st_as_sf() %>% as_data_frame() %>%
        select(country = admin, iso = iso_a2, geometry) %>%
        mutate(country = tolower(country), iso = tolower(iso)) %>%
        dplyr::filter(country == UQ(country))
    } else{
      df <- rnaturalearthhires::countries10 %>%
        st_as_sf() %>% as_data_frame() %>%
        select(country = ADMIN, iso = ISO_A2, geometry) %>%
        mutate(country = tolower(country), iso = tolower(iso)) %>%
        dplyr::filter(country == UQ(country))
    }
  } else{
    flags_dir <- dir(paste0("state-flags/", country, "-flags/"))
    df <- rnaturalearthhires::states10 %>%
      st_as_sf() %>% as_data_frame() %>%
      select(country = admin, state = name, geometry) %>%
      mutate(country = tolower(country), state = tolower(state),
             iso = tolower(iso)) %>%
      dplyr::filter(country == UQ(country))
  }

  # get flags:
  iso_list <- flags_dir %>% gsub("\\.png", '', .) %>%
    .[which(. %in% df$iso)] %>%
    as_data_frame() %>%
    rename(iso = value) %>% pull()

  df <- df %>% dplyr::filter(iso %in% iso_list) %>%
    mutate(flag_image = list(array(NA, c(1, 1, 3))))

  flags <- paste0(df$iso, ".png")
  if(type == "country"){
    flags <- paste0("country-flags/png", pixels, "px/", flags)
  } else{
    flags <- paste0("state-flags/", country, "-flags/")
  }

  for(i in 1:nrow(df)){
    df$flag_image[[i]] <- readPNG(source = flags[[i]])
  }
  finalize(df)


  #   df_partner <- df %>% st_as_sf() %>% as_data_frame() %>%
  #     dplyr::select(name, iso = iso_a2) %>%
  #     mutate(name = tolower(name)) %>%
  #     dplyr::filter(name %in% partner) %>%
  #     mutate(iso = tolower(iso)) %>%
  #     select(partner = name, iso_partner = iso)


  #
  #   suppressMessages(
  #     DF <- full_join(df_main, df_user, by = c("name" = "countries")) %>%
  #       as_data_frame() %>%
  #       full_join(df_partner) %>% st_as_sf()
  #   )
  #
  #   # flags based on iso_partner:
  #   flags_dir <- dir(paste0(type, "-flags/png", pixels, "px/"))
  #   country_list <- flags_dir %>% gsub("\\.png", '', .) %>%
  #     .[which(. %in% DF$iso_partner)] %>%
  #     as_data_frame() %>%
  #     rename(iso = value)
  #   DF <- suppressMessages(
  #     left_join(DF, country_list) %>%
  #       mutate(flag_image = list(array(NA, c(1, 1, 3))))
  #   )
  #   flags <- paste0(DF$iso_partner, ".png")
  #   flags <- paste0(type, "-flags/png", pixels, "px/", flags)
  #   for(i in 1:nrow(DF)){
  #     DF$flag_image[[i]] <- readPNG(source = flags[[i]])
  #   }
  #   finalize(DF)
  #
  # } else if(!is.null(state_col)){
  #   type <- "state"
  #   states <- state_col
  #
  #   country <- tolower(country)
  #   states <- tolower(states) %>% gsub(" ", "_", .)
  #   states <- cbind(states, partner) %>% as_data_frame()
  #
  #   DF <-  %>% sf::st_as_sf() %>%
  #     dplyr::mutate(admin = tolower(admin), name = tolower(name),
  #                   iso_a2 = tolower(iso_a2))
  #   suppressMessages(
  #     df_partner <- DF %>% as_data_frame() %>%
  #       dplyr::select(admin, iso_a2) %>%
  #       dplyr::filter(admin %in% partner) %>%
  #       dplyr::select(partner = admin, iso= iso_a2) %>%
  #       distinct(partner, .keep_all = TRUE) %>%
  #       full_join(states)
  #   )
  #
  #   df_states <-  DF %>%
  #     dplyr::mutate(name = stringi::stri_trans_general(name, "Latin-ASCII"),
  #                   name = gsub(" ", "_", name)) %>%
  #     dplyr::filter(name %in% tolower(state_col), admin == country) %>%
  #     dplyr::select(country = admin, name, geometry) %>% as_data_frame()
  #
  #   suppressMessages(df <- full_join(df_states, df_partner, by = c("name" = "states")))
  #   df <- df %>% st_as_sf()
  #
  #   # flags based on iso_partner:
  #   flags_dir <- dir(paste0("country-flags/png", size, "px/"))
  #   country_list <- flags_dir %>% gsub("\\.png", '', .) %>%
  #     .[which(. %in% df$iso)] %>%
  #     as_data_frame() %>%
  #     rename(iso = value)
  #   df <- suppressMessages(
  #     left_join(df, country_list) %>%
  #       mutate(flag_image = list(array(NA, c(1, 1, 3)))) %>%
  #       as_data_frame() %>% sf::st_as_sf()
  #   )
  #   flags <- paste0(df$iso, ".png")
  #   flags <- paste0("country-flags/png", pixels, "px/", flags)
  #   for(i in 1:nrow(df)){
  #     df$flag_image[[i]] <- readPNG(source = flags[[i]])
  #   }
  #   finalize(df)
  # }
#}
}


