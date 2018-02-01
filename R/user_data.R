#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_data} uses user-supplied data to get flags as a fill for a map of a
#' particular country.
#' @details This function accepts a data.frame that must contain at least two columns: one for
#' the base territory (i.e. the area that will be mapped), and another for the names of the countries
#' whose flags will be used as fills in the map. If the goal is to fill states inside of one country, 
#' the base territory column should be called "state"; if you want to map entire countries with one fill,
#' it should be called "country". The column of the flags to be used should be called "partner".  
#' If you would rather not rename the columns in your data, you can specify the 'partner' 
#' (\code{partner_col}), 'state' (\code{state_col}), and 'country' (\code{country_col}) columns. 
#' These must be the actual columns, written in the format \code{df$state}, where \code{df} is your 
#' data.frame and \code{state} is the 'state' column (with whatever name you have for it). If you already 
#' possess the ISO_A2 codes for the country names for the partner flags, you can enter this information as
#' a similar vector, using the argument \code{iso_codes}, for example \code{df$ISO_A2}. 
#' @examples
#' \dontrun{
#' flag_fillr_data()
#' }
#' @export
flag_fillr_data <- function(data = NULL, country = NULL, # take out country?
                            partner_col = NULL, iso_codes = NULL,
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
  if(is.null(country)){
    stop("Need to specify a country! Where are we going to plot this thing? The moon?")
  }
  if(is.null(partner_col) && !grepl("partner", kols)){
    stop("Need a partner...grab 'em by the hand")
  }
  if(type == "state" && is.null(state_col) && !grepl("state", kols)){
    stop("Missing a column of states. YOU LOSE!")
  } else if(type == "country" && is.null(country_col) && !grepl("country", kols)){
    stop("Missing a country column...")
  }
  
  
  
  
  
  
  # data needs state/country; partner/image (map to iso codes); 
  # dataframe could be country (unique value); state; partner
  # state data needs a parent country
  res <- match.arg(resolution, choices = c("small", "large"))
  type = match.arg(type, choices = c("country", "state"))
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  
  if(is.null(country) && type == "state"){
    stop("Please specifiy a country.")
  }
  
  if(!is.null(iso_codes)){
    flags_dir <- dir(paste0("country-flags/png", pixels, "px/"))
    
    iso_list <- flags_dir %>% gsub("\\.png", '', .) %>%
      .[which(. %in% iso_codes)] %>%
      as_data_frame() %>%
      rename(iso = value) %>% pull()
    
    if(type == "country"){
      if(res == "small"){
        df <- rnaturalearth::countries110
      } else{
        df <- rnaturalearth::countries10
      }
    }
    df <- df %>% st_as_sf() %>% 
      select(admin, iso = iso_a2, geometry) %>% 
      mutate(iso = tolower(iso)) %>% 
      filter(iso %in% iso_list) %>% 
      mutate(flag_image = list(array(NA, c(1, 1, 3))))
    
    flags <- paste0(df$iso, ".png")
    flags <- paste0("country-flags/png", pixels, "px/", flags)
    for(i in 1:nrow(df)){
      df$flag_image[[i]] <- readPNG(source = flags[[i]])
    }
    finalize(df)
  }
  if(is.null(partner_col)){
    stop("Need partner info")
  }
  
  partner <- tolower(partner_col)
  
  
  if(!is.null(countries_col)){
    type <- "country"
    country <- NULL
    countries <- countries_col
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
             partner = tolower(partner))
    
    df_partner <- df %>% st_as_sf() %>% as_data_frame() %>% 
      dplyr::select(name, iso = iso_a2) %>%
      mutate(name = tolower(name)) %>%
      dplyr::filter(name %in% partner) %>% 
      mutate(iso = tolower(iso)) %>% 
      select(partner = name, iso_partner = iso)
    
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
    
  } else if(!is.null(state_col)){
    type <- "state"
    states <- state_col
    
    country <- tolower(country)
    states <- tolower(states) %>% gsub(" ", "_", .)
    states <- cbind(states, partner) %>% as_data_frame()
    
    DF <- rnaturalearthhires::states10 %>% sf::st_as_sf() %>%
      dplyr::mutate(admin = tolower(admin), name = tolower(name),
                    iso_a2 = tolower(iso_a2))
    suppressMessages(
      df_partner <- DF %>% as_data_frame() %>% 
        dplyr::select(admin, iso_a2) %>% 
        dplyr::filter(admin %in% partner) %>% 
        dplyr::select(partner = admin, iso= iso_a2) %>% 
        distinct(partner, .keep_all = TRUE) %>% 
        full_join(states) 
    )
    
    df_states <-  DF %>%
      dplyr::mutate(name = stringi::stri_trans_general(name, "Latin-ASCII"),
                    name = gsub(" ", "_", name)) %>% 
      dplyr::filter(name %in% tolower(state_col), admin == country) %>%
      dplyr::select(country = admin, name, geometry) %>% as_data_frame()
    
    suppressMessages(df <- full_join(df_states, df_partner, by = c("name" = "states")))
    df <- df %>% st_as_sf()
    
    # flags based on iso_partner:
    flags_dir <- dir(paste0("country-flags/png", size, "px/"))
    country_list <- flags_dir %>% gsub("\\.png", '', .) %>%
      .[which(. %in% df$iso)] %>%
      as_data_frame() %>%
      rename(iso = value)
    df <- suppressMessages(
      left_join(df, country_list) %>%
        mutate(flag_image = list(array(NA, c(1, 1, 3)))) %>% 
        as_data_frame() %>% sf::st_as_sf()
    )
    flags <- paste0(df$iso, ".png")
    flags <- paste0("country-flags/png", pixels, "px/", flags)
    for(i in 1:nrow(df)){
      df$flag_image[[i]] <- readPNG(source = flags[[i]])
    }
    finalize(df)
  }
}


