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
#' @examples
#' \dontrun{
#' flag_fillr_continent(resolution = "small", size = 100, continent = "Africa")
#' }
#' @export
flag_fillr_continent <- function(continent = c("North America", "Asia", "Africa",
                                               "Europe", "South America", 
                                               "Oceania", "Antarctica", 
                                               "Seven seas (open ocean)"),
                                 resolution = c("small", "large"), 
                                 size = c("100", "250", "1000")
                                 ){
  #### porganise flag paths ####
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  flags_dir <- dir(paste0("../country-flags/png", pixels, "px/"))
  
  ##### get data #####
  res <- match.arg(resolution, choices = c("small", "large"))
  if(res == "small"){
    data <- rnaturalearth::countries110 %>% sf::st_as_sf()
  } else{
    data <- rnaturalearthhires::countries10 %>% sf::st_as_sf()
    message("Using large-scale data. This may take a while...")
  }
  data <- data %>% 
    dplyr::select(name, iso = iso_a2, continent, geometry) %>% 
    dplyr::filter(continent == UQ(continent)) %>% 
    mutate(iso = tolower(iso))
  # match to flags
  country_list <- flags_dir %>% gsub("\\.png", '', .) %>% 
    .[which(!. %in% data$iso)] %>% 
    as_data_frame() %>% 
    rename(iso = value)
  # read pngs
  data <- suppressMessages(
    left_join(data, country_list) %>% 
      mutate(flag_image = list(array(NA, c(1, 1, 3))))
  )
  flags <- paste0(data$iso, ".png")
  flags <- paste0("../country-flags/png", pixels, "px/", flags)
  for(i in 1:nrow(data)){
    data$flag_image[[i]] <- readPNG(source = flags[[i]])
  }
  
  output <- flag_fillr(data)
  message("Combining flags and data...\n")
  message("Creating plot...")
  flag_plotr(output)
}

#' @title Use flags as a fill argument in a ggplot2 map
#' @param country \code{character}. For a list of countries, run
#'  \code{country_list()} in the R console.
#' @export
flag_fillr_country <- function(country = "", resolution = c("small", "large"), 
                               size = c("100", "250", "1000")){
  #### flags ####
  pixels <- match.arg(size, choices = c("100", "250", "1000"))
  flags_dir <- dir(paste0("../country-flags/png", pixels, "px/"))
  
  ##### data #####
  res <- match.arg(resolution, choices = c("small", "large"))
  if(res == "small"){
    data <- rnaturalearth::countries110 %>% sf::st_as_sf() %>% 
      dplyr::select(name, iso = iso_a2, geometry)
  } else{
    data <- rnaturalearthhires::countries10 %>% sf::st_as_sf() %>% 
      dplyr::select(name = NAME, iso = ISO_A2, geometry)
    if(pixels == "1000"){
      message("Using large-scale data with high res. This may take a while...")
    }
  }
  
  data <- data %>% 
    dplyr::filter(name == UQ(country)) %>% 
    mutate(iso = tolower(iso))
  
  country_list <- flags_dir %>% gsub("\\.png", '', .) %>% 
    .[which(!. %in% data$iso)] %>% 
    as_data_frame() %>% 
    rename(iso = value)
  
  data <- suppressMessages(
    left_join(data, country_list) %>% 
      mutate(flag_image = list(array(NA, c(1, 1, 3))))
  )
  flags <- paste0(data$iso, ".png")
  flags <- paste0("../country-flags/png", pixels, "px/", flags)
  for(i in 1:nrow(data)){
    data$flag_image[[i]] <- readPNG(source = flags[[i]])
  }
  
  output <- flag_fillr(data)
  message("Combining flags and data...\n")
  message("Creating plot...")
  flag_plotr(output)
}


#' @title Use flags as a fill argument in a ggplot2 map
#' @description \code{flag_fillr_states} uses state-level flags as a fill for a 
#' particular country.
#' @param country \code{character}. For a list of countries, run
#'  \code{country_list_states()} in the R console. (This extra function is needed
#'  due to inconsistencies in the country names across rnaturalearth.) 
#' @examples
#' \dontrun{
#' flag_fillr_states(country = "Brazil", resolution = "small", size = 250)
#' }
#' @export
flag_fillr_states <- function(country = "", resolution = c("small", "large"), 
                               size = c("100", "400"),
                              remove_non_mainland = TRUE){
  #### flags ####
  #pixels <- match.arg(size, choices = c("100", "250", "1000"))
  # leave one size?
  pixels <- "100"  # for now
  country <- tolower(country)
  country2 <- gsub(" ", "-", country)
  #flags_dir <- dir(paste0("../state-flags/", country2, "-flags/png", pixels, "px/"))
  flags_dir <- dir(paste0("../state-flags/", country2, "-flags/"))
  
  ##### data #####
  data <- rnaturalearthhires::states10 %>% sf::st_as_sf() %>% 
    dplyr::select(country = admin, name, iso = iso_a2, geometry) %>% 
    dplyr::mutate(country = tolower(country),
           name = tolower(name))
  
  data <- data %>% 
    dplyr::filter(country == UQ(country), !is.na(name)) %>% 
    dplyr::mutate(name = stringi::stri_trans_general(name, "Latin-ASCII"),
           name = gsub(" ", "_", name))
  
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
  
  
  country_list <- flags_dir %>% gsub("\\.png", '', .) %>% 
    .[which(!. %in% tolower(data$name))] %>% 
    as_data_frame() %>% 
    rename(iso = value)
  
  data <- suppressMessages(
    left_join(data, country_list) %>% 
      mutate(flag_image = list(array(NA, c(1, 1, 3))))
  )
  flags <- paste0(data$name, ".png")
  #flags <- paste0("../state-flags/", country2, "-flags/png", pixels, "px/", flags)
  flags <- paste0("../state-flags/", country2, "-flags/", flags)
  for(i in 1:nrow(data)){
    try(
      XX <- readPNG(source = flags[[i]]),
      silent = FALSE
      )
    if(class(XX) == "try-error"){
      next
    } else data$flag_image[[i]] <- XX
  }
  
  output <- flag_fillr(data)
  message("Combining flags and data...\n")
  message("Creating plot...")
  flag_plotr(output)
}

########## flag filler function ##########
flag_fillr <- function(df){
  # establish boundaries; rescale to boundaries; filter into polygon
  # inspiration from jalapic's question, and inscaven's amazing answer:
  #https://stackoverflow.com/questions/28206611/adding-custom-image-to-geom-polygon-fill-in-ggplot
  # and Giora Simchoni's ggwithimages package, which reminded me I wanted to do this,
  # and had the links to the above So question. Goooooo R community!
  #http://giorasimchoni.com/2018/01/03/2018-01-03-congratulations-it-s-a-geom/
  
  # establish bounding boxes
  xmin <- map(df$geometry, st_bbox) %>% map_dbl("xmin")
  xmax <- map(df$geometry, st_bbox) %>% map_dbl("xmax")
  ymin <- map(df$geometry, st_bbox) %>% map_dbl("ymin")
  ymax <- map(df$geometry, st_bbox) %>% map_dbl("ymax")
  
  # check for alpha value
  alpha_check <- function(flag_image){
    if(dim(flag_image)[3] > 3) hasalpha <- TRUE else hasalpha <- FALSE
  }
  alph <- map_lgl(df$flag_image, alpha_check)
  
  # matrix of colours
  NumRow <- map_dbl(df$flag_image, function(x) dim(x)[1])
  NumCol <- map_dbl(df$flag_image, function(x) dim(x)[2])
  matrixList <- vector("list", nrow(df))
  matrixList <- mapply(matrix, matrixList, data = "#00000000", 
                       nrow = NumRow, ncol = NumCol, byrow = FALSE)
  # with thanks to Daniel Falbel:
  matrixList <- map2(df$flag_image, alph, function(x, y) {
    rgb(x[,,1], x[,,2], x[,,3], ifelse(y, x[,,4], 1)) %>% 
      matrix(ncol = dim(x)[2], nrow = dim(x)[1])
  })
  
  df_func <- function(DF){
    suppressWarnings(
      DF <- DF %>% 
        set_colnames(value = 1:ncol(.)) %>% 
        mutate(Y = nrow(.):1) %>% 
        gather(X, color, -Y) %>% 
        dplyr::select(X, Y, color) %>%
        mutate(X = as.integer(X))
    )
    return(DF)
  }
  
  matrixList <- map(matrixList, as.data.frame)
  matrixList <- map(matrixList, df_func)
  # resize
  for(m in 1:length(matrixList)){
    matrixList[[m]]$X <- rescale(matrixList[[m]]$X, to = c(xmin[[m]], xmax[[m]]))
    matrixList[[m]]$Y <- rescale(matrixList[[m]]$Y, to = c(ymin[[m]], ymax[[m]]))
  }
  
  # filter into polygon
  latlonList <- map(df$geometry, st_coordinates)
  
  for(ll in 1:length(latlonList)){
    latlonList[[ll]] <- latlonList[[ll]][, c(1, 2, 4)]
  }
  # with thanks to Juio Trecenti:
  poly_check <- function(x, y) {
    
    island_list <- y %>% 
      as_tibble() %>% 
      group_by(L2) %>% 
      nest() %>% 
      pull(data)
    
    lists <- map(island_list, ~{
      y <- as.matrix(.x)
      x[point.in.polygon(x$X, x$Y, y[, 1], y[, 2]) %>% as.logical, ]
    })
    bind_rows(lists, .id = ".id")
  }
  matrixList <- Map(poly_check, matrixList, latlonList)
  
  # put back in dataframe:
  df <- df %>% 
    mutate(latlon = latlonList, plot_image = matrixList)
  
  return(df)
}

############# flag plot function ###############
# uses: 
# > packageVersion("ggplot2")
# [1] 2.2.1.9000
#devtools::install_github("tidyverse/ggplot2")
flag_plotr <- function(df){
  # thanks to Julio Trecenti for the help!
  p <- ggplot() 
  df_list <- unique(df$name)
  
  for (i in seq_along(df_list)){
    DF <- df$plot_image[[i]]
    p <- p + geom_tile(data = DF, aes(x = X, y = Y, group = .id), 
                       fill = DF$color)
  }
  
  p + xlab(NULL) + ylab(NULL) +
    geom_sf(data = df, size = .2, alpha = 0.01) +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_line(colour = "white"), # hack from 
          #https://github.com/tidyverse/ggplot2/issues/2071
          axis.ticks = element_blank(),
          axis.line = element_blank())
}

###### helpers ########

#' @export
country_list <- function(){
  rnaturalearth::countries110 %>% st_as_sf() %>% pull(name) %>% unique() %>% sort()
}
#' @export
country_list_states <- function(){
  rnaturalearthhires::states10 %>% st_as_sf() %>% pull(country) %>% unique() %>% sort()
}