rm(list = ls())
setwd("country-flags/png250px/")
#setwd("country-flags/png1000px/")
library(rnaturalearth)
library(dplyr)
library(png)
#devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
library(tidyr)
library(scales)
library(sf) 
library(sp)
library(purrr)
library(magrittr)
library(brmap)
library(rnaturalearthhires)
# do for other countries, do in D3 with tooltips. Do one of the globe, 
# each country with its flag! Do US, Europe, Australia
#globe <- countries10 %>% st_as_sf()

globe <- countries10 %>% st_as_sf() %>% 
  filter(!is.na(ISO_A2)) %>% 
  select(state = SUBUNIT, iso = ISO_A2, continent = CONTINENT,
         region = SUBREGION, geometry) %>% 
  mutate(iso = tolower(iso))

country_list <- dir() %>% gsub("\\.png", '', .) %>% 
  .[which(!. %in% globe$iso)] %>% as_data_frame() %>% rename(iso = value)
globe <- left_join(globe, country_list) %>% 
  mutate(flag_image = list(array(NA, c(1, 1, 3))))
flags <- paste0(globe$iso, ".png")
for(i in 1:nrow(globe)){
  globe$flag_image[[i]] <- readPNG(source = flags[[i]])
}
br <- brmap_estado %>% rename(state = estado)
br <- ne_states(country = "Brazil", returnclass = "sf")


######################### set up data frames ############

trade <- tibble(
  #state = br$state,
  state = br$woe_name,
  partner = NA_character_,
  flag = NA_character_,
  flag_image = list(array(NA, c(1, 1, 3)))
) %>% 
  mutate(partner = case_when(
    state == "Acre" ~ "Peru",
    state == "Alagoas" ~ "China",
    state == "Amapá" ~ "USA",
    state == "Amazonas" ~ "Argentina",
    state == "Bahia" ~ "China",
    state == "Ceará" ~ "USA",
    state == "Distrito Federal" ~ "China",
    state == "Espírito Santo" ~ "USA",
    state == "Goiás" ~ "China",
    state == "Maranhão" ~ "Canada",
    state == "Mato Grosso" ~ "China",
    state == "Mato Grosso do Sul" ~ "China",
    state == "Minas Gerais" ~ "China",
    state == "Pará" ~ "China",
    state == "Paraíba" ~ "USA",
    state == "Paraná" ~ "China",
    state == "Pernambuco" ~ "Argentina",
    state == "Piauí" ~ "China",
    state == "Rio de Janeiro" ~ "China",
    state == "Rio Grande do Norte" ~"USA",
    state == "Rio Grande do Sul" ~ "China",
    state == "Rondônia" ~ "Hong Kong",
    state == "Roraima" ~ "Venezuela",
    state == "Santa Catarina" ~ "USA",
    state == "São Paulo" ~ "USA",
    state == "Sergipe" ~ "Netherlands",
    TRUE ~ "China"
    ),
    flag = case_when(
      partner == "China" ~ "cn.png",
      partner == "USA" ~ "us.png",
      partner == "Argentina" ~ "ar.png",
      partner == "Netherlands" ~ "nl.png",
      partner == "Venezuela" ~ "ve.png",
      partner == "Canada" ~ "ca.png",
      partner == "Peru" ~ "pe.png",
      TRUE ~ "hk.png"
    ))

for(i in 1:27){
  trade$flag_image[[i]] <- readPNG(source = trade$flag[[i]])
}

br <- select(br, state, geometry)
br <- select(br, state= woe_name, geometry)
brazil <- full_join(trade, br)
rm(trade, br, i)

################ transform for plotting ###############
flag_fill <- function(df){
  if(!"sf" %in% class(df)){
    stop("df must be a simple features data.frame")
  }
  # establish boundaries; rescale to boundaries; filter into polygon
  
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
  
  # matrix 
  NumRow <- map_dbl(df$flag_image, function(x) dim(x)[1])
  NumCol <- map_dbl(df$flag_image, function(x) dim(x)[2])
  matrixList <- vector("list", nrow(df))
  matrixList <- mapply(matrix, matrixList, data = "#00000000", 
                    nrow = NumRow, ncol = NumCol, byrow = FALSE)
  
  matrixList <- map2(df$flag_image, alph, function(x, y) {
    rgb(x[,,1], x[,,2], x[,,3],
        ifelse(y, x[,,4], 1)
    ) %>% 
      matrix(ncol = dim(x)[2], nrow = dim(x)[1])
  })
  
  df_func <- function(DF){
    suppressWarnings(
    DF <- DF %>% 
      set_colnames(value = 1:ncol(.)) %>% 
      mutate(Y = nrow(.):1) %>% 
      gather(X, color, -Y) %>% 
      select(X, Y, color) %>%
      mutate(X = as.integer(X))
    )
      return(DF)
  }
  
  matrixList <- map(matrixList, as.data.frame)
  matrixList <- map(matrixList, df_func)
  # resize
  for(m in 1:length(matrixList)){
    matrixList[[m]]$X <- rescale(matrixList[[m]]$X, 
                                 to = c(xmin[[m]], xmax[[m]]))
    matrixList[[m]]$Y <- rescale(matrixList[[m]]$Y, 
                                 to = c(ymin[[m]], ymax[[m]]))
  }
  
  # filter into polygon
  latlonList <- map(df$geometry, st_coordinates)
  for(ll in 1:length(latlonList)){
    latlonList[[ll]] <- latlonList[[ll]][, 1:2]
  }
  poly_check <- function(x, y){
    x <- x[point.in.polygon(x$X, x$Y, 
                            y[, 1], 
                            y[, 2]
                            ) %>% 
             as.logical, ]
    return(x)
  }
  matrixList <- Map(poly_check, matrixList, latlonList)
  
  # put back in dataframe:
  df <- df %>% 
    mutate(latlon = latlonList, plot_image = matrixList)
  
  return(df)
}


flag_plot <- function(df){
  p <- ggplot() 
  df_list <- unique(df$state)
  for (i in seq_along(df_list)){
   DF <- df$plot_image[[i]]
    p <- p + geom_tile(data = DF,
                  aes(x = X, y = Y), 
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

################

br <- flag_fill(brazil)
flag_plot(br)
globe %>% filter(continent == "Asia") %>% flag_fill() %>% 
  flag_plot()
globe %>% filter(region == "Western Europe") %>% flag_fill() %>% 
  flag_plot()
globe %>% filter(state %in% c("Portugal")) %>% 
  flag_fill() %>% 
  flag_plot()
sa %>% mutate(state = name) %>% flag_fill() %>% flag_plot()
