######## move to directory and load packages #######
setwd("country-flags/png250px/")
#setwd("country-flags/png1000px/")
#setwd("country-flags/png100px/")
library(rnaturalearth)
library(rnaturalearthhires)
library(dplyr)
library(purrr)
library(tidyr)
library(scales)
library(magrittr)
library(png)
# > packageVersion("ggplot2")
# [1] ‘2.2.1.9000’
#devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
library(sf) 

library(sp)
library(brmap)
library(geojsonio)
library(rmapshaper)

########### data #########
globe <- countries10 %>% st_as_sf() %>% 
  #filter(!is.na(ISO_A2)) %>%  # france fucks this up
  select(state = SUBUNIT, iso = ISO_A2, continent = CONTINENT,
         region = SUBREGION, state2 = SOVEREIGNT, geometry) %>% 
  mutate(iso = tolower(iso))

country_list <- dir() %>% gsub("\\.png", '', .) %>% 
  .[which(!. %in% globe$iso)] %>% as_data_frame() %>% rename(iso = value)
globe <- left_join(globe, country_list) %>% 
  mutate(flag_image = list(array(NA, c(1, 1, 3))))
flags <- paste0(globe$iso, ".png")
for(i in 1:nrow(globe)){
  globe$flag_image[[i]] <- readPNG(source = flags[[i]])
}

############ flag_fill function #############
flag_fill <- function(df, filter_islands = TRUE){
  if(filter_islands == TRUE){
    df1 <- df %>% select(state, geometry) %>% 
      geojson_json(geometry = "polygon") %>% 
      ms_filter_islands() %>% geojson_sp() %>% 
      st_as_sf() %>% as_data_frame() %>% 
      mutate(state = as.character(state)) %>% 
      st_as_sf()
    df$geometry <- NULL
    attr(df, "sf_column") <- NULL
    df <- full_join(df, df1)
  }
  
    
  
  # establish boundaries; rescale to boundaries; filter into polygon
  # df must have columns 'geometry' and 'flag_image'
  
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
        select(X, Y, color) %>%
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

######## flag_plot function #############
flag_plot <- function(df){
  # takes a dataframe with column 'state' for country or state,
  # and plot_image, the result of flag_fill(), as well as 'color',
  # also the result of flag_fill()
  
  p <- ggplot() 
  df_list <- unique(df$state)
  
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

############## test ###############
globe %>% 
  #filter(region == "Seven seas (open ocean)") %>% 
  filter(state == "Albania") %>% 
  # filter(state %in% c("Ireland", "United Kingdom", "Spain",
  #                     "Portugal", "Germany", "Denmark", "Belgium",
  #                     "Italy", "Switzerland", "Ukraine", "Sweden",
  #                     "Poland", "Austria", "Latvia", "Lithuania", 
  #                     "Estonia", "Finland", 'Belarus', "Hungary",
  #                     "Romania", "Bulgaria", "Greece", "Croatia", 
  #                     "Moldova", "Slovakia", "Turkey", "Macedonia",
  #                     "Republic of Serbia", "Slovenia", "Kosovo",
  #                     "Montenegro", "Georgia", "Czech Republic",
  #                     "Bosnia and Herzegovina", "Azerbaijan",
  #                     "Morocco")) %>%
  flag_fill() %>% 
  flag_plot()

library(geojsonio)
states_json <- geojson_json(globe, geometry = "polygon")
ss <- states_json %>% ms_filter_islands() %>% geojson_sp() %>% 
  st_as_sf()
f_gl <- globe
f_gl <- select(f_gl, state, geometry)
f_gl <- ms_filter_islands(f_gl)
flags <- select(globe, state, flag_image, region, continent)
f_gl <- st_join(f_gl, flags)
f_gl %>% filter(region == "Western Africa") %>%
  flag_fill() %>% 
  flag_plot()
# one post on the method
# other on trading partners
# remember D3

library(rmapshaper)
library(geojsonio)
pt <- globe %>% filter(state == "Portugal") %>% 
   %>% 
  geojson_sp() %>% 
  st_as_sf()# %>% 
  # as.data.frame() %>% 
  # flag_fill() %>% 
  # flag_plot()

