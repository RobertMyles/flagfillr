
flag_fillr <- function(data = data){
  # establish boundaries; rescale to boundaries; filter into polygon
  # inspiration from jalapic's question, and inscaven's amazing answer:
  #https://stackoverflow.com/questions/28206611/adding-custom-image-to-geom-polygon-fill-in-ggplot
  # and Giora Simchoni's ggwithimages package, which reminded me I wanted to do this,
  # and had the links to the above So question. Goooooo R community!
  #http://giorasimchoni.com/2018/01/03/2018-01-03-congratulations-it-s-a-geom/

  # establish bounding boxes
  xmin <- map(data$geometry, st_bbox) %>% map_dbl("xmin")
  xmax <- map(data$geometry, st_bbox) %>% map_dbl("xmax")
  ymin <- map(data$geometry, st_bbox) %>% map_dbl("ymin")
  ymax <- map(data$geometry, st_bbox) %>% map_dbl("ymax")

  # check for alpha value
  alpha_check <- function(flag_image){
    if(dim(flag_image)[3] > 3) hasalpha <- TRUE else hasalpha <- FALSE
  }
  alph <- map_lgl(data$flag_image, alpha_check)

  # matrix of colours
  NumRow <- map_dbl(data$flag_image, function(x) dim(x)[1])
  NumCol <- map_dbl(data$flag_image, function(x) dim(x)[2])
  matrixList <- vector("list", nrow(data))
  matrixList <- mapply(matrix, matrixList, data = "#00000000",
                       nrow = NumRow, ncol = NumCol, byrow = FALSE)
  # with thanks to Daniel Falbel:
  matrixList <- map2(data$flag_image, alph, function(x, y) {
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
  latlonList <- map(data$geometry, st_coordinates)

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
  data <- data %>%
    mutate(latlon = latlonList, plot_image = matrixList)

  return(data)
}
