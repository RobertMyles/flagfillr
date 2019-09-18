# https://www.crwflags.com
# X11(type = "cairo")
# spanish flags: https://en.wikipedia.org/wiki/List_of_Spanish_flags need attribution
# sweden too https://en.wikipedia.org/wiki/List_of_flags_of_Sweden
# egypt https://en.wikipedia.org/wiki/Flags_of_country_subdivisions#/media/File:Governadorat_d'Alexandria.svg
# east timor too
# + leave disclaimer/contact notice


flag_fillr <- function(data = data){
  # establish boundaries; rescale to boundaries; filter into polygon
  # inspiration from jalapic's question, and inscaven's amazing answer:
  #https://stackoverflow.com/questions/28206611/adding-custom-image-to-geom-polygon-fill-in-ggplot
  # and Giora Simchoni's ggwithimages package, which reminded me I wanted to do this,
  # and had the links to the above SO question. Goooooo R community!
  #http://giorasimchoni.com/2018/01/03/2018-01-03-congratulations-it-s-a-geom/

  # double_check
  data <- as.data.table(data)
  # create 'partner' for non-partner types:
  partner_check <- suppressWarnings(is.null(data$partner))
  if (partner_check) {
    data$partner <- data$name
  }
  # remove NULL flag_images:
  ## TODO
  ##data[!is.na(partner)][!is.na(iso)] ## is this necessary??


  # establish bounding boxes
  xmin <- map(data$geometry, st_bbox) %>% map_dbl("xmin")
  xmax <- map(data$geometry, st_bbox) %>% map_dbl("xmax")
  ymin <- map(data$geometry, st_bbox) %>% map_dbl("ymin")
  ymax <- map(data$geometry, st_bbox) %>% map_dbl("ymax")

  # check for alpha value
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
      DF <- DF %>%
        #set_colnames(value = 1:ncol(.)) %>%
        .[, Y := .N:1] %>%   # Y is a sequence of nrow back to 1
        as_tibble() %>%
        gather(X, color, -Y) %>%
        #select(X, Y, color) %>%
        as.data.table()
      DF[, X := str_remove(X, "V")][, X := as.integer(X)]

    return(DF)
  }

  matrixList <- map(matrixList, as.data.table)
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
  tst <- matrixList[1:3]
  tst2 <- latlonList[1:3]
  tst3 <- map2(tst, tst2, poly_check)
  #Map(poly_check, tst, tst2)
  #matrixList <- Map(poly_check, matrixList, latlonList)
  matrixList <- map2(matrixList, latlonList, poly_check)

  # put back in dataframe:

  data[, `:=`(
    latlon = latlonList,
    plot_image = matrixList
  )]

  return(data)
}
