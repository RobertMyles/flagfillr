
############# flag plot function ###############
flag_plotr <- function(df){
  # thanks to Julio Trecenti for the help!
  p <- ggplot()
  df <- st_as_sf(df) # double check
  df_list <- 1:nrow(df)

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
          panel.grid.major = element_line(colour = "white"),
          axis.ticks = element_blank(),
          axis.line = element_blank())
}
