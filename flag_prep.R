
# see what districts we have in each country
library(tidyverse); library(sf)
rnaturalearthhires::states10 %>%
  st_as_sf() %>%
  select(country = admin, name = name_en, geometry) %>%
  mutate(country = tolower(country), name = tolower(name)) %>%
  filter(country == "egypt", !is.na(name)) %>%
  pull(name) %>% unique() %>% sort()

rnaturalearthhires::states10 %>%
  st_as_sf() %>%
  select(country = admin, name = name_en, geometry) %>%
  mutate(country = tolower(country), name = tolower(name)) %>%
  filter(country == "egypt", !is.na(name)) %>%
  ggplot() + geom_sf(aes(fill = name))

# read in SVG, convert to PNG
# img <- jpeg::readJPEG("continent-flags/eu.jpeg")
# png::writePNG(img, "continent-flags/eu.png")
library(magick)
#tiger <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 350)

png_conv <- function(file) {
  outfile <- str_replace(file, ".svg", ".png") %>%
    stringi::stri_trans_general("Latin-ASCII") %>% tolower()
  x <- image_read_svg(paste0("bg/", file)) %>%
    image_convert("png")
  image_write(x, path = paste0("bg/", outfile))
  file.remove(paste0("bg/", file))
}
files <- dir("bg", pattern = ".svg")
map(files, png_conv)

# make as data for package
library(glue)
make_flags <- function(country){
  dr <- dir(glue("state-flags/{country}-flags"))
  x_list <- map(dr, ~{png::readPNG(glue::glue("state-flags/{country}-flags/{.x}"))})
  names(x_list) <- dr
  x_list
}
flag_list_germany <- make_flags("germany")
usethis::use_data(flag_list_germany, overwrite = T)


# scale PNGs


#image_scale(tiger, "500")
