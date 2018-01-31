
<!-- README.md is generated from README.Rmd. Please edit that file -->
flagfillr \[WIP\]
=================

flagfillr is a little package for R that makes it easier to use flags as fills (i.e. backgrounds) for [ggplot2](https://github.com/tidyverse/ggplot2) maps.

Installation
------------

You can install flagfillr from github with:

``` r
# install.packages("devtools")
devtools::install_github("RobertMyles/flag_fillr")
```

<br> **Important Note:** flagfillr works with ggplot2's `geom_sf()`, which is currently only available in the current development version of ggplot2 (version 2.2.1.9000).

Why? For the love of god, why??!!!
----------------------------------

Well, I'm happy you asked. Imagine you saw the following image, from the [Drunkneysian](http://drunkeynesian.blogspot.com.br/), taken from [here](https://brazilian.report/2017/11/27/brazil-important-trading-partners/):

![](brazil-trade-partners.jpg)

It's an interesting subject, and a handy visual summary of the data. But... <br> ...isn't this sooo much nicer?

``` r
library(rnaturalearth)
library(flagfillr)
library(dplyr)
library(png)
### data needs to be flexible user can choose one country or many
### need to specify which flags to use in which areas
br <- ne_states(country = "Brazil", returnclass = "sf")
trade <- tibble(
  state = br$name,
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
brazil <- full_join(trade, br)
flag_fillr_data(brazil, type = "state")
```

Ok...it depends on some knowledge of flags, but look! They're so purty. And, I mean, just look at Africa...

![](africa.png)

That's pure unadulterated awesome-sauce, right there.

So in the (un)likely event you find yourself pining for flags in a ggplot2 map, flagfillr has got you covered.

<br> This package is designed to be quick and easy to use. As such, it's not sooo flexible. If you'd like to use your own data, or mess around with things, I recommend taking a look at the .R files, I hope they're reasonably easy to understand. Then you can hack away to your heart's content. The logic is simple: import the flags using `readPNG()`, establish the boundaries of the geometry, filter the flag *into* the geometry shape, use that as a fill.

<br> Pull requests and issues are welcome. I particularly welcome any datasets of flags in png format! The plotting of state-level flags is still a bit messy, and removing islands and far-off territories could be much better. The flags are great, though (Argentina has some good ones).

Related
-------

I should mention that this isn't new: XX shows how to do it with grid graphics in XX. And ggplot2 is based on grid graphics. But I haven't seen a ggplot2 one, so... For images other than flags in plots other than maps, Giora XXXX's package might be what you're looking for.

Thanks
------

The main data wrangling function in this package is not painfully slow because the talented Daniel Falbel helped me out with purrr's map2. And I don't have [distastrous 'outliers'](https://stackoverflow.com/questions/48366952/check-if-sf-geometry-is-contiguous-in-r) (get it?) because of the help of the gifted Julio Trecenti. Obrigado, pessoal!

``` r
## basic example code
```

Flags
-----

The flags used in this repo come from XX and XX. The state flags (Brazil) come from XX and the US come from XX. It's my understanding that these flags are free to use, and are available on Wikipedia. Thanks to Wikipedia and the contributors of the above-linked projects for their work on making these flags available and easy to get.

to do
-----

-\[\] create repos of flag pngs -\[\] abstract process into function or perhaps package -\[\] blog post introducing the idea -\[\] blog post showing economic partners

links
-----

-   merge this to passport? <https://github.com/mledoze/countries>

<https://github.com/CivilServiceUSA/us-states>

dependent territories:
======================

<https://en.wikipedia.org/wiki/Gallery_of_flags_of_dependent_territories>

flags by country:
=================

<https://en.wikipedia.org/wiki/List_of_Argentine_flags> (argentina)

related
-------

<https://github.com/emphaticpuma/webflags>
