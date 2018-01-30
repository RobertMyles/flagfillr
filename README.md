
<!-- README.md is generated from README.Rmd. Please edit that file -->
flagfillr
=========

flagfillr is a little package that makes it easier to use flags as fills (i.e. backgrounds) for [ggplot2](https://github.com/tidyverse/ggplot2) maps.

Installation
------------

You can install flagfillr from github with:

``` r
# install.packages("devtools")
devtools::install_github("RobertMyles/flag_fillr")
```

<br> **Important Note:** flagfillr works with ggplot2's `geom_sf()`, which is currently only available in the current development version of ggplot2 (version 2.2.1.9000).

Why??! For the love of god, why??!!!
------------------------------------

Well, I'm happy you asked. Imagine you saw the following image (published by the Brazilian government and freely available):

(use drunkneysian's?)

![](mapaexportacoes.jpg)

Or worse, this monstrosity (deals with the same subject): ![](table.jpg)

Isn't this sooo much nicer?

Ok...it depends on some knowledge of flags, but look! They're so purty. And, I mean, just look at Africa...

That's pure unadulterated awesome-sauce, right there.

So in the unlikely event you find yourself pining for flags in a ggplot2 map, flagfillr has got you covered.

Related
-------

I should mention that this isn't new: XX shows how to do it with grid graphics in XX. And ggplot2 is based on grid graphics. But I haven't seen a ggplot2 one, so...

Thanks
------

The main data wrangling function in this package is not painfully slow because the talented Daniel Falbel helped me out with purrr's map2. And I don't have [distastrous 'outliers']() (get it?) because of the help of the gifted Julio Trecenti. Obrigado, pessoal!

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
