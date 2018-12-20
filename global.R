# global.R

# Notes:
# ------
# Where you can prepare all data that isn't reactive, clean up things and more.
#
# Note: you could also do this preparation elsewhere, especially if it takes
# some time to run and just save it as a .Rdata file and load it in this file


# Libraries

library(tidyverse)
library(forcats)
library(reshape2)

library(gapminder)

library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(dendextend)

library(crosstalk)
library(wordcloud)
library(ggplot2)
library(matrixStats)
library(tm)
source("https://raw.githubusercontent.com/mateyneykov/315_code_data/master/code/geom_mosaic.R")
library(rsconnect)
### colors and theme

cb_pal = c("#000000", "#E69F00",
           "#56B4E9", "#009E73",
           "#F0E442", "#0072B2",
           "#D55E00", "#CC79A7")
cb_pal_cont = c("Africa" = "#000000",
                "Americas" = "#E69F00",
                "Asia" = "#56B4E9",
                "Europe" = "#009E73",
                "Oceania" = "#F0E442")

my_theme = theme_minimal() +
  theme(axis.title  = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20)
        )

############################################################################
# Choropleth Cleaning for Leaflet: HTML Widget (embedded in Shiny)
############################################################################
# The following code cleans up and filters the gapminder code to just focus
#   2007 and have matching country names as that from the shape file pulled
#   from `johan`'s github
#
#   You will also notice I also set up the color range, and labels in this
#   document so to not clutter up my server file.


biopics <-  read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/biopics/biopics.csv")


checkV <- function(x){
  substr(x, nchar(x), nchar(x))
}

convNum <- function(x, v) {
  ifelse(v,
         return(as.numeric(substr(x, 2, (nchar(x)-1)))),
         return(as.numeric(substr(x, 2, (nchar(x)-1)))/1000))
}

biopics <- biopics %>%
  mutate(
    box_office = ifelse((biopics$box_office == "-"), NA, biopics$box_office),
    box_office = as.numeric(ifelse(!is.na(biopics$box_office),
                                   (ifelse((checkV(biopics$box_office) == "M"),
                                           convNum(biopics$box_office, TRUE),
                                           convNum(biopics$box_office, FALSE))),
                                   biopics$box_office))
  )

biopics$box_office[is.na(biopics$box_office)] <- 0

biopics <- biopics[!(duplicated(biopics$title)&duplicated(biopics$year_release)),]

biopics$director[137] = "Roland Joffe"
biopics$director[138] = "Roland Joffe"
biopics$director[187] = "Hans-Jurgen Syberberg"
biopics$director[487] = "Nikolai Mullerschon"
biopics$director[488] = "Charles Biname"

