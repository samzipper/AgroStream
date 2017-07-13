## plot_colors.R
#' This script is intended to hold codes corresponding to the colors
#' used for plots.

# from http://paletton.com/#uid=7000u0ktSlllysDruqa-qh2KKbE

## hot/cold
col.red  <- "#E53629"
col.blue <- "#422C9E"

## 1D/2D
line.1D <- "21"
line.2D <- "solid"

## custom theme
theme_SCZ <- function(...){
  theme_bw() +
    theme(panel.grid=element_blank(),
          panel.border=element_rect(color="black"),
          axis.ticks=element_line(color="black"),
          ...)
} 
