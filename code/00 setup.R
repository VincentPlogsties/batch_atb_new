
# (install &) load packages
if (!require("pacman", quietly = TRUE))
  install.packages("pacman")
pacman::p_load(
  broom,
  janitor,
  conflicted,
  desplot,
  emmeans,
  ggtext,
  multcomp,
  multcompView,
  #openxlsx,
  openxlsx2,
  readxl,
  tidyverse,
  glue,
  gt,
  here
  )

# handle function conflicts
conflicts_prefer(dplyr::filter) 
conflicts_prefer(dplyr::select)



  