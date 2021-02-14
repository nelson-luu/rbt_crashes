# init.R

my_packages = c("shiny", "tidyverse", "lubridate", "plotly", "leaflet", "shinyWidgets", "shinyjqui")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))