library(usethis); library(climateR)

catalog = read_live_catalog()

use_data(catalog, overwrite = TRUE)
