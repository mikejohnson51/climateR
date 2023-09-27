library(usethis); library(arrow)

catalog = read_live_catalog()

use_data(catalog, overwrite = TRUE)
