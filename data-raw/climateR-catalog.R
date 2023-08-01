library(usethis); library(arrow)

catalog = read_live_catalog()

use_data(catalog, overwrite = TRUE)


load("data/params.rda")
catalog = params

r2 = load("data/catalog.rda")
