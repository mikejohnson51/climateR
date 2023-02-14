library(usethis); library(arrow)

params = read_parquet(paste0(getwd(), "-catalogs/docs/catalog.parquet"))

use_data(params,overwrite = TRUE)



