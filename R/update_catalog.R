#' @title Read Live Catalog from Github release
#' @description
#' Every month, our data catalog is refreshed. This function reads the most current catalog from the Github release.
#' @param url URL to read
#' @return data.frame
#' @export
#' @family catalog

read_live_catalog = function(url = 'https://github.com/mikejohnson51/climateR-catalogs/releases/latest/download/catalog.parquet') {
  read_parquet(url)
}


