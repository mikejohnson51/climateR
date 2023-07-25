cities = data.frame(city = c("Fort Collins, CO", "Durham, NC", "Raleigh, NC"),
               lat = c(40.544100, 35.994034 , 35.7796),
               lng = c(-105.110130, -78.898621, -78.6382)) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

saveRDS(cities, "tests/data/cities.rds")

