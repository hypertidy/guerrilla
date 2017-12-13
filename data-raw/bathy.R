load("data-raw/broke_bathy.RData")
library(raster)
bathy <- setValues(bathy, as.integer(values(bathy)))
usethis::use_data(bathy)
