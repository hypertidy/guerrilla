## Rebuild data/bathy.rda as a plain guerrilla grid.
##
## It used to be a serialized RasterLayer, which meant the dataset alone made
## raster a hard runtime requirement even after no function in the package used
## it. A grid is a list, so it depends on nothing.
library(raster)
load("data-raw/broke_bathy.RData")   ## a RasterLayer, from ETOPO2, see
                                     ## inst/examples/afternoon_exercise.Rmd
bathy <- list(dimension = c(ncol(broke), nrow(broke)),
              extent = as.vector(extent(broke))[1:4],
              crs = "EPSG:4326",
              values = as.integer(values(broke)))
class(bathy) <- "guerrilla_grid"
usethis::use_data(bathy, overwrite = TRUE)
