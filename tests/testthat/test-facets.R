context("test-facets")


d <- readxl::read_excel(system.file("extdata", "BW-Zooplankton_env.xls", package= "guerrilla", mustWork = TRUE))
library(spatstat.geom)

library(raster)
rasterToOwin <- function(x) {
  owin(c(xmin(x), xmax(x)), c(ymin(x), ymax(x)))
}

xname <- "Lon"
yname <- "Lat"
varname <- "temp"
rbase <- defaultgrid(as.matrix(d[c(xname, yname)]))
pObj <- ppp(d[[xname]], d[[yname]], window = rasterToOwin(rbase), marks = d[[varname]])




test_that("facets works", {
  fcs.dir <- facets(pObj, nx = ncol(rbase) , ny = nrow(rbase), method = "dirichlet")
  fcs.del <- facets(pObj, nx = ncol(rbase) , ny = nrow(rbase), method = "delaunay")
  a <- facets(pObj, 20, 30)
  b <- facets(pObj, 20, 30, x = seq(30, 70, by = 5), y = seq(-67, -62, length.out = 9))
  expect_s3_class(fcs.dir, "ppp")
  expect_s3_class(fcs.del, "ppp")
  expect_s3_class(a, "ppp")
})
