d <- readxl::read_excel(system.file("extdata", "BW-Zooplankton_env.xls", package= "guerrilla", mustWork = TRUE))
library(spatstat.geom)

gridToOwin <- function(x) {
  owin(x$extent[1:2], x$extent[3:4])
}

xname <- "Lon"
yname <- "Lat"
varname <- "temp"
rbase <- grid_spec(as.matrix(d[c(xname, yname)]))
pObj <- ppp(d[[xname]], d[[yname]], window = gridToOwin(rbase), marks = d[[varname]])




## facets() reports its progress to the console, which is not this suite's job
quiet_facets <- function(...) {
  out <- NULL
  invisible(utils::capture.output(out <- facets(...)))
  out
}

test_that("facets works", {
  fcs.dir <- quiet_facets(pObj, nx = rbase$dimension[1], ny = rbase$dimension[2], method = "dirichlet")
  fcs.del <- quiet_facets(pObj, nx = rbase$dimension[1], ny = rbase$dimension[2], method = "delaunay")
  a <- quiet_facets(pObj, 20, 30)
  b <- quiet_facets(pObj, 20, 30, x = seq(30, 70, by = 5), y = seq(-67, -62, length.out = 9))
  expect_s3_class(fcs.dir, "ppp")
  expect_s3_class(fcs.del, "ppp")
  expect_s3_class(a, "ppp")
})
