test_that("grid_spec covers the coordinates it is given", {
  xy <- cbind(c(0, 4, 10), c(0, 2, 5))
  g <- grid_spec(xy)
  expect_true(is_grid(g))
  expect_equal(g$extent, c(0, 10, 0, 5))
  expect_equal(g$dimension, c(60L, 50L))
  expect_null(g$crs)
  expect_null(g$values)
})

test_that("grid_spec reads two coordinates as coordinates, not as an extent", {
  ## raster::extent() reads a 2x2 matrix as an extent, which made
  ## defaultgrid(cbind(c(0, 10), c(0, 5))) fail with "min and max x are the same"
  expect_equal(grid_spec(cbind(c(0, 10), c(0, 5)))$extent, c(0, 10, 0, 5))
})

test_that("grid_spec validates", {
  expect_error(grid_spec(), "supply either")
  expect_error(grid_spec(extent = c(0, 1, 2)), "four finite numbers")
  expect_error(grid_spec(extent = c(1, 0, 0, 1)), "xmin < xmax")
  expect_error(grid_spec(extent = c(0, 1, 0, 1), dimension = c(0, 5)),
               "positive whole numbers")
})

test_that("pad expands the extent by a fraction of each range", {
  g <- grid_spec(extent = c(0, 10, 0, 5), pad = 0.1)
  expect_equal(g$extent, c(-1, 11, -0.5, 5.5))
})

test_that("a grid is a plain list", {
  g <- grid_spec(extent = c(0, 1, 0, 1))
  expect_type(unclass(g), "list")
  expect_named(unclass(g), c("dimension", "extent", "crs", "values"))
})

test_that("grid_xy agrees with vaster, and with raster's cell order", {
  g <- grid_spec(dimension = c(4, 3), extent = c(0, 8, 0, 6))
  expect_equal(grid_ncell(g), 12L)
  expect_equal(grid_res(g), c(2, 2))
  expect_equal(dim(g), c(3L, 4L))
  xy <- grid_xy(g)
  expect_equal(dim(xy), c(12L, 2L))
  ## cells run left to right along the top row first
  expect_equal(xy[1, ], c(1, 5))
  expect_equal(xy[4, ], c(7, 5))
  expect_equal(xy[12, ], c(7, 1))
  skip_if_not_installed("raster")
  r <- raster::raster(raster::extent(0, 8, 0, 6), ncols = 4, nrows = 3)
  expect_equal(xy, unname(raster::xyFromCell(r, seq_len(12))))
})

test_that("as.matrix puts the top row of the grid first", {
  g <- grid_spec(dimension = c(3, 2), extent = c(0, 3, 0, 2))
  g$values <- 1:6
  m <- as.matrix(g)
  expect_equal(dim(m), c(2L, 3L))
  expect_equal(m[1, ], 1:3)
  expect_equal(m[2, ], 4:6)
})

test_that("as.data.frame gives one row per cell", {
  g <- grid_spec(dimension = c(3, 2), extent = c(0, 3, 0, 2))
  g$values <- 1:6
  d <- as.data.frame(g)
  expect_equal(dim(d), c(6L, 3L))
  expect_named(d, c("x", "y", "value"))
  expect_equal(d$value, 1:6)
})

test_that("converters round trip the dimension, extent and values", {
  xy <- cbind(runif(60), runif(60))
  g <- suppressMessages(tri_fun(xy, xy[, 1]))

  skip_if_not_installed("raster")
  r <- as_raster(g)
  expect_equal(c(ncol(r), nrow(r)), g$dimension)
  expect_equal(as.vector(raster::extent(r))[1:4], g$extent)
  expect_equal(raster::values(r), g$values)
  expect_equal(as_grid(r)$extent, g$extent)

  skip_if_not_installed("terra")
  tr <- as_terra(g)
  expect_equal(c(terra::ncol(tr), terra::nrow(tr)), g$dimension)
  expect_equal(unname(terra::values(tr)[, 1]), g$values)
  expect_equal(as_grid(tr)$extent, g$extent)
})

test_that("as_gdalraster writes a grid GDAL can read back", {
  skip_if_not_installed("gdalraster")
  g <- grid_spec(dimension = c(4, 3), extent = c(0, 8, 0, 6))
  g$values <- as.numeric(1:12)
  ds <- as_gdalraster(g)
  on.exit(ds$close(), add = TRUE)
  expect_equal(c(ds$getRasterXSize(), ds$getRasterYSize()), g$dimension)
  ## GDAL's geotransform: top left corner, positive x step, negative y step
  gt <- ds$getGeoTransform()
  expect_equal(gt[1], g$extent[1])
  expect_equal(gt[4], g$extent[4])
  expect_equal(gt[2], grid_res(g)[1])
  expect_equal(gt[6], -grid_res(g)[2])
  expect_equal(ds$read(band = 1, xoff = 0, yoff = 0, xsize = 4, ysize = 3,
                       out_xsize = 4, out_ysize = 3), g$values)
})

test_that("collapse_duplicates combines repeated coordinates", {
  xy <- cbind(c(1, 2, 2, 3), c(1, 2, 2, 3))
  expect_message(d <- collapse_duplicates(xy, c(10, 20, 30, 40)),
                 "1 duplicated coordinate collapsed")
  expect_equal(nrow(d$xy), 3L)
  expect_equal(d$value, c(10, 25, 40))
  expect_silent(collapse_duplicates(xy, c(10, 20, 30, 40), quiet = TRUE))
  ## nothing to do
  xy2 <- cbind(1:3, 1:3)
  expect_silent(d2 <- collapse_duplicates(xy2, 1:3))
  expect_equal(d2$value, 1:3)
})

test_that("bathy is a grid that needs no raster package", {
  data("bathy", package = "guerrilla")
  expect_true(is_grid(bathy))
  expect_equal(bathy$dimension, c(1200L, 300L))
  expect_length(bathy$values, 360000L)
  expect_type(unclass(bathy), "list")
})
