skip_if_no_gdal <- function() {
  skip_if_not_installed("sf")
  skip_if_not_installed("gdalraster")
}

set.seed(13)
xy <- cbind(runif(60), runif(60))
val <- 3 * xy[, 1] - 2 * xy[, 2]
g <- grid_spec(xy, dimension = c(30, 25))

test_that("GDAL's linear gridding is our barycentric interpolation", {
  skip_if_no_gdal()
  gdal <- grid_gdal(xy, val, g, "linear:radius=0.0")
  ours <- grid_barycentric(xy, val, g)
  expect_equal(is.na(gdal$values), is.na(ours$values))
  expect_equal(gdal$values, ours$values, tolerance = 1e-12)
})

test_that("GDAL's nearest gridding is our Voronoi fill", {
  skip_if_no_gdal()
  skip_if_not_installed("geos")
  gdal <- grid_gdal(xy, val, g, "nearest")
  expect_equal(gdal$values, grid_voronoi(xy, val, g)$values)
})

test_that("GDAL's default linear extrapolates beyond the hull", {
  skip_if_no_gdal()
  ## radius defaults to -1, an infinite search, so nothing is left out
  expect_false(anyNA(grid_gdal(xy, val, g)$values))
  expect_true(anyNA(grid_gdal(xy, val, g, "linear:radius=0.0")$values))
})

test_that("empty cells arrive as NA rather than as zero", {
  skip_if_no_gdal()
  out <- grid_gdal(xy, val, g, "linear:radius=0.0")
  expect_true(anyNA(out$values))
  expect_false(any(out$values == 0, na.rm = TRUE))
  ## a nodata already in the algorithm string is left alone, and lands in the
  ## band metadata, so gdalraster reads those cells back as NA too
  explicit <- grid_gdal(xy, val, g, "linear:radius=0.0:nodata=-9999")
  expect_equal(is.na(explicit$values), is.na(out$values))
  expect_equal(explicit$values, out$values, tolerance = 1e-12)
})

test_that("the grid and its crs come back unchanged", {
  skip_if_no_gdal()
  gc <- grid_spec(xy, dimension = c(30, 25), crs = "EPSG:4326")
  out <- grid_gdal(xy, val, gc, "nearest")
  expect_equal(out$dimension, c(30L, 25L))
  expect_equal(out$extent, gc$extent)
  expect_equal(out$crs, "EPSG:4326")
})
