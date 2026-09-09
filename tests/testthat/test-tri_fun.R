set.seed(pi)
zero_extent <- c(0, ncol(volcano), 0, nrow(volcano))
gv <- grid_spec(dimension = c(ncol(volcano), nrow(volcano)), extent = zero_extent)
gv$values <- as.numeric(t(volcano[, ncol(volcano):1]))
xy <- grid_xy(gv)[sample(grid_ncell(gv), 150), , drop = FALSE]
v0 <- gv$values[vaster::cell_from_xy(gv$dimension, gv$extent, xy)]

test_that("tri_fun returns a grid with one value per cell", {
  g <- tri_fun(xy, v0)
  expect_true(is_grid(g))
  expect_equal(g$dimension, c(60L, 50L))
  expect_length(g$values, 3000L)
  expect_true(sum(!is.na(g$values)) > 2000)
})

test_that("cells outside the convex hull are NA", {
  g <- tri_fun(xy, v0, grid_spec(xy, pad = 0.5))
  expect_true(anyNA(g$values))
  ## and the corners of a generously padded grid are certainly outside
  expect_true(is.na(g$values[1]))
})

test_that("mesh_raster and tri_fun agree cell for cell", {
  grid <- grid_spec(xy)
  expect_equal(mesh_raster(cbind(xy, v0), grid = grid)$values,
               tri_fun(xy, v0, grid = grid)$values)
})

test_that("interpolation reproduces the input values at the input points", {
  ## a plane through the data is reproduced exactly by linear interpolation
  set.seed(1)
  p <- cbind(runif(60), runif(60))
  z <- 3 * p[, 1] - 2 * p[, 2] + 1
  g <- tri_fun(p, z, grid_spec(p, dimension = c(40, 40)))
  gxy <- grid_xy(g)
  ok <- !is.na(g$values)
  expect_equal(g$values[ok], 3 * gxy[ok, 1] - 2 * gxy[ok, 2] + 1)
})

test_that("value must match xy", {
  expect_error(tri_fun(xy, v0[-1]), "one element per row")
})

test_that("duplicated coordinates are collapsed, and can be left alone", {
  p <- rbind(cbind(runif(40), runif(40)), c(0.5, 0.5), c(0.5, 0.5))
  z <- c(runif(40), 10, 20)
  expect_message(g <- tri_fun(p, z), "duplicated coordinate")
  expect_true(is_grid(g))
  ## left alone, Qhull quietly drops them instead, which is what the
  ## 'duplicates' default exists to avoid
  expect_warning(tri_fun(p, z, duplicates = NULL), "missing from triangulation")
})

test_that("collinear coordinates get an error that says so", {
  p <- cbind(1:10, 1:10)
  expect_error(suppressWarnings(tri_fun(p, seq_len(10))), "all on one line")
})

test_that("a raster or SpatRaster can still be used as the target", {
  skip_if_not_installed("raster")
  r <- raster::raster(raster::extent(0, 1, 0, 1), ncols = 10, nrows = 10)
  p <- cbind(runif(50), runif(50))
  g <- tri_fun(p, p[, 1], grid = r)
  expect_true(is_grid(g))
  expect_equal(g$dimension, c(10L, 10L))
})

test_that("defaultgrid is deprecated but still works", {
  expect_warning(g <- defaultgrid(xy), "deprecated")
  expect_true(is_grid(g))
  expect_equal(g$dimension, c(60L, 50L))
})
