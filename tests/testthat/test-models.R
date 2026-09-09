set.seed(12)
xy <- cbind(runif(80), runif(80))
val <- 3 * xy[, 1] - 2 * xy[, 2]
g <- grid_spec(xy, dimension = c(15, 15))

test_that("every method returns a grid of the size it was given", {
  skip_if_not_installed("fields")
  skip_if_not_installed("gstat")
  skip_if_not_installed("mgcv")
  for (f in list(grid_tps, grid_idw, grid_kriging, grid_gam)) {
    out <- suppressWarnings(f(xy, val, g))
    expect_true(is_grid(out))
    expect_equal(out$dimension, c(15L, 15L))
    expect_length(out$values, 225L)
    expect_false(anyNA(out$values))
  }
})

test_that("a thin plate spline reproduces a plane", {
  skip_if_not_installed("fields")
  out <- suppressWarnings(grid_tps(xy, val, g))
  gxy <- grid_xy(out)
  expect_equal(out$values, 3 * gxy[, 1] - 2 * gxy[, 2], tolerance = 1e-6)
})

test_that("standard errors come back, and are positive", {
  skip_if_not_installed("fields")
  skip_if_not_installed("mgcv")
  se <- suppressWarnings(grid_tps(xy, val, g, statistic = "se"))
  expect_true(all(se$values > 0))
  ## a gam on a plane with no residual variance has nothing to fit, so use
  ## data with some noise in it
  noisy <- val + stats::rnorm(length(val), sd = 0.1)
  se2 <- grid_gam(xy, noisy, g, statistic = "se")
  expect_true(all(se2$values > 0))
})

test_that("a bigger idp moves idw towards nearest neighbour", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("geos")
  nn <- grid_voronoi(xy, val, g)$values
  near <- grid_idw(xy, val, g, idp = 12)$values
  far <- grid_idw(xy, val, g, idp = 0.5)$values
  expect_lt(mean(abs(near - nn)), mean(abs(far - nn)))
})

test_that("kriging reports its own uncertainty, and refuses noise", {
  skip_if_not_installed("gstat")
  v <- suppressWarnings(grid_kriging(xy, val, g, statistic = "se"))
  expect_true(all(v$values >= 0))
  set.seed(9)
  p <- cbind(runif(120), runif(120))
  expect_error(suppressWarnings(grid_kriging(p, rnorm(120))),
               "no spatial structure")
})

test_that("gam takes a formula, and s(x) + s(y) is a different surface", {
  skip_if_not_installed("mgcv")
  v <- exp(-20 * ((xy[, 1] - 0.5)^2 + (xy[, 2] - 0.5)^2))
  iso <- grid_gam(xy, v, g)$values
  add <- grid_gam(xy, v, g, formula = value ~ s(x) + s(y))$values
  expect_gt(max(abs(iso - add)), 1e-6)
})

test_that("grid_smooth returns a square grid over the same extent", {
  skip_if_not_installed("fields")
  out <- grid_smooth(xy, val, grid_spec(xy, dimension = c(30, 10)))
  expect_equal(out$dimension, c(30L, 30L))
  expect_equal(out$extent, g$extent)
})

test_that("a crs on the input is carried through every method", {
  skip_if_not_installed("fields")
  skip_if_not_installed("wk")
  p <- wk::xyz(xy[, 1], xy[, 2], val, crs = "EPSG:4326")
  expect_equal(suppressWarnings(grid_tps(p))$crs, "EPSG:4326")
  expect_equal(grid_bin(p)$crs, "EPSG:4326")
})

test_that("statistic is checked", {
  expect_error(grid_tps(xy, val, g, statistic = "nope"), "should be one of")
})
