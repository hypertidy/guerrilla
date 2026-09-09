set.seed(pi)
zero_extent <- c(0, ncol(volcano), 0, nrow(volcano))
gv <- grid_spec(dimension = c(ncol(volcano), nrow(volcano)), extent = zero_extent)
gv$values <- as.numeric(t(volcano[, ncol(volcano):1]))
xy <- grid_xy(gv)[sample(grid_ncell(gv), 150), , drop = FALSE]
v0 <- gv$values[vaster::cell_from_xy(gv$dimension, gv$extent, xy)]

test_that("grid_barycentric returns a grid with one value per cell", {
  g <- grid_barycentric(xy, v0)
  expect_true(is_grid(g))
  expect_equal(g$dimension, c(60L, 50L))
  expect_length(g$values, 3000L)
  expect_true(sum(!is.na(g$values)) > 2000)
})

test_that("cells outside the convex hull are NA", {
  g <- grid_barycentric(xy, v0, grid_spec(xy, pad = 0.5))
  expect_true(is.na(g$values[1]))
})

test_that("the two engines agree", {
  grid <- grid_spec(xy, dimension = c(30, 25))
  a <- grid_barycentric(xy, v0, grid)$values
  b <- grid_barycentric(xy, v0, grid, engine = "R")$values
  expect_equal(is.na(a), is.na(b))
  expect_equal(a, b)
})

test_that("mesh_raster and grid_barycentric agree cell for cell", {
  grid <- grid_spec(xy)
  expect_equal(mesh_raster(cbind(xy, v0), grid = grid)$values,
               grid_barycentric(xy, v0, grid = grid)$values)
})

test_that("interpolation reproduces a plane exactly", {
  set.seed(1)
  p <- cbind(runif(60), runif(60))
  z <- 3 * p[, 1] - 2 * p[, 2] + 1
  g <- grid_barycentric(p, z, grid_spec(p, dimension = c(40, 40)))
  gxy <- grid_xy(g)
  ok <- !is.na(g$values)
  expect_equal(g$values[ok], 3 * gxy[ok, 1] - 2 * gxy[ok, 2] + 1)
})

test_that("the value can be the z of the coordinates, or a separate argument", {
  expect_equal(grid_barycentric(cbind(xy, v0))$values,
               grid_barycentric(xy, v0)$values)
  expect_error(grid_barycentric(xy), "no values found")
  expect_error(grid_barycentric(xy, v0[-1]), "one element per coordinate")
})

test_that("wk input carries its crs across", {
  skip_if_not_installed("wk")
  p <- wk::xyz(xy[, 1], xy[, 2], v0, crs = "EPSG:4326")
  g <- grid_barycentric(p)
  expect_equal(g$crs, "EPSG:4326")
  expect_equal(g$values, grid_barycentric(xy, v0)$values)
})

test_that("duplicated coordinates are collapsed, and can be left alone", {
  p <- rbind(cbind(runif(40), runif(40)), c(0.5, 0.5), c(0.5, 0.5))
  z <- c(runif(40), 10, 20)
  expect_message(g <- grid_barycentric(p, z), "duplicated coordinate")
  expect_true(is_grid(g))
  expect_warning(grid_barycentric(p, z, duplicates = NULL),
                 "missing from triangulation")
})

test_that("collinear coordinates get an error that says so", {
  p <- cbind(1:10, 1:10)
  expect_error(suppressWarnings(grid_barycentric(p, seq_len(10))), "all on one line")
})

test_that("a raster or SpatRaster can still be used as the target", {
  skip_if_not_installed("raster")
  r <- raster::raster(raster::extent(0, 1, 0, 1), ncols = 10, nrows = 10)
  p <- cbind(runif(50), runif(50))
  g <- grid_barycentric(p, p[, 1], grid = r)
  expect_equal(g$dimension, c(10L, 10L))
})

test_that("tri_fun and defaultgrid are deprecated but still work", {
  expect_warning(g <- tri_fun(xy, v0), "deprecated")
  expect_true(is_grid(g))
  expect_warning(g2 <- defaultgrid(xy), "deprecated")
  expect_equal(g2$dimension, c(60L, 50L))
})

test_that("mesh_raster takes a mesh3d of triangles", {
  set.seed(6)
  p <- cbind(runif(60), runif(60))
  z <- 3 * p[, 1] - 2 * p[, 2] + 1
  ## a mesh3d is vertices in vb (4 rows, homogeneous) and triangle index in it
  mesh <- structure(list(vb = rbind(t(cbind(p, z)), 1),
                         it = t(geometry::delaunayn(p)),
                         primitivetypes = "triangle"),
                    class = c("mesh3d", "shape3d"))
  grid <- grid_spec(p, dimension = c(30, 30))
  g <- mesh_raster(mesh, grid = grid)
  expect_true(is_grid(g))
  ## the mesh is the same triangulation, so the surface is the same one
  expect_equal(g$values, grid_barycentric(p, z, grid = grid)$values)
  ## and it is still the plane the values came from
  gxy <- grid_xy(g)
  ok <- !is.na(g$values)
  expect_equal(g$values[ok], 3 * gxy[ok, 1] - 2 * gxy[ok, 2] + 1)
})

test_that("mesh_raster makes its own grid from a mesh3d", {
  set.seed(7)
  p <- cbind(runif(40) * 10, runif(40))
  mesh <- structure(list(vb = rbind(t(cbind(p, p[, 1])), 1),
                         it = t(geometry::delaunayn(p)),
                         primitivetypes = "triangle"),
                    class = c("mesh3d", "shape3d"))
  g <- mesh_raster(mesh, n = 40)
  expect_equal(g$dimension[1L], 40L)
  ## squarish cells: the extent is 10 wide and 1 tall, so the grid should be too
  expect_equal(unname(diff(grid_res(g))), 0, tolerance = 0.05)
})

test_that("coordinates of any magnitude work", {
  ## geometry::tsearch() builds a quadtree and fails on some coordinate ranges;
  ## this is one, and the interpolation normalises first so it never arises
  d <- readxl::read_excel(system.file("extdata", "BW-Zooplankton_env.xls",
                                      package = "guerrilla", mustWork = TRUE))
  ll <- as.matrix(d[c("Lon", "Lat")])
  big <- cbind((ll[, 1] - mean(ll[, 1])) * 1000, (ll[, 2] - mean(ll[, 2])) * 1000)
  grid <- grid_spec(big, dimension = c(50, 40))
  ## the raw call is what fails
  expect_error(geometry::tsearch(big[, 1], big[, 2], geometry::delaunayn(big),
                                 grid_xy(grid)[, 1], grid_xy(grid)[, 2],
                                 bary = TRUE),
               "QuadTree")
  ## and this does not
  g <- grid_barycentric(big, d$temp, grid)
  expect_true(sum(!is.na(g$values)) > 1000)
  ## same surface as the unscaled data, which is the reason normalising is safe
  small <- grid_barycentric(ll, d$temp, grid_spec(ll, dimension = c(50, 40)))
  expect_equal(is.na(g$values), is.na(small$values))
  expect_equal(g$values, small$values, tolerance = 1e-10)
})
