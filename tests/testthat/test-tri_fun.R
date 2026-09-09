zero_extent <- raster::extent(0, ncol(volcano), 0, nrow(volcano))
r <- raster::setExtent(raster::raster(volcano), zero_extent)
set.seed(pi)
xy <- raster::sampleRandom(r, size = 150, xy = TRUE)[, 1:2, drop = FALSE]
v0 <- raster::extract(r, xy)

test_that("tri_fun works", {
  tri_est <- tri_fun(xy, v0)
  expect_s4_class(tri_est, "RasterLayer")
  expect_equal(dim(tri_est), c(50L, 60L, 1L))
  expect_true(sum(raster::values(tri_est), na.rm = TRUE) > 360000)
})

test_that("mesh_raster agrees with tri_fun on the same grid", {
  grid <- defaultgrid(xy)
  expect_equal(raster::values(mesh_raster(cbind(xy, v0), grid = grid)),
               raster::values(tri_fun(xy, v0, grid = grid)))
})

test_that("defaultgrid asserts no CRS unless it is given one", {
  expect_true(is.na(raster::crs(defaultgrid(xy), asText = TRUE)))
  expect_equal(raster::ncol(defaultgrid(xy)), 60L)
  expect_equal(raster::nrow(defaultgrid(xy)), 50L)
  expect_false(is.na(raster::crs(defaultgrid(xy, prj = "EPSG:4326"), asText = TRUE)))
})
