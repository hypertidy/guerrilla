context("test-tri_fun")

library(raster)
zero_extent <- raster::extent(0, ncol(volcano), 0, nrow(volcano))
r <- raster::setExtent(raster::raster(volcano), zero_extent)
set.seed(pi)
xy <- raster::sampleRandom(r, size = 150, xy = TRUE)[, 1:2, drop = FALSE]

test_that("tri_fun works", {
  v0 <- raster::extract(r, xy)
  tri_est <- tri_fun(xy, v0) %>% expect_s4_class("RasterLayer")
  tri_est2 <- mesh_raster(cbind(xy, v0))
  expect_s4_class(tri_est2, "BasicRaster")
  expect_equal(dim(tri_est), c(50L, 60L, 1L))
  expect_true(sum(values(tri_est), na.rm = TRUE) > 360000)
})

test_that("tri_pip works", {
  tri <- geometry::delaunayn(xy)
  set.seed(1)
  p <- jitter(xy[c(1, 10, 20, 40, 40, 50, 60, 70, 80), ], 20)
  pp <- tri_pip(list(T = tri, P = xy), sp::SpatialPoints(p))
  expect_equal(length(pp), 9L)
})
