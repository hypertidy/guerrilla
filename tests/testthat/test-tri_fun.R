context("test-tri_fun")

library(raster)
zero_extent <- raster::extent(0, ncol(volcano), 0, nrow(volcano))
r <- raster::setExtent(raster::raster(volcano), zero_extent)
set.seed(pi)
xy <- raster::sampleRandom(r, size = 150, xy = TRUE)[, 1:2, drop = FALSE]

test_that("tri_fun works", {
  tri_est <- tri_fun(xy, raster::extract(r, xy)) %>% expect_s4_class("RasterLayer")
  expect_equal(dim(tri_est), c(50L, 60L, 1L))
  expect_equivalent(sum(values(tri_est), na.rm = TRUE), 360501.3478)
})

test_that("tri_pip works", {
  tri <- geometry::delaunayn(xy)
  set.seed(1)
  p <- jitter(xy[c(1, 10, 20, 40, 40, 50, 60, 70, 80), ], 20)
  pp <- tri_pip(list(T = tri, P = xy), sp::SpatialPoints(p))
  expect_equal(pp, c(208L, 157L, 252L, 261L, 3L, 149L, 215L, 281L, 145L))
})
