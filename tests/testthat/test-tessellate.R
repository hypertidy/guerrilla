set.seed(42)
xy <- cbind(runif(60), runif(60))
v <- sin(3 * xy[, 1]) + xy[, 2]
g <- grid_spec(xy, dimension = c(40, 30))

test_that("grid_voronoi is nearest neighbour", {
  skip_if_not_installed("geos")
  gv <- grid_voronoi(xy, v, g)
  rxy <- grid_xy(g)
  nn <- apply(rxy, 1, function(p) which.min((xy[, 1] - p[1])^2 + (xy[, 2] - p[2])^2))
  expect_equal(gv$values, v[nn])
})

test_that("grid_voronoi fills the whole grid, unlike barycentric", {
  skip_if_not_installed("geos")
  expect_false(anyNA(grid_voronoi(xy, v, g)$values))
  expect_true(anyNA(grid_barycentric(xy, v, g)$values))
})

test_that("a plane fit per Delaunay triangle is barycentric interpolation", {
  skip_if_not_installed("geos")
  bary <- grid_barycentric(xy, v, g)$values
  lm_fit <- grid_facet_lm(xy, v, g)$values
  both <- !is.na(bary) & !is.na(lm_fit)
  expect_gt(sum(both), 500)
  expect_equal(lm_fit[both], bary[both])
})

test_that("grid_voronoi is what the old facets(method = 'dirichlet') computed", {
  ## A Dirichlet tile holds exactly one point, so facets() fitted an
  ## intercept-only lm() per tile and predicted that point's value across it.
  ## That is nearest neighbour, the slow way.
  skip_if_not_installed("geos")
  skip_if_not_installed("spatstat.geom")
  set.seed(3)
  n <- 25
  px <- runif(n); py <- runif(n); pm <- rnorm(n)
  pp <- spatstat.geom::ppp(px, py,
                           window = spatstat.geom::owin(c(0, 1), c(0, 1)),
                           marks = pm)
  ## facets() reports its progress to the console; not this suite's job
  invisible(utils::capture.output(old <- as.data.frame(suppressWarnings(
    facets(pp, nx = 20, ny = 20, method = "dirichlet")))))
  nn <- apply(cbind(old$x, old$y), 1,
              function(q) which.min((px - q[1])^2 + (py - q[2])^2))
  expect_equal(old$marks, pm[nn])

  ## and grid_voronoi() gives that same answer under its right name
  gg <- grid_spec(dimension = c(20, 20), extent = c(0, 1, 0, 1))
  gxy <- grid_xy(gg)
  nn2 <- apply(gxy, 1, function(q) which.min((px - q[1])^2 + (py - q[2])^2))
  expect_equal(grid_voronoi(cbind(px, py), pm, gg)$values, pm[nn2])
})
