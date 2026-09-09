test_that("bary_weights gives the corners weight one", {
  tri <- cbind(c(0, 1, 0), c(0, 0, 1))
  w <- bary_weights(tri, tri)
  expect_equal(unname(w), diag(3))
})

test_that("bary_weights sums to one, and the centroid is a third of each", {
  tri <- cbind(c(0, 4, 1), c(0, 1, 5))
  pts <- cbind(runif(20, 1, 2), runif(20, 1, 2))
  w <- bary_weights(tri, pts)
  expect_equal(unname(rowSums(w)), rep(1, 20))
  centroid <- cbind(mean(tri[, 1]), mean(tri[, 2]))
  expect_equal(unname(bary_weights(tri, centroid)[1, ]), rep(1/3, 3))
})

test_that("a weight goes negative outside the triangle", {
  tri <- cbind(c(0, 1, 0), c(0, 0, 1))
  expect_true(any(bary_weights(tri, cbind(1, 1)) < 0))
  expect_true(all(bary_weights(tri, cbind(0.2, 0.2)) >= 0))
})

test_that("bary_weights matches what geometry::tsearch computes in C", {
  set.seed(7)
  xy <- cbind(runif(40), runif(40))
  tri <- geometry::delaunayn(xy)
  pts <- cbind(runif(50), runif(50))
  hit <- geometry::tsearch(xy[, 1], xy[, 2], tri, pts[, 1], pts[, 2], bary = TRUE)
  for (i in which(!is.na(hit$idx))[1:10]) {
    ours <- bary_weights(xy[tri[hit$idx[i], ], , drop = FALSE],
                         pts[i, , drop = FALSE])
    expect_equal(unname(ours[1, ]), unname(hit$p[i, ]))
  }
})

test_that("find_triangle agrees with tsearch, including on the misses", {
  skip_if_not_installed("geos")
  set.seed(9)
  xy <- cbind(runif(40), runif(40))
  tri <- geometry::delaunayn(xy)
  pts <- cbind(runif(120, -0.2, 1.2), runif(120, -0.2, 1.2))
  ours <- find_triangle(xy, tri, pts)
  theirs <- geometry::tsearch(xy[, 1], xy[, 2], tri, pts[, 1], pts[, 2])
  expect_equal(is.na(ours), is.na(theirs))
  expect_equal(ours[!is.na(ours)], theirs[!is.na(theirs)])
})
