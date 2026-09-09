set.seed(11)
xy <- cbind(runif(300), runif(300))
g <- grid_spec(xy, dimension = c(20, 20))

test_that("grid_bin puts values in the cells that contain them", {
  b <- grid_bin(xy, xy[, 1], g)
  expect_true(is_grid(b))
  expect_length(b$values, 400L)
  ## a binned x coordinate should be close to the cell's own x
  gx <- grid_xy(b)[, 1]
  ok <- !is.na(b$values)
  expect_lt(max(abs(b$values[ok] - gx[ok])), max(grid_res(b)))
})

test_that("fun decides what happens to two points in one cell", {
  n <- grid_bin(xy, xy[, 1], g, fun = length)
  expect_equal(sum(n$values, na.rm = TRUE), 300)
  first <- grid_bin(xy, xy[, 1], g, fun = function(x) x[1])
  mean_ <- grid_bin(xy, xy[, 1], g, fun = mean)
  ## same cells filled, different answers where cells hold more than one point
  expect_equal(is.na(first$values), is.na(mean_$values))
  expect_true(any(first$values != mean_$values, na.rm = TRUE))
})

test_that("empty cells are NA, and every point lands somewhere", {
  sparse <- grid_bin(xy, xy[, 1], grid_spec(xy, dimension = c(60, 60)))
  expect_true(sum(is.na(sparse$values)) > 300)
  n <- grid_bin(xy, xy[, 1], g, fun = length)
  expect_equal(sum(n$values, na.rm = TRUE), nrow(xy))
})
