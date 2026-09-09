## Every method here starts the same way: read the input, settle on a grid.
method_setup <- function(x, value, grid) {
  input <- xyz_input(x, value)
  grid <- if (is.null(grid)) {
    grid_spec(input$xy, crs = input$crs)
  } else {
    as_grid(grid)
  }
  list(xy = input$xy, value = input$value, grid = grid)
}

need_package <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(what, " needs the ", pkg, " package, which is not installed",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Thin plate spline interpolation
#'
#' Fit a thin plate spline to the points with [fields::Tps()] and evaluate it
#' at every cell.
#'
#' A thin plate spline is the surface that passes near the data while bending
#' as little as possible, with a smoothing parameter deciding the trade between
#' those two. It is the engine behind GDAL's `-tps` warping, so this is also
#' what happens when you georeference an image from ground control points.
#'
#' Unlike [grid_barycentric()] it fills the whole grid, including cells far
#' outside the data, where the answer is extrapolation and should be read as
#' such. `statistic = "se"` is how it tells you that: the standard error grows
#' with distance from the data, so the two surfaces together say both what the
#' method thinks and where it is guessing.
#'
#' `lon.lat` is the one place in this package where what the coordinates mean
#' changes the arithmetic. A spline bends in the plane of its coordinates, and
#' a degree of longitude is not a degree of latitude anywhere but the equator.
#' It is not guessed from the `crs`, because the guess would be wrong exactly
#' when it mattered; say what you have.
#'
#' @inheritParams grid_barycentric
#' @param statistic `"prediction"` for the fitted surface, `"se"` for its
#'   standard error
#' @param lon.lat the coordinates are longitude and latitude, so distances
#'   should be great circle ones; passed to [fields::Tps()]
#' @param ... passed to [fields::Tps()], for instance `lambda` or `m`
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(60), runif(60))
#' v <- sin(xy[, 1] * 6) + xy[, 2]
#' if (requireNamespace("fields", quietly = TRUE)) {
#'   plot(grid_tps(xy, v))
#'   ## and where it is guessing
#'   plot(grid_tps(xy, v, statistic = "se"))
#' }
grid_tps <- function(x, value = NULL, grid = NULL,
                     statistic = c("prediction", "se"),
                     lon.lat = FALSE, ...) {
  need_package("fields", "grid_tps()")
  statistic <- match.arg(statistic)
  s <- method_setup(x, value, grid)
  model <- fields::Tps(s$xy, s$value, lon.lat = lon.lat, ...)
  at <- grid_xy(s$grid)
  s$grid$values <- if (statistic == "se") {
    as.vector(fields::predictSE(model, at))
  } else {
    as.vector(stats::predict(model, at))
  }
  s$grid
}

#' Inverse distance weighted interpolation
#'
#' Estimate each cell as a weighted average of the input values, weighted by
#' one over distance to the power `idp`.
#'
#' There is no model here, only a rule, and that is the point of having it next
#' to the others. Raising `idp` makes the nearest point dominate, so the
#' surface tends to nearest neighbour ([grid_voronoi()]); lowering it flattens
#' towards the overall mean. Neither end is more correct than the other, and
#' nothing in the data tells you where to sit between them.
#'
#' The consequence is that this method cannot report a standard error, because
#' it never claimed to be estimating anything. [grid_kriging()] is the same
#' idea taken seriously: weights chosen from a fitted model of how the values
#' vary with distance, which then also says how uncertain each cell is.
#'
#' @inheritParams grid_barycentric
#' @param idp the inverse distance power
#' @param nmax use at most this many of the nearest points per cell
#' @param ... passed to [gstat::idw()]
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(50), runif(50))
#' if (requireNamespace("gstat", quietly = TRUE)) {
#'   op <- par(mfrow = c(1, 2))
#'   plot(grid_idw(xy, xy[, 1], idp = 0.5), main = "idp = 0.5")
#'   plot(grid_idw(xy, xy[, 1], idp = 8), main = "idp = 8")
#'   par(op)
#' }
grid_idw <- function(x, value = NULL, grid = NULL, idp = 2, nmax = Inf, ...) {
  need_package("gstat", "grid_idw()")
  s <- method_setup(x, value, grid)
  fit <- gstat::idw(value ~ 1, locations = ~ x + y,
                    data = model_frame(s), newdata = cell_frame(s$grid),
                    idp = idp, nmax = nmax, debug.level = 0, ...)
  s$grid$values <- fit$var1.pred
  s$grid
}

#' Kriging
#'
#' Fit a variogram to the data, then use it to weight the input values for
#' every cell.
#'
#' The variogram is a picture of how much two values differ, as a function of
#' how far apart they are. Kriging fits a curve to that picture and uses it to
#' choose the weights, so unlike [grid_idw()] the weighting is estimated from
#' the data rather than chosen.
#'
#' That also makes this the one method here that can refuse. If the values have
#' no spatial structure at these distances there is no curve to fit, the fit
#' comes back with a negative range, and this stops rather than kriging from
#' nonsense. Every other method in the package will happily interpolate pure
#' noise and hand you a picture of it.
#'
#' Because there is a fitted model, there is also a variance: `statistic = "se"`
#' returns the square root of the kriging variance, which is small near the data
#' and rises to the sill away from it. That surface is usually more informative
#' than the prediction, and it is the reason to reach for this method at all.
#'
#' `model` is passed to [gstat::fit.variogram()] as the starting point. The
#' default is a spherical model with the sill at the overall variance and the
#' range at a third of the distances the variogram covers. That is a starting
#' guess, not an analysis: a real one plots `gstat::variogram()` first and
#' chooses. If the fit does not converge, that is what it is telling you.
#'
#' @inheritParams grid_barycentric
#' @param model a `variogramModel` from [gstat::vgm()] to fit, or `NULL` for a
#'   spherical starting guess
#' @param statistic `"prediction"` for the kriged surface, `"se"` for the
#'   square root of the kriging variance
#' @param ... passed to [gstat::krige()]
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(80), runif(80))
#' v <- xy[, 1] * 3 + rnorm(80, sd = 0.1)
#' if (requireNamespace("gstat", quietly = TRUE)) {
#'   plot(grid_kriging(xy, v))
#'   plot(grid_kriging(xy, v, statistic = "se"))
#' }
grid_kriging <- function(x, value = NULL, grid = NULL, model = NULL,
                         statistic = c("prediction", "se"), ...) {
  need_package("gstat", "grid_kriging()")
  statistic <- match.arg(statistic)
  s <- method_setup(x, value, grid)
  d <- model_frame(s)
  vg <- gstat::variogram(value ~ 1, locations = ~ x + y, data = d)
  if (is.null(model)) {
    ## a starting guess, not an analysis: sill at the overall variance, range
    ## at a third of the distances the variogram actually covers
    model <- gstat::vgm(psill = stats::var(s$value), model = "Sph",
                        range = max(vg$dist) / 3, nugget = 0)
  }
  fitted <- gstat::fit.variogram(vg, model)
  if (any(fitted$range < 0) || any(fitted$psill < 0)) {
    stop("the variogram fit returned a model that cannot be used: ",
         "a negative range or sill.\n  Usually that means these values have no ",
         "spatial structure at these distances,\n  so there is nothing for ",
         "kriging to weight by. Look at\n",
         "    gstat::variogram(value ~ 1, ~ x + y, data)\n",
         "  and pass a starting 'model' from gstat::vgm() if you can see ",
         "structure in it.", call. = FALSE)
  }
  out <- gstat::krige(value ~ 1, locations = ~ x + y, data = d,
                      newdata = cell_frame(s$grid), model = fitted,
                      debug.level = 0, ...)
  s$grid$values <- if (statistic == "se") sqrt(out$var1.var) else out$var1.pred
  s$grid
}

#' Generalized additive model on the coordinates
#'
#' Fit a smooth of x and y with [mgcv::gam()] and predict it at every cell.
#'
#' The default formula is `value ~ s(x, y)`, one isotropic two dimensional
#' smooth. That matters: `mgcv`'s default basis for a two dimensional `s()` is a
#' thin plate regression spline, so this is [grid_tps()] with the number of
#' basis functions reduced and the smoothing parameter chosen by REML. The two
#' give visibly similar surfaces on the same data, and seeing that is worth more
#' than either surface on its own.
#'
#' `value ~ s(x) + s(y)` is the wrong model for a surface and a useful thing to
#' try: additive in x and y means every column of the grid has the same shape as
#' every other, so it cannot represent a feature that sits in one place. Pass it
#' as `formula` and look.
#'
#' @inheritParams grid_barycentric
#' @param formula a model formula in `value`, `x` and `y`
#' @param statistic `"prediction"` for the fitted surface, `"se"` for its
#'   standard error
#' @param ... passed to [mgcv::gam()]
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(200), runif(200))
#' v <- exp(-20 * ((xy[, 1] - 0.5)^2 + (xy[, 2] - 0.5)^2))
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   op <- par(mfrow = c(1, 2))
#'   plot(grid_gam(xy, v), main = "s(x, y)")
#'   ## additive in x and y cannot put a bump in one place
#'   plot(grid_gam(xy, v, formula = value ~ s(x) + s(y)), main = "s(x) + s(y)")
#'   par(op)
#' }
grid_gam <- function(x, value = NULL, grid = NULL, formula = value ~ s(x, y),
                     statistic = c("prediction", "se"), ...) {
  need_package("mgcv", "grid_gam()")
  statistic <- match.arg(statistic)
  s <- method_setup(x, value, grid)
  fit <- mgcv::gam(formula, data = model_frame(s), method = "REML", ...)
  pred <- stats::predict(fit, cell_frame(s$grid), se.fit = statistic == "se")
  s$grid$values <- as.vector(if (statistic == "se") pred$se.fit else pred)
  s$grid
}

#' Kernel smoothing on a grid
#'
#' Bin the points to a grid, then smooth that grid with a Gaussian kernel,
#' using [fields::image.smooth()].
#'
#' This is the two step method: rasterize first ([grid_bin()]), estimate second.
#' Everything else here works from the points directly, so this one is worth
#' having as the contrast -- the binning throws away where inside its cell each
#' point was, and no amount of smoothing afterwards gets that back.
#'
#' `theta` is the kernel bandwidth in coordinate units, and it does all the
#' work. There is no standard error, because there is no model.
#'
#' `fields::image.smooth()` wants a square grid, so this uses one of
#' `max(dimension)` cells a side and returns it; the result covers the same
#' extent but need not have the dimension you asked for.
#'
#' @inheritParams grid_barycentric
#' @param theta kernel bandwidth, in the units of the coordinates
#' @param ... passed to [fields::image.smooth()]
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(200), runif(200))
#' if (requireNamespace("fields", quietly = TRUE)) {
#'   plot(grid_smooth(xy, xy[, 1] + xy[, 2], theta = 0.1))
#' }
grid_smooth <- function(x, value = NULL, grid = NULL, theta = NULL, ...) {
  need_package("fields", "grid_smooth()")
  s <- method_setup(x, value, grid)
  n <- max(s$grid$dimension)
  if (is.null(theta)) theta <- max(grid_res(s$grid)) * 2
  im <- fields::as.image(s$value, x = s$xy, nx = n, ny = n)
  sm <- fields::image.smooth(im, theta = theta, ...)
  ## image.smooth() returns z[x, y] with both axes increasing; a grid runs
  ## top-down, so the columns come back reversed
  out <- grid_spec(dimension = c(n, n), extent = s$grid$extent,
                   crs = s$grid$crs)
  out$values <- as.vector(sm$z[, rev(seq_len(ncol(sm$z)))])
  out
}

## gstat and mgcv both take a plain data frame of x, y and value, and gstat
## takes the coordinates as a formula. No sp, no sf, no raster: the statistical
## engines never needed a spatial class, they needed coordinates.
model_frame <- function(s) {
  data.frame(x = s$xy[, 1L], y = s$xy[, 2L], value = s$value)
}

cell_frame <- function(grid) {
  xy <- grid_xy(grid)
  data.frame(x = xy[, 1L], y = xy[, 2L])
}
