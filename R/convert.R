#' Turn a grid into a matrix
#'
#' The values as a matrix, one row per grid row, with the top row of the grid
#' first. This is the arrangement `image()` and `raster` both think in, and it
#' is where most orientation bugs come from, so it gets its own function.
#'
#' @param x a `guerrilla_grid`
#' @param ... ignored
#' @return A matrix with `nrow` rows and `ncol` columns.
#' @export
#' @examples
#' g <- grid_spec(dimension = c(3, 2), extent = c(0, 3, 0, 2))
#' g$values <- 1:6
#' as.matrix(g)
as.matrix.guerrilla_grid <- function(x, ...) {
  if (is.null(x$values)) stop("this grid has no values")
  matrix(x$values, nrow = x$dimension[2L], ncol = x$dimension[1L], byrow = TRUE)
}

#' Turn a grid into a data frame
#'
#' One row per cell, with the cell centre coordinates and the value.
#'
#' @param x a `guerrilla_grid`
#' @param ... ignored
#' @param row.names,optional ignored, for consistency with the generic
#' @return A data frame of `x`, `y` and `value`.
#' @export
#' @examples
#' g <- grid_spec(dimension = c(3, 2), extent = c(0, 3, 0, 2))
#' g$values <- 1:6
#' head(as.data.frame(g))
as.data.frame.guerrilla_grid <- function(x, row.names = NULL, optional = FALSE,
                                         ...) {
  xy <- grid_xy(x)
  data.frame(x = xy[, 1L], y = xy[, 2L],
             value = if (is.null(x$values)) NA_real_ else x$values)
}

#' Plot a grid
#'
#' Draw the grid with [graphics::image()]. No raster class is involved, which
#' is the point: a grid is a dimension, an extent and a vector of values, and
#' that is enough to draw it.
#'
#' @param x a `guerrilla_grid` with values
#' @param col colours
#' @param asp aspect ratio; `1` by default, or use `NA` to fill the device
#' @param ... passed to [graphics::image()]
#' @return `x`, invisibly.
#' @export
#' @examples
#' xy <- cbind(runif(50), runif(50))
#' g <- grid_barycentric(xy, xy[, 1] + xy[, 2])
#' plot(g)
plot.guerrilla_grid <- function(x, col = grDevices::hcl.colors(24, "YlGnBu"),
                                asp = 1, ...) {
  if (is.null(x$values)) stop("this grid has no values to plot")
  m <- as.matrix(x)
  ## image() wants both axes increasing and z[x, y], so flip the rows (which
  ## run top-down) and transpose.
  z <- t(m[rev(seq_len(nrow(m))), , drop = FALSE])
  graphics::image(x = seq(x$extent[1L], x$extent[2L], length.out = nrow(z) + 1L),
                  y = seq(x$extent[3L], x$extent[4L], length.out = ncol(z) + 1L),
                  z = z, col = col, asp = asp, xlab = "", ylab = "", ...)
  invisible(x)
}

#' Hand a grid to another package
#'
#' Converters to the raster classes people actually have installed. Each one is
#' a handful of lines, because there is nothing in a grid that these classes do
#' not also hold: the same dimension, the same extent, the same values in the
#' same order.
#'
#' `as_gdalraster()` has to write a file, since that is how GDAL works; by
#' default it writes to GDAL's in-memory filesystem.
#'
#' @param grid a `guerrilla_grid`
#' @param filename file to write; defaults to a path in GDAL's `/vsimem`
#' @return An object of the corresponding class.
#' @name converters
#' @examples
#' xy <- cbind(runif(50), runif(50))
#' g <- grid_barycentric(xy, xy[, 1])
#' if (requireNamespace("terra", quietly = TRUE)) as_terra(g)
NULL

#' @rdname converters
#' @export
as_raster <- function(grid) {
  stopifnot(is_grid(grid))
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("as_raster() needs the raster package")
  }
  r <- raster::raster(raster::extent(grid$extent),
                      ncols = grid$dimension[1L], nrows = grid$dimension[2L],
                      crs = if (is.null(grid$crs)) NA else grid$crs)
  if (!is.null(grid$values)) r <- raster::setValues(r, grid$values)
  r
}

#' @rdname converters
#' @export
as_terra <- function(grid) {
  stopifnot(is_grid(grid))
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("as_terra() needs the terra package")
  }
  r <- terra::rast(nrows = grid$dimension[2L], ncols = grid$dimension[1L],
                   xmin = grid$extent[1L], xmax = grid$extent[2L],
                   ymin = grid$extent[3L], ymax = grid$extent[4L],
                   crs = if (is.null(grid$crs)) "" else grid$crs)
  if (!is.null(grid$values)) terra::values(r) <- grid$values
  r
}

#' @rdname converters
#' @export
as_gdalraster <- function(grid, filename = NULL) {
  stopifnot(is_grid(grid))
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop("as_gdalraster() needs the gdalraster package")
  }
  if (is.null(filename)) {
    filename <- sprintf("/vsimem/guerrilla_%s.tif",
                        paste(sample(c(letters, 0:9), 8L, TRUE), collapse = ""))
  }
  invisible(gdalraster::create(format = "GTiff", dst_filename = filename,
                     xsize = grid$dimension[1L], ysize = grid$dimension[2L],
                              nbands = 1L, dataType = "Float64"))
  ds <- methods::new(gdalraster::GDALRaster, filename, read_only = FALSE)
  ## GDAL's geotransform is origin at the top left, with a negative y step.
  res <- grid_res(grid)
  ds$setGeoTransform(c(grid$extent[1L], res[1L], 0,
                       grid$extent[4L], 0, -res[2L]))
  if (!is.null(grid$crs)) try(ds$setProjection(grid$crs), silent = TRUE)
  if (!is.null(grid$values)) {
    ds$write(band = 1L, xoff = 0L, yoff = 0L,
             xsize = grid$dimension[1L], ysize = grid$dimension[2L],
             rasterData = as.numeric(grid$values))
  }
  ds
}
