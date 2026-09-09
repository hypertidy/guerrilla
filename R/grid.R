#' A grid, as a plain list
#'
#' Describe a regular grid: how many cells across and down, what region of the
#' plane it covers, and optionally what coordinate system that plane is in.
#'
#' The value returned is a list. That is deliberate. Every raster in every
#' package is some version of these few numbers, and a reader who can print the
#' whole thing has actually been told what a raster is:
#'
#' - `dimension`, the number of columns and rows, in that order;
#' - `extent`, the outer edges as `xmin, xmax, ymin, ymax`;
#' - `crs`, a coordinate reference system, or `NULL` for none;
#' - `values`, one number per cell, or `NULL` for a grid that is only a target.
#'
#' `dimension` and `extent` are exactly the first two arguments of the
#' \pkg{vaster} functions, so `grid$dimension` and `grid$extent` can be handed
#' straight to [vaster::xy_from_cell()] and friends. Cells are numbered left to
#' right along the top row first, the same order raster and GDAL use.
#'
#' Nothing here assumes the plane is the Earth. `crs` stays `NULL` unless you
#' say otherwise.
#'
#' @param x coordinates to cover: anything with two or more columns, such as a
#'   matrix or data frame. Ignored when `extent` is given.
#' @param dimension number of columns and rows, in that order
#' @param extent outer edges `c(xmin, xmax, ymin, ymax)`; taken from `x` if not
#'   given
#' @param crs coordinate reference system, or `NULL` (the default) for none
#' @param pad fraction of each axis range to add on every side, so the outermost
#'   input coordinates do not sit exactly on the boundary. `0` by default.
#'
#' @return A `guerrilla_grid`, which is a list of `dimension`, `extent`, `crs`
#'   and `values`.
#' @export
#' @examples
#' xy <- cbind(c(0, 4, 10), c(0, 2, 5))
#' grid_spec(xy)
#'
#' ## it really is just a list
#' str(unclass(grid_spec(xy)))
#'
#' ## give the points some room
#' grid_spec(xy, pad = 0.05)$extent
#'
#' ## or skip the points entirely
#' grid_spec(dimension = c(4, 3), extent = c(0, 8, 0, 6))
grid_spec <- function(x = NULL, dimension = c(60L, 50L), extent = NULL,
                      crs = NULL, pad = 0) {
  if (is.null(extent)) {
    if (is.null(x)) stop("supply either 'x' (coordinates to cover) or 'extent'")
    xy <- as_xy(x)
    extent <- c(range(xy[, 1L], na.rm = TRUE), range(xy[, 2L], na.rm = TRUE))
  }
  extent <- as.numeric(extent)
  if (length(extent) != 4L || anyNA(extent)) {
    stop("'extent' must be four finite numbers: xmin, xmax, ymin, ymax")
  }
  if (pad != 0) extent <- pad_extent(extent, pad)
  if (extent[1L] >= extent[2L] || extent[3L] >= extent[4L]) {
    stop("'extent' must have xmin < xmax and ymin < ymax, got ",
         paste(format(extent), collapse = ", "))
  }
  dimension <- as.integer(round(dimension))
  if (length(dimension) != 2L || anyNA(dimension) || any(dimension < 1L)) {
    stop("'dimension' must be two positive whole numbers: ncol, nrow")
  }
  new_grid(dimension, extent, crs, NULL)
}

new_grid <- function(dimension, extent, crs = NULL, values = NULL) {
  structure(list(dimension = dimension, extent = extent,
                 crs = crs, values = values),
            class = "guerrilla_grid")
}

pad_extent <- function(extent, pad) {
  dx <- diff(extent[1:2]) * pad
  dy <- diff(extent[3:4]) * pad
  extent + c(-dx, dx, -dy, dy)
}

#' Is this a guerrilla grid?
#'
#' @param x any object
#' @return `TRUE` or `FALSE`
#' @export
#' @examples
#' is_grid(grid_spec(cbind(1:3, 1:3)))
#' is_grid(1:10)
is_grid <- function(x) inherits(x, "guerrilla_grid")

#' Coordinates of the cell centres
#'
#' The centre of every cell of `grid`, in cell order: left to right along the
#' top row, then the next row down.
#'
#' This is one call to [vaster::xy_from_cell()] with the grid's own first two
#' elements. It is the only thing the interpolation functions need to know
#' about the grid, which is why the grid can be a list.
#'
#' @param grid a `guerrilla_grid`
#' @return A two column matrix with one row per cell.
#' @export
#' @examples
#' grid_xy(grid_spec(dimension = c(3, 2), extent = c(0, 6, 0, 4)))
grid_xy <- function(grid) {
  grid <- as_grid(grid)
  xy <- vaster::xy_from_cell(grid$dimension, grid$extent,
                             seq_len(grid_ncell(grid)))
  unname(as.matrix(xy))
}

#' Number of cells in a grid
#'
#' @param grid a `guerrilla_grid`
#' @return A single integer, the product of the two dimensions.
#' @export
#' @examples
#' grid_ncell(grid_spec(dimension = c(3, 2), extent = c(0, 6, 0, 4)))
grid_ncell <- function(grid) {
  grid <- as_grid(grid)
  as.integer(prod(grid$dimension))
}

#' Cell size in each direction
#'
#' @param grid a `guerrilla_grid`
#' @return Two numbers, the x and y size of one cell.
#' @export
#' @examples
#' grid_res(grid_spec(dimension = c(4, 2), extent = c(0, 8, 0, 4)))
grid_res <- function(grid) {
  grid <- as_grid(grid)
  c(diff(grid$extent[1:2]) / grid$dimension[1L],
    diff(grid$extent[3:4]) / grid$dimension[2L])
}

#' @export
print.guerrilla_grid <- function(x, ...) {
  res <- grid_res(x)
  cat("<guerrilla grid>\n")
  cat(sprintf("dimension : %s  (ncol, nrow) = %i cells\n",
              paste(x$dimension, collapse = ", "), grid_ncell(x)))
  cat(sprintf("extent    : %s  (xmin, xmax, ymin, ymax)\n",
              paste(format(x$extent, trim = TRUE), collapse = ", ")))
  cat(sprintf("resolution: %s\n",
              paste(format(res, trim = TRUE), collapse = ", ")))
  cat(sprintf("crs       : %s\n", if (is.null(x$crs)) "<none>" else x$crs))
  if (is.null(x$values)) {
    cat("values    : <none>\n")
  } else {
    ok <- !is.na(x$values)
    cat(sprintf("values    : %i of %i cells%s\n", sum(ok), length(x$values),
                if (any(ok)) sprintf(", %s to %s",
                                     format(min(x$values[ok])),
                                     format(max(x$values[ok]))) else ""))
  }
  invisible(x)
}

#' @export
dim.guerrilla_grid <- function(x) c(x$dimension[2L], x$dimension[1L])

#' Read another package's raster as a grid
#'
#' Take the dimension, extent and coordinate system off a `RasterLayer` or a
#' terra `SpatRaster` and return them as a [grid_spec()]. Values are not
#' carried over; this is for describing a target to interpolate onto.
#'
#' It exists so that a grid you already have still works as a target, and so
#' that the claim these classes are all the same few numbers can be checked
#' rather than taken on trust.
#'
#' @param x a `guerrilla_grid`, a `RasterLayer`, or a `SpatRaster`
#' @return A `guerrilla_grid`.
#' @export
#' @examples
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   as_grid(raster::raster(raster::extent(0, 8, 0, 6), ncols = 4, nrows = 3))
#' }
as_grid <- function(x) {
  if (is_grid(x)) return(x)
  if (inherits(x, "BasicRaster")) {
    return(new_grid(c(ncol(x), nrow(x)),
                    as.vector(raster::extent(x))[c(1, 2, 3, 4)],
                    crs_text(raster::crs(x, asText = TRUE)), NULL))
  }
  if (inherits(x, "SpatRaster")) {
    e <- as.vector(terra::ext(x))
    return(new_grid(c(terra::ncol(x), terra::nrow(x)),
                    unname(e[c("xmin", "xmax", "ymin", "ymax")]),
                    crs_text(terra::crs(x)), NULL))
  }
  stop("cannot use an object of class '", paste(class(x), collapse = "/"),
       "' as a grid; see grid_spec()")
}

crs_text <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) NULL else as.character(x)
}

## Normalise anything two-column-ish to a plain numeric matrix of coordinates.
as_xy <- function(x, n = 2L) {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("expected a numeric matrix or data frame of coordinates")
  }
  if (ncol(x) < n) {
    stop("expected at least ", n, " numeric columns, got ", ncol(x))
  }
  unname(x[, seq_len(n), drop = FALSE])
}

#' Collapse coordinates that repeat
#'
#' Triangulation has nothing to say about two values at the same place, and
#' Qhull refuses outright. Real data does this constantly: a station occupied
#' twice, a transect that crosses its own track, coordinates rounded to a
#' printed precision.
#'
#' So decide explicitly. `collapse_duplicates()` reduces repeated coordinates to
#' one, combining their values with `fun`, and says how many it collapsed.
#'
#' @param xy coordinates, two columns
#' @param value one value per row of `xy`
#' @param fun function used to combine the values at a repeated coordinate
#' @param quiet do not report how many coordinates were collapsed
#' @param fun_label name of `fun` to use in that report
#'
#' @return A list of `xy` and `value`, with no repeated coordinate.
#' @export
#' @examples
#' xy <- cbind(c(1, 2, 2, 3), c(1, 2, 2, 3))
#' collapse_duplicates(xy, c(10, 20, 30, 40))
collapse_duplicates <- function(xy, value, fun = mean, quiet = FALSE,
                                fun_label = deparse(substitute(fun))) {
  xy <- as_xy(xy)
  dup <- duplicated(xy)
  if (!any(dup)) return(list(xy = xy, value = value))
  key <- paste(xy[, 1L], xy[, 2L], sep = "\r")
  keep <- !dup
  agg <- tapply(value, key, fun)
  if (!quiet) {
    message(sum(dup), " duplicated coordinate", if (sum(dup) > 1L) "s" else "",
            " collapsed with ", fun_label, "()")
  }
  list(xy = xy[keep, , drop = FALSE], value = as.vector(agg[key[keep]]))
}
