#' Coordinates and values, from whatever you have
#'
#' Every interpolation function here takes the same thing: places, and a value
#' at each place. This is what turns the various ways of expressing that into
#' one matrix of coordinates and one vector of values.
#'
#' A value can arrive two ways, and they are the same thing said differently:
#' as a separate `value` argument, or as the z of three dimensional
#' coordinates. Barycentric interpolation really is treating the value as a
#' height above the plane, so `wk::xyz()` input is not an abuse of notation
#' here, it is the notation. For kriging or a GAM it would be, which is why
#' `value` exists too.
#'
#' Objects from \pkg{wk} and \pkg{sf} go through [wk::wk_coords()], which also
#' carries their coordinate reference system across. A plain matrix or data
#' frame is read by position: first column x, second y, third z if present.
#' `wk::as_xy()` is deliberately not used for those, because it refuses to
#' guess at column names, and `Lon`/`Lat` is exactly what real data is called.
#'
#' @param x coordinates: a matrix, a data frame, or anything \pkg{wk} handles
#' @param value one value per coordinate, or `NULL` to use the z of `x`
#'
#' @return A list of `xy` (a two column matrix), `value`, and `crs`.
#' @export
#' @examples
#' xyz_input(cbind(1:3, 4:6), c(10, 20, 30))
#'
#' ## z is the value
#' xyz_input(cbind(1:3, 4:6, c(10, 20, 30)))
#'
#' ## and the crs comes along
#' xyz_input(wk::xyz(1:3, 4:6, c(10, 20, 30), crs = "EPSG:4326"))$crs
xyz_input <- function(x, value = NULL) {
  crs <- NULL
  if (inherits(x, c("wk_vctr", "wk_rcrd", "sfc", "sf"))) {
    if (!requireNamespace("wk", quietly = TRUE)) {
      stop("reading this object needs the wk package")
    }
    crs <- wk_crs_text(wk::wk_crs(x))
    co <- wk::wk_coords(x)
    xy <- cbind(co$x, co$y)
    z <- co$z
  } else {
    m <- if (is.data.frame(x)) as.matrix(x) else x
    if (!is.matrix(m) || !is.numeric(m)) {
      stop("expected a numeric matrix or data frame of coordinates")
    }
    if (ncol(m) < 2L) {
      stop("expected at least 2 numeric columns, got ", ncol(m))
    }
    xy <- m[, 1:2, drop = FALSE]
    z <- if (ncol(m) >= 3L) m[, 3L] else NULL
  }
  if (is.null(value)) {
    if (is.null(z) || all(is.na(z))) {
      stop("no values found: give a 'value' argument, or coordinates with a z")
    }
    value <- z
  }
  if (length(value) != nrow(xy)) {
    stop("'value' must have one element per coordinate: ",
         length(value), " values for ", nrow(xy), " coordinates")
  }
  list(xy = unname(xy), value = unname(value), crs = crs)
}

wk_crs_text <- function(x) {
  if (is.null(x) || inherits(x, "wk_crs_inherit")) return(NULL)
  if (is.character(x) && length(x) == 1L && nzchar(x)) return(x)
  out <- try(wk::wk_crs_proj_definition(x), silent = TRUE)
  if (inherits(out, "try-error") || !is.character(out) || !nzchar(out[1])) {
    return(NULL)
  }
  out[1]
}
