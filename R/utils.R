#' Create a target grid for interpolation
#'
#' Superseded by [grid_spec()], which returns a plain list rather than a
#' `RasterLayer`, and which takes its size as `dimension = c(ncol, nrow)`.
#'
#' @param xy coordinates
#' @param ncols number of columns
#' @param nrows number of rows
#' @param prj projection metadata, `NA` (the default) for none
#'
#' @return A `guerrilla_grid`, from [grid_spec()].
#' @export
#' @keywords internal
#' @examples
#' xy <- cbind(c(0, 4, 10), c(0, 2, 5))
#' grid_spec(xy)  ## use this instead
defaultgrid <- function(xy, ncols = 60, nrows = 50, prj = NA) {
  .Deprecated("grid_spec")
  grid_spec(xy, dimension = c(ncols, nrows),
            crs = if (length(prj) == 1L && !is.na(prj)) prj else NULL)
}
