
#' Identify point-in-triangle by conversion to polygons
#'
#' @param tri list P n*2 coordinates and T matrix of n*3 indices defining triangles
#' @param pts input points
#' @importFrom sp Polygon Polygons SpatialPolygons CRS proj4string over
tri_pip <- function(tri, pts) {
  ps <- lapply(split(tri$T, seq(nrow(tri$T))), function(x) Polygon(tri$P[c(x, x[1]), ]))
  sp <- lapply(seq_along(ps), function(x) Polygons(ps[x], x))
  spp <- SpatialPolygons(sp, proj4string = CRS(proj4string(pts)))
  over(pts, spp)
}



#' Title
#'
#' @param xy coordinates
#' @param value value to interpolate
#' @param grid grid to use
#' @param ... ignored
#'
#' @return raster
#' @export
tri_fun <- function(xy, value, grid = NULL, ...) {
  if (is.null(grid)) grid <- defaultgrid(xy)
  tri <- geometry::delaunayn(xy); 
  #tri <- rgl::triangulate(xy)
  #tri <- deldir::deldir(xy)
  #tri <- RTriangle::triangulate(pslg(P = xy))
  rxy <- sp::coordinates(grid)
  
  ## triangle id for every point
  pid <- tri_pip(list(T = tri, P = xy), sp::SpatialPoints(rxy))
  ok <- !is.na(pid)
  
  ## estimated value from interpolation
  est <- rep(NA_real_, nrow(rxy))
  for (i in which(ok)) {
    ## triangle points
    tripts <- xy[tri[pid[i], ], ]
    ## grid points inside the triangle
    rpts <- rxy[pid == pid[i] & !is.na(pid) , , drop = FALSE]
    wgts <- geometry::cart2bary(tripts, rpts)
    vals <- matrix(value[tri[pid[i], ]], ncol = 3, nrow = nrow(wgts), byrow = TRUE)
    est[pid == pid[i] & !is.na(pid)] <- rowSums(vals * wgts)
  }
  raster::setValues(grid, est)
}