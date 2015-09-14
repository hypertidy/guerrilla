
#' Identify point-in-triangle by conversion to polygons
#'
#' @param tri list P n*2 coordinates and T matrix of n*3 indices defining triangles
#' @param pts 
#' @importFrom sp Polygon Polygons SpatialPolygons CRS proj4string over
tri_pip <- function(tri, pts) {
  ps <- lapply(split(tri$T, seq(nrow(tri$T))), function(x) Polygon(tri$P[c(x, x[1]), ]))
  sp <- lapply(seq_along(ps), function(x) Polygons(ps[x], x))
  spp <- SpatialPolygons(sp, proj4string = CRS(proj4string(pts)))
  over(pts, spp)
}


library(geometry)
library(sp)

## raw data
data(volcano)
vx <- seq(0, 1, length = nrow(volcano))
vy <- seq(0, 1, length = ncol(volcano))

## reconstruct volcano by bilinear interpolation within triangles
set.seed(10)
## scattered points from volcano domain
xy <- matrix(runif(548), ncol = 2)
xi <- findInterval(xy[,1], vx)
yi <- findInterval(xy[,2], vy)
v <- volcano[cbind(xi, yi)]

## as a raster
ras <- raster(list(x = vx, y = vy, z = volcano))

## triangulation (see also RTriangle, rgeos, spatstat, geometry, ...)
tri <- delaunayn(xy)
rxy <- coordinates(ras)

## triangle id for every point
pid <- tri_pip(list(T = tri, P = xy), SpatialPoints(rxy))
ok <- !is.na(pid)

## estimated value from interpolation
est <- rep(NA_real_, nrow(rxy))
for (i in which(ok)) {
  ## triangle points
  tripts <- xy[tri[pid[i], ], ]
  ## grid points inside the triangle
  rpts <- rxy[pid == pid[i] & !is.na(pid) , , drop = FALSE]
  wgts <- cart2bary(tripts, rpts)
  vals <- matrix(v[tri[pid[i], ]], ncol = 3, nrow = nrow(wgts), byrow = TRUE)
  est[pid == pid[i] & !is.na(pid)] <- rowSums(vals * wgts)
}

par(mfrow = c(2, 1))
plot(ras); contour(ras, add = TRUE)
plot(setValues(ras, est))
contour(setValues(ras, est), add = TRUE)
