
##https://stat.ethz.ch/pipermail/r-sig-geo/2011-November/013525.html
#' Interpolate by fitting a plane within each tessellation facet
#'
#' Tessellate a marked point pattern into Dirichlet (Voronoi) cells or Delaunay
#' triangles, fit a linear trend in x and y to the marks falling within each
#' facet, and predict that trend at a set of grid locations.
#'
#' This is deliberately the slow, explicit version of what [tri_fun()] does in
#' one vectorised pass: every facet is visited in an R loop and gets its own
#' `lm()` fit. It is here to show the mechanics, and because a per-facet plane
#' fit is a genuinely different estimator from barycentric interpolation when a
#' facet contains more than three points.
#'
#' @param X spatstat object
#' @param nx number of x coords
#' @param ny number of y coords
#' @param x option input x values
#' @param y optional input y values
#' @param na.v na value
#' @param method dirichlet or delaunay
#'
#' @return ppp object
#' @export
facets <- function(X, nx, ny, x=NULL, y=NULL, na.v=0, method= c("dirichlet", "delaunay")){

  method <- match.arg(method)
  if(method == "dirichlet")  lltes <- spatstat.geom::dirichlet(X)
  if(method == "delaunay")  lltes <- spatstat.geom::delaunay(X)
  
  if(is.null(x)){
    gri <-  spatstat.geom::gridcentres(X$window, nx=nx, ny=ny)
    gri.ppp <- spatstat.geom::ppp(gri$x,gri$y, window=X$window,
                   marks=rep(na.v,length(gri$x)))
  }
  if(!is.null(x)){
    gri.ppp<- spatstat.geom::ppp(x=x, y=y, window=X$window,
                  marks=rep(na.v, length(x)))
  }
  
  cat("\n","number of triangles =",
      length(lltes[[3]]),"\n\n")
  for(i in 1:length(lltes[[3]])){
    spatstat.geom::progressreport(i, length(lltes[[3]]))
    
    #grid points within the triangulation
    xoyo <- spatstat.geom::unmark(gri.ppp[lltes[[3]][[i]]])
    
    # original points defining the triangle
    xyz <- X[lltes[[3]][[i]]]
    # z values of the three points
    z<-xyz$marks
    mtrend <-with(xyz, lm(marks~x+y))
    suppressWarnings({
    grim <- stats::predict(mtrend,
                    newdata=data.frame(x = xoyo$x, y=xoyo$y))
    })
    #assign interpolated values
    gri.ppp[lltes[[3]][[i]]]$marks <- grim
  }
  return(gri.ppp)
}
