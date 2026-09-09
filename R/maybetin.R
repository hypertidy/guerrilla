
##https://stat.ethz.ch/pipermail/r-sig-geo/2011-November/013525.html
#' Interpolate by fitting a plane within each tessellation facet
#'
#' Superseded. Use [grid_facet_lm()] for the Delaunay case and [grid_voronoi()]
#' for the Dirichlet one, both of which take the same arguments as everything
#' else here and return a grid rather than a point pattern.
#'
#' Tessellate a marked point pattern into Dirichlet (Voronoi) cells or Delaunay
#' triangles, fit a linear trend in x and y to the marks falling within each
#' facet, and predict that trend at a set of grid locations.
#'
#' Worth knowing what the two methods were: a Dirichlet tile contains exactly
#' one point, so `method = "dirichlet"` fits an intercept and nothing else, and
#' predicts that one point's value across its whole tile. It is nearest
#' neighbour, by way of a linear model per tile in an R loop. [grid_voronoi()]
#' is the same numbers, and says so.
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
#' @keywords internal
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
