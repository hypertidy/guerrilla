
##https://stat.ethz.ch/pipermail/r-sig-geo/2011-November/013525.html
facets <- function(X, nx, ny, x=NULL, y=NULL, na.v=0, method= c("dirichlet", "delaunay")){
  require(spatstat)
  method <- match.arg(method)
  if(method == "dirichlet")  lltes <- dirichlet(X)
  if(method == "delaunay")  lltes <- delaunay(X)
  
  if(is.null(x)){
    gri <-  gridcentres(X$window, nx=nx, ny=ny)
    gri.ppp <- ppp(gri$x,gri$y, window=X$window,
                   marks=rep(na.v,length(gri$x)))
  }
  if(!is.null(x)){
    gri.ppp<- ppp(x=x, y=y, window=X$window,
                  marks=rep(na.v, length(x)))
  }
  
  cat("\n","number of triangles =",
      length(lltes[[3]]),"\n\n")
  for(i in 1:length(lltes[[3]])){
    progressreport(i, length(lltes[[3]]))
    
    #grid points within the triangulation
    xoyo <- unmark(gri.ppp[lltes[[3]][[i]]])
    
    # original points defining the triangle
    xyz <- X[lltes[[3]][[i]]]
    # z values of the three points
    z<-xyz$marks
    mtrend <-with(xyz, lm(marks~x+y))
    suppressWarnings({
    grim <- predict(mtrend,
                    newdata=data.frame(x = xoyo$x, y=xoyo$y))
    })
    #assign interpolated values
    gri.ppp[lltes[[3]][[i]]]$marks <- grim
  }
  return(gri.ppp)
}
