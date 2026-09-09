# Hand a grid to another package

Converters to the raster classes people actually have installed. Each
one is a handful of lines, because there is nothing in a grid that these
classes do not also hold: the same dimension, the same extent, the same
values in the same order.

## Usage

``` r
as_raster(grid)

as_terra(grid)

as_gdalraster(grid, filename = NULL)
```

## Arguments

- grid:

  a `guerrilla_grid`

- filename:

  file to write; defaults to a path in GDAL's `/vsimem`

## Value

An object of the corresponding class: a `RasterLayer`, a `SpatRaster`,
or an open `GDALRaster`.

## Details

`as_gdalraster()` has to write a file, since that is how GDAL works; by
default it writes to GDAL's in-memory filesystem. It returns the open
`GDALRaster` object, not the file name, and the caller has to `$close()`
it. The file name is `$getFilename()` on the returned object.

## Examples

``` r
xy <- cbind(runif(50), runif(50))
g <- grid_barycentric(xy, xy[, 1])
if (requireNamespace("terra", quietly = TRUE)) as_terra(g)
#> class       : SpatRaster
#> size        : 50, 60, 1  (nrow, ncol, nlyr)
#> resolution  : 0.01638855, 0.01954008  (x, y)
#> extent      : 0.007399441, 0.9907123, 0.02006522, 0.9970691  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> source(s)   : memory
#> name        :    lyr.1
#> min value   : 0.015594
#> max value   : 0.982518
```
