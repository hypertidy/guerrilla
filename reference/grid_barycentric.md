# Interpolate to a grid across a triangulation

Triangulate the input coordinates, then estimate a value at every cell
of a target grid from the barycentric coordinates of the cell centre
within the triangle that contains it.

## Usage

``` r
grid_barycentric(
  x,
  value = NULL,
  grid = NULL,
  duplicates = mean,
  engine = c("geometry", "R"),
  ...
)
```

## Arguments

- x:

  coordinates, or coordinates carrying their value as z; see
  [`xyz_input()`](https://hypertidy.github.io/guerrilla/reference/xyz_input.md)

- value:

  one value per coordinate, or `NULL` to use the z of `x`

- grid:

  a
  [`grid_spec()`](https://hypertidy.github.io/guerrilla/reference/grid_spec.md)
  to interpolate onto, or `NULL` for a default one

- duplicates:

  function used to combine values at repeated coordinates, or `NULL` to
  leave them alone

- engine:

  `"geometry"` for the fast path, `"R"` for the readable one

- ...:

  ignored

## Value

A `guerrilla_grid` with values.

## Details

Within a triangle the surface is the plane through its three corner
values, so the result is continuous, passes exactly through the input
values, and invents nothing beyond them. This is what MATLAB calls
`griddata(method = "linear")` and what GDAL's `gdal_grid` calls
`linear`.

Cells outside the convex hull of the points are in no triangle, and are
left `NA`. Points sharing a coordinate are combined first, because a
triangulation cannot hold two values in one place; see
[`collapse_duplicates()`](https://hypertidy.github.io/guerrilla/reference/collapse_duplicates.md).

## Two engines

`engine = "geometry"` locates the cells and computes their weights in
one pass of
[`geometry::tsearch()`](https://rdrr.io/pkg/geometry/man/tsearch.html),
in C. It is the one to use.

`engine = "R"` does the same work in the open:
[`find_triangle()`](https://hypertidy.github.io/guerrilla/reference/find_triangle.md)
to locate each cell,
[`bary_weights()`](https://hypertidy.github.io/guerrilla/reference/bary_weights.md)
to weight it. It is slower, and it is the point of this package – the
two agree to floating point, and the second one can be read.

## Examples

``` r
xy <- cbind(runif(100), runif(100))
grid_barycentric(xy, xy[, 1] + xy[, 2])
#> <guerrilla grid>
#> dimension : 60, 50  (ncol, nrow) = 3000 cells
#> extent    : 0.004496308, 0.999652457, 0.017015688, 0.983525408  (xmin, xmax, ymin, ymax)
#> resolution: 0.01658594, 0.01933019
#> crs       : <none>
#> values    : 2855 of 3000 cells, 0.125144 to 1.896132

## the value can be the z of the coordinates
grid_barycentric(cbind(xy, xy[, 1]))
#> <guerrilla grid>
#> dimension : 60, 50  (ncol, nrow) = 3000 cells
#> extent    : 0.004496308, 0.999652457, 0.017015688, 0.983525408  (xmin, xmax, ymin, ymax)
#> resolution: 0.01658594, 0.01933019
#> crs       : <none>
#> values    : 2855 of 3000 cells, 0.01278928 to 0.9913595

## the two engines agree
g1 <- grid_barycentric(xy, xy[, 1], grid_spec(xy, dimension = c(20, 20)))
g2 <- grid_barycentric(xy, xy[, 1], grid_spec(xy, dimension = c(20, 20)),
                       engine = "R")
max(abs(g1$values - g2$values), na.rm = TRUE)
#> [1] 2.220446e-16
```
