# Barycentric coordinates of points within a triangle

The weights that say how much of each corner a point is made of. They
sum to one, and they are all non-negative exactly when the point is
inside the triangle – which makes them a point-in-triangle test and an
interpolation rule at the same time.

## Usage

``` r
bary_weights(triangle, xy)
```

## Arguments

- triangle:

  three corners, as a 3 by 2 matrix

- xy:

  points to place within it, a two column matrix

## Value

A matrix with one row per point and three columns, one weight per corner
of `triangle`.

## Details

This is the whole of what `geometry::tsearch(bary = TRUE)` computes in
C, written out so it can be read. For a triangle with corners
\\(x_1,y_1)\\, \\(x_2,y_2)\\, \\(x_3,y_3)\\ and a point \\(x,y)\\, solve
for the weights by Cramer's rule on

\$\$w_1 (x_1,y_1) + w_2 (x_2,y_2) + w_3 (x_3,y_3) = (x,y), \quad w_1 +
w_2 + w_3 = 1\$\$

Estimating a value is then one line: the weighted sum of the corner
values.

## See also

[`find_triangle()`](https://hypertidy.github.io/guerrilla/reference/find_triangle.md),
which uses this to locate points, and
[`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md),
which uses it to interpolate.

## Examples

``` r
tri <- cbind(c(0, 1, 0), c(0, 0, 1))
## the corners themselves
bary_weights(tri, tri)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1

## the centroid is one third of each
bary_weights(tri, cbind(1/3, 1/3))
#>           [,1]      [,2]      [,3]
#> [1,] 0.3333333 0.3333333 0.3333333

## outside the triangle, a weight goes negative
bary_weights(tri, cbind(1, 1))
#>      [,1] [,2] [,3]
#> [1,]   -1    1    1
```
