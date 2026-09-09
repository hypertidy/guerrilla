# Which triangle is each point in?

For every point, the row of `triangles` whose triangle contains it, or
`NA` for a point in none of them.

## Usage

``` r
find_triangle(xy, triangles, points)
```

## Arguments

- xy:

  vertex coordinates, a two column matrix

- triangles:

  one triangle per row, as three indices into `xy`

- points:

  points to locate, a two column matrix

## Value

An integer vector with one element per row of `points`.

## Details

This is what the old `tri_pip()` did by building one sp polygon per
triangle and calling `over()`. GEOS does the same job with a spatial
index it builds for you:
[`geos::geos_intersects_matrix()`](https://paleolimbot.github.io/geos/reference/geos_disjoint_matrix.html)
puts the triangles in an STRtree, so each point is only tested against
the few whose bounding boxes it falls in. The exact containment test is
then
[`bary_weights()`](https://hypertidy.github.io/guerrilla/reference/bary_weights.md),
in R.

## Examples

``` r
xy <- cbind(c(0, 1, 1, 0), c(0, 0, 1, 1))
triangles <- rbind(c(1, 2, 3), c(1, 3, 4))
find_triangle(xy, triangles, cbind(c(0.9, 0.1, 5), c(0.5, 0.9, 5)))
#> [1]  1  2 NA
```
