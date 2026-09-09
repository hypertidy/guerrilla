# Inverse distance weighted interpolation

Estimate each cell as a weighted average of the input values, weighted
by one over distance to the power `idp`.

## Usage

``` r
grid_idw(x, value = NULL, grid = NULL, idp = 2, nmax = Inf, ...)
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

- idp:

  the inverse distance power

- nmax:

  use at most this many of the nearest points per cell

- ...:

  passed to
  [`gstat::idw()`](https://r-spatial.github.io/gstat/reference/krige.html)

## Value

A `guerrilla_grid` with values.

## Details

There is no model here, only a rule, and that is the point of having it
next to the others. Raising `idp` makes the nearest point dominate, so
the surface tends to nearest neighbour
([`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md));
lowering it flattens towards the overall mean. Neither end is more
correct than the other, and nothing in the data tells you where to sit
between them.

The consequence is that this method cannot report a standard error,
because it never claimed to be estimating anything.
[`grid_kriging()`](https://hypertidy.github.io/guerrilla/reference/grid_kriging.md)
is the same idea taken seriously: weights chosen from a fitted model of
how the values vary with distance, which then also says how uncertain
each cell is.

## Examples

``` r
xy <- cbind(runif(50), runif(50))
if (requireNamespace("gstat", quietly = TRUE)) {
  op <- par(mfrow = c(1, 2))
  plot(grid_idw(xy, xy[, 1], idp = 0.5), main = "idp = 0.5")
  plot(grid_idw(xy, xy[, 1], idp = 8), main = "idp = 8")
  par(op)
}
```
