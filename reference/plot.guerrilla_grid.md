# Plot a grid

Draw the grid with
[`graphics::image()`](https://rdrr.io/r/graphics/image.html). No raster
class is involved, which is the point: a grid is a dimension, an extent
and a vector of values, and that is enough to draw it.

## Usage

``` r
# S3 method for class 'guerrilla_grid'
plot(x, col = grDevices::hcl.colors(24, "YlGnBu"), asp = 1, ...)
```

## Arguments

- x:

  a `guerrilla_grid` with values

- col:

  colours

- asp:

  aspect ratio; `1` by default, or use `NA` to fill the device

- ...:

  passed to [`graphics::image()`](https://rdrr.io/r/graphics/image.html)

## Value

`x`, invisibly.

## Examples

``` r
xy <- cbind(runif(50), runif(50))
g <- grid_barycentric(xy, xy[, 1] + xy[, 2])
plot(g)
```
