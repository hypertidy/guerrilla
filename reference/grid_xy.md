# Coordinates of the cell centres

The centre of every cell of `grid`, in cell order: left to right along
the top row, then the next row down.

## Usage

``` r
grid_xy(grid)
```

## Arguments

- grid:

  a `guerrilla_grid`

## Value

A two column matrix with one row per cell.

## Details

This is one call to
[`vaster::xy_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.html)
with the grid's own first two elements. It is the only thing the
interpolation functions need to know about the grid, which is why the
grid can be a list.

## Examples

``` r
grid_xy(grid_spec(dimension = c(3, 2), extent = c(0, 6, 0, 4)))
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    3    3
#> [3,]    5    3
#> [4,]    1    1
#> [5,]    3    1
#> [6,]    5    1
```
