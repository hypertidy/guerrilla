# Number of cells in a grid

Number of cells in a grid

## Usage

``` r
grid_ncell(grid)
```

## Arguments

- grid:

  a `guerrilla_grid`

## Value

A single integer, the product of the two dimensions.

## Examples

``` r
grid_ncell(grid_spec(dimension = c(3, 2), extent = c(0, 6, 0, 4)))
#> [1] 6
```
