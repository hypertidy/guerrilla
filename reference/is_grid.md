# Is this a guerrilla grid?

Is this a guerrilla grid?

## Usage

``` r
is_grid(x)
```

## Arguments

- x:

  any object

## Value

`TRUE` or `FALSE`

## Examples

``` r
is_grid(grid_spec(cbind(1:3, 1:3)))
#> [1] TRUE
is_grid(1:10)
#> [1] FALSE
```
