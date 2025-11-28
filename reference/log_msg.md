# Log a timestamped message to the console

Utility function to print a timestamped message using
[`message()`](https://rdrr.io/r/base/message.html), which works well
with progress bars and other console output.

## Usage

``` r
log_msg(msg)
```

## Arguments

- msg:

  A character string to log.

## Examples

``` r
log_msg("Starting the process...")
#> [2025-11-28 05:57] Starting the process...
```
