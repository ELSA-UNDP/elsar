# Clean up Earth Engine environment

Removes temporary conda environment if one was created during
initialization. Should be called at the end of any function that uses
initialize_earthengine().

## Usage

``` r
cleanup_earthengine(env_info)
```

## Arguments

- env_info:

  List returned from initialize_earthengine() containing temp_env info

## Examples

``` r
if (FALSE) { # \dontrun{
env_info <- initialize_earthengine()
# ... do work with GEE ...
cleanup_earthengine(env_info)
} # }
```
