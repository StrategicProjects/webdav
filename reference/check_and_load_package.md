# Check if a package is installed and load it

\`r lifecycle::badge("deprecated")\`

This function is deprecated because all required packages are declared
in the package DESCRIPTION file and are automatically available. There
is no need to check or load them at runtime.

## Usage

``` r
check_and_load_package(package_name)
```

## Arguments

- package_name:

  A string with the name of the package to check and load.

## Value

Invisibly returns \`TRUE\` if the package namespace is available.

## Examples

``` r
check_and_load_package("httr2")
#> Warning: `check_and_load_package()` is deprecated.
#> ℹ All required packages are declared in DESCRIPTION and loaded
#>   automatically.
```
