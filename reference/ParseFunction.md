# Parse Function String to Function Object

Converts a character string representing a function name into an actual
function object. Supports both simple function names (e.g., "mean") and
package-qualified names (e.g., "gsm.studykri::Transform_Long"). This is
particularly useful in YAML workflows where function references need to
be stored as strings and later resolved.

## Usage

``` r
ParseFunction(strFunction)
```

## Arguments

- strFunction:

  `character` A single character string representing a function name.
  Can be a simple name like "mean" or package-qualified like
  "pkg::function".

## Value

A function object resolved from the input string.

## Details

The function uses `eval(parse(text=...))` to resolve the string to a
function object. This approach successfully handles the `::` operator
for package-qualified names.

If the string does not resolve to a function object, an error is thrown.

## Examples

``` r
# Parse a base R function
func <- ParseFunction("mean")
func(1:10)
#> [1] 5.5

# Parse a package-qualified function
func <- ParseFunction("gsm.studykri::Transform_Long")

if (FALSE) { # \dontrun{
# Use in YAML workflow pattern:
# Step 1: Parse function string
# - output: MyFunc
#   name: gsm.studykri::ParseFunction
#   params:
#     strFunction: package::function_name
#
# Step 2: Use parsed function
# - output: Result
#   name: purrr::map
#   params:
#     .x: MyData
#     .f: MyFunc
} # }
```
