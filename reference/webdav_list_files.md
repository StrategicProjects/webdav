# List files from a specific folder on WebDAV server

This function lists the files in a specific folder on the WebDAV server.
If no folder path is provided, it lists files from the root directory.
The function validates the provided parameters and handles errors during
the process.

## Usage

``` r
webdav_list_files(
  base_url,
  folder_path = NULL,
  username = Sys.getenv("WEBDAV_USERNAME"),
  password = Sys.getenv("WEBDAV_PASSWORD"),
  depth = 1,
  verbose = FALSE
)
```

## Arguments

- base_url:

  The base URL of the WebDAV server.

- folder_path:

  The path inside WebDAV where the files are located. If not provided or
  empty, the root folder will be listed.

- username:

  The username for WebDAV authentication. Defaults to the
  "WEBDAV_USERNAME" environment variable.

- password:

  The password for WebDAV authentication. Defaults to the
  "WEBDAV_PASSWORD" environment variable.

- depth:

  The depth of the PROPFIND request (default is 1).

- verbose:

  Logical value indicating whether to print detailed debug messages.
  When TRUE, the function outputs additional information about its
  progress and actions.

## Value

A tibble containing:

- display_name:

  The name of the file or directory.

- full_path:

  The full URL (href) of the resource.

- creation_date:

  The date the resource was created.

- last_modified:

  The date the resource was last modified.

- content_length:

  The size of the resource in bytes (NA for directories).

- is_folder:

  Logical indicating whether the resource is a directory.

Returns \`NULL\` if an error occurs during the execution of the
function.

## Examples

``` r
# Example usage with a public WebDAV server.
# Visit test_server$url link to view the results of the operation.
library(httr2)
test_server <- "http://webdavserver.net/" |>
  request() |>
  req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
  req_perform() |>
  try(silent = TRUE)

# List files in a directory
if (class(test_server) != "try-error")
  webdav_list_files(base_url = test_server$url, folder_path = "Sales/", verbose = TRUE)
#> ℹ Listing files in folder: Sales/
#> ℹ Full URL: <http://webdavserver.net/User6f8b599/Sales>
#> ℹ Base URL: <http://webdavserver.net/User6f8b599/Sales>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
#> ✔ Files listed successfully.
#> # A tibble: 4 × 6
#>   display_name full_path     creation_date last_modified content_length
#>   <chr>        <chr>         <chr>         <chr>                  <dbl>
#> 1 Australia/   http://webda… 2026-04-07T2… Tue, 07 Apr …             NA
#> 2 Canada/      http://webda… 2026-04-07T2… Tue, 07 Apr …             NA
#> 3 Europe/      http://webda… 2026-04-07T2… Tue, 07 Apr …             NA
#> 4 USA/         http://webda… 2026-04-07T2… Tue, 07 Apr …             NA
#> # ℹ 1 more variable: is_folder <lgl>
```
