# Move or rename a resource on the WebDAV server

This function moves (or renames) a resource from one URI to another on
the WebDAV server using the MOVE method. It validates the provided
parameters and handles errors during the move process.

## Usage

``` r
webdav_move_file(
  base_url,
  from_path,
  to_path,
  username = Sys.getenv("WEBDAV_USERNAME"),
  password = Sys.getenv("WEBDAV_PASSWORD"),
  overwrite = TRUE,
  verbose = FALSE
)
```

## Arguments

- base_url:

  The base URL of the WebDAV server.

- from_path:

  The source path of the resource to move.

- to_path:

  The destination path of the resource. Moving within the same folder
  renames the resource.

- username:

  The username for WebDAV authentication. Defaults to the
  "WEBDAV_USERNAME" environment variable.

- password:

  The password for WebDAV authentication. Defaults to the
  "WEBDAV_PASSWORD" environment variable.

- overwrite:

  Logical. If TRUE (the default, matching the WebDAV protocol default),
  an existing resource at \`to_path\` is overwritten.

- verbose:

  Logical. If TRUE, prints detailed messages during the move process.

## Value

\`TRUE\` if the resource was moved successfully, \`FALSE\` (with a
warning) otherwise. Invalid arguments raise an error.

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

# Rename a file on the WebDAV server
if (!inherits(test_server, "try-error"))
  webdav_move_file(base_url = test_server$url,
    from_path = "Notes.txt",
    to_path = "Notes-renamed.txt",
    verbose = TRUE)
#> ℹ Base URL: <http://webdavserver.net/User20550fc>
#> ℹ Moving from: Notes.txt to: Notes-renamed.txt
#> ℹ Source Path: <http://webdavserver.net/User20550fc/Notes.txt>
#> ℹ Destination Path:
#>   <http://webdavserver.net/User20550fc/Notes-renamed.txt>
#> ℹ Base URL: <http://webdavserver.net/User20550fc/Notes.txt>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
#> ✔ Resource successfully moved from Notes.txt to Notes-renamed.txt
#> [1] TRUE
```
