# Delete a file or directory from the WebDAV server

This function deletes a file or directory on the WebDAV server using the
DELETE method. It validates the provided parameters and handles errors
during the process.

## Usage

``` r
webdav_delete_resource(
  base_url,
  resource_path,
  username = Sys.getenv("WEBDAV_USERNAME"),
  password = Sys.getenv("WEBDAV_PASSWORD"),
  verbose = FALSE
)
```

## Arguments

- base_url:

  The base URL of the WebDAV server.

- resource_path:

  The path of the file or directory to delete on the WebDAV server.

- username:

  The username for WebDAV authentication. Defaults to the
  "WEBDAV_USERNAME" environment variable.

- password:

  The password for WebDAV authentication. Defaults to the
  "WEBDAV_PASSWORD" environment variable.

- verbose:

  Logical value indicating whether to print detailed debug messages.
  When TRUE, the function outputs additional information about its
  progress and actions.

## Value

Logical value indicating whether the file or directory was deleted
successfully.

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

# Delete a file or directory
if (class(test_server) != "try-error")
  webdav_delete_resource(base_url = test_server$url, resource_path = "Notes.txt", verbose = TRUE)
#> ℹ Attempting to delete resource at:
#>   <http://webdavserver.net/Userb0325c3/Notes.txt>
#> ℹ Base URL: <http://webdavserver.net/Userb0325c3/Notes.txt>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
#> ✔ Resource successfully deleted at: Notes.txt
#> [1] TRUE
```
