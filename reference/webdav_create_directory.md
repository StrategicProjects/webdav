# Create a collection (directory) on a WebDAV server

This function creates a collection (directory/folder) on the WebDAV
server using the MKCOL method. It validates parameters and handles
errors during the process.

## Usage

``` r
webdav_create_directory(
  base_url,
  folder_path,
  username = Sys.getenv("WEBDAV_USERNAME"),
  password = Sys.getenv("WEBDAV_PASSWORD"),
  verbose = FALSE
)
```

## Arguments

- base_url:

  The base URL of the WebDAV server (e.g.,
  "https://example.com/remote.php/dav/files/").

- folder_path:

  The path of the directory to create.

- username:

  The username for WebDAV authentication. Defaults to the
  "WEBDAV_USERNAME" environment variable.

- password:

  The password for WebDAV authentication. Defaults to the
  "WEBDAV_PASSWORD" environment variable.

- verbose:

  Logical. If TRUE, prints detailed messages during the directory
  creation process.

## Value

Logical value indicating whether the collection was created
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

# Create a directory on the WebDAV server
if (class(test_server) != "try-error")
  webdav_create_directory(base_url = test_server$url, folder_path = "Test_Folder", verbose = TRUE)
#> ℹ Folder path to create: Test_Folder
#> ℹ Base URL: <http://webdavserver.net/Userf48b007/Test_Folder>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
#> ✔ Collection successfully created at: Test_Folder
#> [1] TRUE
```
