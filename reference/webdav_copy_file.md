# Copy a resource on the WebDAV server

This function copies a resource from one URI to another on the WebDAV
server using the COPY method. It validates the provided parameters and
handles errors during the copy process.

## Usage

``` r
webdav_copy_file(
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

  The source path of the resource to copy.

- to_path:

  The destination path where the resource will be copied.

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

  Logical. If TRUE, prints detailed messages during the copy process.

## Value

\`TRUE\` if the resource was copied successfully, \`FALSE\` (with a
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

# Copy a file on the WebDAV server
if (!inherits(test_server, "try-error"))
  webdav_copy_file(base_url = test_server$url,
    from_path = "Project.pdf",
    to_path = "Project-copy.pdf",
    verbose = TRUE)
#> ℹ Base URL: <http://webdavserver.net/Userc297020>
#> ℹ Copying from: Project.pdf to: Project-copy.pdf
#> ℹ Source Path: <http://webdavserver.net/Userc297020/Project.pdf>
#> ℹ Destination Path:
#>   <http://webdavserver.net/Userc297020/Project-copy.pdf>
#> ℹ Base URL: <http://webdavserver.net/Userc297020/Project.pdf>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
#> ✔ Resource successfully copied from Project.pdf to Project-copy.pdf
#> [1] TRUE
```
