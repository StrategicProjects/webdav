# Upload a file to the WebDAV server

This function uploads a file to a specific folder on the WebDAV server.
It validates the provided parameters and handles errors during the
process.

## Usage

``` r
webdav_upload_file(
  base_url,
  local_path,
  server_path = "",
  username = Sys.getenv("WEBDAV_USERNAME"),
  password = Sys.getenv("WEBDAV_PASSWORD"),
  timeout = 300,
  verbose = FALSE
)
```

## Arguments

- base_url:

  The base URL of the WebDAV server.

- local_path:

  The local path of the file to be uploaded.

- server_path:

  The folder path on the WebDAV server where the file will be uploaded.

- username:

  The username for WebDAV authentication. Defaults to the
  "WEBDAV_USERNAME" environment variable.

- password:

  The password for WebDAV authentication. Defaults to the
  "WEBDAV_PASSWORD" environment variable.

- timeout:

  The timeout for the upload request in seconds (default is 300
  seconds).

- verbose:

  Logical value indicating whether to print detailed debug messages.
  When TRUE, the function outputs additional information about its
  progress and actions.

## Value

Logical value indicating whether the file was uploaded successfully.

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

# Upload a file
file_test <- tempfile(pattern = "teste_", fileext = ".txt")
cat("Text file content", file = file_test)
if (class(test_server) != "try-error")
  webdav_upload_file(base_url = test_server$url, local_path = file_test, verbose = TRUE)
#> ℹ Uploading file: /tmp/RtmpjnK4zR/teste_1c00245de33d.txt
#> ℹ Target URL:
#>   <http://webdavserver.net/Usereae52dc/teste_1c00245de33d.txt>
#> ℹ Base URL:
#>   <http://webdavserver.net/Usereae52dc/teste_1c00245de33d.txt>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
#> ✔ File successfully uploaded to: 
#> [1] TRUE
```
