# Download a file from the WebDAV server

This function downloads a file from the WebDAV server and saves it to a
local directory. It validates the provided parameters, handles errors,
and optionally prints detailed logs if requested.

## Usage

``` r
webdav_download_file(
  base_url,
  file_path,
  destination_path = ".",
  username = Sys.getenv("WEBDAV_USERNAME"),
  password = Sys.getenv("WEBDAV_PASSWORD"),
  verbose = FALSE
)
```

## Arguments

- base_url:

  The base URL of the WebDAV server (e.g.,
  "https://example.com/remote.php/dav/files/").

- file_path:

  The path of the file on the WebDAV server to download (relative to the
  \`base_url\`).

- destination_path:

  The local directory where the downloaded file will be saved. Defaults
  to the current directory.

- username:

  The username for WebDAV authentication. Defaults to the
  "WEBDAV_USERNAME" environment variable.

- password:

  The password for WebDAV authentication. Defaults to the
  "WEBDAV_PASSWORD" environment variable.

- verbose:

  Logical. If TRUE, prints detailed messages during the download
  process.

## Value

Logical value indicating whether the file was downloaded successfully.

## Examples

``` r
# Example usage with a public WebDAV server.
library(httr2)
test_server <- "http://webdavserver.net/" |>
  request() |>
  req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
  req_perform() |>
  try(silent = TRUE)

# Download a file from the WebDAV server
if (class(test_server) != "try-error")
  webdav_download_file(base_url = test_server$url,
    file_path = "Project.pdf",
    destination_path = tempdir(),
    verbose = TRUE)
#> ℹ Base URL: <http://webdavserver.net/User2a2ba47>
#> ℹ Downloading from: Project.pdf to: /tmp/RtmphwSEO6/Project.pdf
#> ℹ Server Path: <http://webdavserver.net/User2a2ba47/Project.pdf>
#> ℹ Local Destination Path: /tmp/RtmphwSEO6/Project.pdf
#> ℹ Base URL: <http://webdavserver.net/User2a2ba47/Project.pdf>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
#> ✔ Resource successfully downloaded from <http://webdavserver.net/User2a2ba47/Project.pdf>
#> ✔ Resource successfully written to /tmp/RtmphwSEO6/Project.pdf
#> [1] TRUE
# Visit test_server$url to view the results of the operation.
```
