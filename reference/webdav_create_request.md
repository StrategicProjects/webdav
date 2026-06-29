# Create a request for the WebDAV server

This function creates a base request for the WebDAV server with proper
authentication. It validates the provided parameters and handles errors
during the connection setup.

## Usage

``` r
webdav_create_request(
  base_url,
  username = Sys.getenv("WEBDAV_USERNAME"),
  password = Sys.getenv("WEBDAV_PASSWORD"),
  verbose = FALSE
)
```

## Arguments

- base_url:

  The base URL of the WebDAV server (e.g.,
  "https://example.com/remote.php/dav/files/").

- username:

  The username for WebDAV authentication. Defaults to the
  "WEBDAV_USERNAME" environment variable.

- password:

  The password for WebDAV authentication. Defaults to the
  "WEBDAV_PASSWORD" environment variable.

- verbose:

  Logical. If TRUE, prints detailed messages during the request creation
  process.

## Value

An \`httr2_request\` object with authentication and base URL configured,
or an error message if the connection fails.

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

# Create a request
if (class(test_server) != "try-error")
  req <- webdav_create_request(base_url = test_server$url, verbose = TRUE)
#> ℹ Base URL: <http://webdavserver.net/User98501d8>
#> ℹ Username: "Not provided"
#> ✔ Request object created successfully.
#> ! No authentication added.
```
