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

- verbose:

  Logical. If TRUE, prints detailed messages during the copy process.

## Value

Logical value indicating whether the resource was copied successfully.
