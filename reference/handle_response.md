# Handle HTTP response from Server

This function processes the response from the WebDAV server, checking
for errors.

## Usage

``` r
handle_response(response)
```

## Arguments

- response:

  The response object from an \`httr2\` request.

## Value

The processed response object if successful, or an error if the request
failed.
