# Changelog

## webdav 0.2.0

### Breaking Changes

- [`check_and_load_package()`](https://monitoramento.sepe.pe.gov.br/webdav/reference/check_and_load_package.md)
  is now deprecated. All required packages are declared in `DESCRIPTION`
  and loaded automatically by R. Calling this function will issue a
  deprecation warning.

### Enhancements

- Migrated all user-facing messages from
  [`message()`](https://rdrr.io/r/base/message.html) /
  [`stop()`](https://rdrr.io/r/base/stop.html) to the `cli` package
  ([`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_alert_success()`](https://cli.r-lib.org/reference/cli_alert.html),
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)),
  providing richer formatted output with semantic markup for URLs, file
  paths, and argument names.
- Added `cli` to `Imports` in DESCRIPTION.
- Added
  [`httr2::resp_status()`](https://httr2.r-lib.org/reference/resp_status.html)
  import to NAMESPACE.
- Fixed
  [`handle_response()`](https://monitoramento.sepe.pe.gov.br/webdav/reference/handle_response.md)
  to use
  [`httr2::resp_status()`](https://httr2.r-lib.org/reference/resp_status.html)
  instead of the legacy `response$status_code` accessor (httr v1
  syntax).
- Removed redundant runtime calls to
  [`check_and_load_package()`](https://monitoramento.sepe.pe.gov.br/webdav/reference/check_and_load_package.md)
  inside all exported functions — dependencies declared in `Imports` are
  guaranteed to be available and do not need manual loading.

### Authors

- Added Marcos Wasilew, Carlos Amorin as authors.
- Fixed Hugo Vasconcelos surname spelling.

## webdav 0.1.6

CRAN release: 2025-07-17

### Enhancements

- Major improvements to the
  [`webdav_list_files()`](https://monitoramento.sepe.pe.gov.br/webdav/reference/webdav_list_files.md)
  function for better compatibility and robustness:
  - Now dynamically detects and uses the correct XML namespace prefix
    from the server (`d`, `D`, or other), preventing XPath errors like
    `Undefined namespace prefix`.
  - Uses `xml2::as_list()` to reliably parse the XML response structure.
  - Ensures `content_length` is safely parsed and padded with `NA` where
    missing (e.g., for directories), preventing errors when creating the
    resulting `tibble`.
  - Adds a new field `is_folder`, correctly identifying whether each
    resource is a directory.
  - Improved documentation and output consistency with more informative
    column names (e.g., `display_name`, `full_path`, `creation_date`,
    etc.).

### Acknowledgements

Special thanks to **Adrian Jusepeitis** (University of Jena, Germany)
for reporting two key issues related to XML namespace parsing and
missing metadata in
[`webdav_list_files()`](https://monitoramento.sepe.pe.gov.br/webdav/reference/webdav_list_files.md),
and for proposing initial fixes that inspired the improvements in this
release.

## webdav 0.1.4

CRAN release: 2025-02-13

- Changed the license to MIT
- Adjusted the time limits in the examples
- Added an internet connection test to ensure network availability

## webdav 0.1.3

CRAN release: 2025-01-08

- Expose lastmodified and contentlength via webdav_list_files. Idea from
  Benjamin Buchwitz (bchwtz) issue:
  <https://github.com/StrategicProjects/webdav/issues/1>

## webdav 0.1.2

CRAN release: 2024-12-02

- \[New Feature\] Added a new function
  [`webdav_download_file()`](https://monitoramento.sepe.pe.gov.br/webdav/reference/webdav_download_file.md)
  to download files from a WebDAV server to a local directory.
  - This function allows users to specify a file on the server and
    download it to a specified local path.
  - Verbose mode (`verbose = TRUE`) provides detailed output of the
    download process, including the file path on the server and local
    destination.
- Added examples using demo WebDav server.

## webdav 0.1.1

CRAN release: 2024-10-08

- Added a `NEWS.md` file to track changes to the package.
