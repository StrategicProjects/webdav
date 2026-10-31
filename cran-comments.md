# webdav 0.3.0

## Summary of changes

* New `webdav_move_file()` (WebDAV MOVE method); `webdav_copy_file()` gained
  an `overwrite` argument.
* Bug fixes: `webdav_list_files()` no longer fails on R < 4.4.0 (the package
  now defines `%||%` internally) and handles folder names containing regex
  metacharacters; `webdav_copy_file()` now percent-encodes the `Destination`
  header.
* Error handling is now consistent across all functions (invalid arguments
  raise an error; network/server failures warn and return `FALSE`), with
  precise HTTP status messages.
* Uploads and downloads are now streamed from/to disk instead of being held
  in memory.
* Added an offline test suite (testthat, with mocked requests); examples and
  tests do not require network access to pass.
* Removed `LazyData` from DESCRIPTION (the package has no data directory).

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* Checked locally on macOS (aarch64), R 4.6.0.

## Reverse dependencies

There are no reverse dependencies on CRAN.
