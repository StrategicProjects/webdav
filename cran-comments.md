## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a new release.

### Changes in Version .1.4

* [New Feature] Added a new function `webdav_download_file()` to download files from a WebDAV server to a local directory.
  - This function allows users to specify a file on the server and download it to a specified local path.
  - Verbose mode (`verbose = TRUE`) provides detailed output of the download process, including the file path on the server and local destination.

#### Comments

* Removed: 
    - Found the following (possibly) invalid URLs:
    - URL: https://example.com/remote.php/dav/files/
    - From: inst/doc/webdav.html
    - Status: 404
    - Message: Not Found
* Removed: 
    - URL: https://monitoramento.sepe.pe.gov.br/webdav
    - From: DESCRIPTION
    - Status: Error
    - Message: Connection timed out after 60002 milliseconds

### Changes in Version 0.1.5 

* fix: correct faulty example
