# Get Started

This overview introduces the `webdav` R package, which provides an
interface for managing files on WebDAV-compliant servers, enabling file
operations such as uploading, downloading, copying, moving, and deleting
resources.

## Functions

### 1. Creating a WebDAV Request

\
[`webdav_create_request`](https://strategicprojects.github.io/webdav/reference/webdav_create_request.md)`(``base_url``, ``username``, ``password``, verbose ``=`` ``FALSE``)`

This function creates a basic authenticated WebDAV request for the given
resource path on a WebDAV server. It sets up authentication using the
provided username and password and returns a request object.

**Parameters**:

1.  `base_url`: Base URL of the WebDAV server (e.g.,
    `https://drive.expresso.pe.gov.br`).
2.  `username`: Username for authentication.
3.  `password`: Password for authentication.
4.  `verbose`: Whether to print debug information (default is `FALSE`).

**Example**:

\
`# Example usage with a public WebDAV server.`\
`# Visit test_server$url link to view the results of the operation.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`try`](https://rdrr.io/r/base/try.html)`(``silent ``=`` ``TRUE``)`\
\
`# Create a request`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  ``req`` ``<-`` `[`webdav_create_request`](https://strategicprojects.github.io/webdav/reference/webdav_create_request.md)`(``base_url ``=`` ``test_server``$``url``, verbose ``=`` ``TRUE``)`

### 2. Copying Files

\
[`webdav_copy_file`](https://strategicprojects.github.io/webdav/reference/webdav_copy_file.md)`(``base_url``, ``from_path``, ``to_path``, ``username``, ``password``, overwrite ``=`` ``TRUE``)`

This function copies a file or directory from one path to another on a
WebDAV server.

**Parameters**:

1.  `base_url`: Base URL of the WebDAV server.
2.  `from_path`: The source file path to copy.
3.  `to_path`: The destination path where the file will be copied.
4.  `username`: Username for authentication.
5.  `password`: Password for authentication.
6.  `overwrite`: If `TRUE` (the default), an existing resource at
    `to_path` is overwritten.

**Example**:

\
`# Example usage with a public WebDAV server.`\
`# Visit test_server$url link to view the results of the operation.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`try`](https://rdrr.io/r/base/try.html)`(``silent ``=`` ``TRUE``)`\
\
`# Copy a file from one path to another`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  `[`webdav_copy_file`](https://strategicprojects.github.io/webdav/reference/webdav_copy_file.md)`(``base_url ``=`` ``test_server``$``url``,`\
`    from_path ``=`` ``"Project.pdf"``,`\
`    to_path ``=`` ``"New_Project.pdf"``,`\
`    verbose ``=`` ``TRUE``)`

### 3. Moving or Renaming Files

\
[`webdav_move_file`](https://strategicprojects.github.io/webdav/reference/webdav_move_file.md)`(``base_url``, ``from_path``, ``to_path``, ``username``, ``password``, overwrite ``=`` ``TRUE``)`

This function moves (or renames) a file or directory on the WebDAV
server using the MOVE method. Moving within the same folder renames the
resource.

**Parameters**:

1.  `base_url`: Base URL of the WebDAV server.
2.  `from_path`: The source file path to move.
3.  `to_path`: The destination path of the resource.
4.  `username`: Username for authentication.
5.  `password`: Password for authentication.
6.  `overwrite`: If `TRUE` (the default), an existing resource at
    `to_path` is overwritten.

**Example**:

\
`# Example usage with a public WebDAV server.`\
`# Visit test_server$url link to view the results of the operation.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`try`](https://rdrr.io/r/base/try.html)`(``silent ``=`` ``TRUE``)`\
\
`# Rename a file on the WebDAV server`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  `[`webdav_move_file`](https://strategicprojects.github.io/webdav/reference/webdav_move_file.md)`(``base_url ``=`` ``test_server``$``url``,`\
`    from_path ``=`` ``"Notes.txt"``,`\
`    to_path ``=`` ``"Notes-renamed.txt"``,`\
`    verbose ``=`` ``TRUE``)`

### 4. Creating a Directory

\
[`webdav_create_directory`](https://strategicprojects.github.io/webdav/reference/webdav_create_directory.md)`(``base_url``, ``folder_path``, ``username``, ``password``)`

This function creates a new directory (or collection) on the WebDAV
server using the MKCOL method.

**Parameters**:

1.  `base_url`: Base URL of the WebDAV server.
2.  `folder_path`: Path to the directory to be created.
3.  `username`: Username for authentication.
4.  `password`: Password for authentication.

**Example**:

\
`# Example usage with a public WebDAV server.`\
`# Visit test_server$url link to view the results of the operation.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`try`](https://rdrr.io/r/base/try.html)`(``silent ``=`` ``TRUE``)`\
\
`# Create a directory on the WebDAV server`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  `[`webdav_create_directory`](https://strategicprojects.github.io/webdav/reference/webdav_create_directory.md)`(`\
`    base_url ``=`` ``test_server``$``url``, `\
`    folder_path ``=`` ``"Test_Folder"``, `\
`    verbose ``=`` ``TRUE``)`

### 5. Deleting a File or Directory

\
[`webdav_delete_resource`](https://strategicprojects.github.io/webdav/reference/webdav_delete_resource.md)`(`\
`  ``base_url``, `\
`  ``resource_path``, `\
`  ``username``, `\
`  ``password``, `\
`  verbose ``=`` ``FALSE`\
`  ``)`

This function deletes a file or directory from the WebDAV server using
the DELETE method.

**Parameters**: 1. `base_url`: Base URL of the WebDAV server. 1.
`resource_path`: The path to the file or directory to delete. 1.
`username`: Username for authentication. 1. `password`: Password for
authentication.

**Example**:

\
`# Example usage with a public WebDAV server.`\
`# Visit test_server$url link to view the results of the operation.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`try`](https://rdrr.io/r/base/try.html)`(``silent ``=`` ``TRUE``)`\
\
`# Delete a file or directory`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  `[`webdav_delete_resource`](https://strategicprojects.github.io/webdav/reference/webdav_delete_resource.md)`(``base_url ``=`` ``test_server``$``url``, `\
`  resource_path ``=`` ``"Notes.txt"``,`\
`  verbose ``=`` ``TRUE``)`

### 6. Listing Files in a Directory

\
[`webdav_list_files`](https://strategicprojects.github.io/webdav/reference/webdav_list_files.md)`(`\
`  ``base_url``, `\
`  ``folder_path``, `\
`  ``username``, `\
`  ``password``, `\
`  depth ``=`` ``1``, `\
`  verbose ``=`` ``FALSE`\
`  ``)`

This function lists the files within a directory on the WebDAV server
using the PROPFIND method.

**Parameters**:

1.  `base_url`: Base URL of the WebDAV server.
2.  `folder_path`: Path to the folder to list files.
3.  `username`: Username for authentication.
4.  `password`: Password for authentication.
5.  `depth`: Depth of the PROPFIND request (default is `1`).

**Example**:

\
`# Example usage with a public WebDAV server.`\
`# Visit test_server$url link to view the results of the operation.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`try`](https://rdrr.io/r/base/try.html)`(``silent ``=`` ``TRUE``)`\
\
`# List files in a directory`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  `[`webdav_list_files`](https://strategicprojects.github.io/webdav/reference/webdav_list_files.md)`(`\
`    base_url ``=`` ``test_server``$``url``, `\
`    folder_path ``=`` ``"Sales/"``, `\
`    verbose ``=`` ``TRUE``)`

### 7. Uploading a File

\
[`webdav_upload_file`](https://strategicprojects.github.io/webdav/reference/webdav_upload_file.md)`(`\
`  ``base_url``, `\
`  ``local_path``, `\
`  ``server_path``, `\
`  ``username``, `\
`  ``password``, `\
`  timeout ``=`` ``300``,`\
`  verbose ``=`` ``FALSE`\
`  ``)`

This function uploads a local file to the WebDAV server.

**Parameters**:

1.  `base_url`: Base URL of the WebDAV server.
2.  `local_path`: Local path to the file.
3.  `server_path`: Path on the WebDAV server where the file will be
    uploaded.
4.  `username`: Username for authentication.
5.  `password`: Password for authentication.
6.  `timeout`: Time limit for the upload operation (default is 300
    seconds).

**Example**:

\
`# Example usage with a public WebDAV server.`\
`# Visit test_server$url link to view the results of the operation.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`try`](https://rdrr.io/r/base/try.html)`(``silent ``=`` ``TRUE``)`\
\
`# Upload a file`\
`file_test`` ``<-`` `[`tempfile`](https://rdrr.io/r/base/tempfile.html)`(``pattern ``=`` ``"teste_"``, fileext ``=`` ``".txt"``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(``"Text file content"``, file ``=`` ``file_test``)`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  `[`webdav_upload_file`](https://strategicprojects.github.io/webdav/reference/webdav_upload_file.md)`(``base_url ``=`` ``test_server``$``url``, local_path ``=`` ``file_test``, verbose ``=`` ``TRUE``)`

### 8. Download a file

\
[`webdav_download_file`](https://strategicprojects.github.io/webdav/reference/webdav_download_file.md)`(``base_url``, ``file_path``, ``destination_path``, ``username``, ``password``, ``verbose``)`

This function downloads a file from the WebDAV server and saves it to a
local directory. It validates the provided parameters, handles errors,
and optionally prints detailed logs if requested.

**Parameters**:

1.  `base_url`: The base URL of the WebDAV server.
2.  `file_path`: The path of the file on the WebDAV server to download
    (relative to the ‘base_url’).
3.  `destination_path`: The local directory where the downloaded file
    will be saved. Defaults to the current directory.
4.  `username`: The username for WebDAV authentication. Defaults to the
    “WEBDAV_USERNAME” environment variable.
5.  `password`: The password for WebDAV authentication. Defaults to the
    “WEBDAV_PASSWORD” environment variable.
6.  `verbose`: Logical. If TRUE, prints detailed messages during the
    download process.

**Example**:

\
`# Example usage with a public WebDAV server.`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`magrittr`](https://magrittr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`httr2`](https://httr2.r-lib.org)`)`\
`test_server`` ``<-`` ``"https://www.webdavserver.com/"`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`request`](https://httr2.r-lib.org/reference/request.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_retry`](https://httr2.r-lib.org/reference/req_retry.html)`(``max_tries ``=`` ``3``, max_seconds ``=`` ``4``, backoff ``=``  ``~`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`req_perform`](https://httr2.r-lib.org/reference/req_perform.html)`(``)`\
\
`# Download a file from the WebDAV server`\
`if`` ``(``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``test_server``, ``"try-error"``)``)`\
`  `[`webdav_download_file`](https://strategicprojects.github.io/webdav/reference/webdav_download_file.md)`(``base_url ``=`` ``test_server``$``url``,`\
`    file_path ``=`` ``"Project.pdf"``,`\
`    destination_path ``=`` `[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``,`\
`    verbose ``=`` ``TRUE``)`\
`# Visit test_server$url to view the results of the operation.`

## Conclusion

The **`webdav`** R package provides a interface for managing files and
directories on WebDAV-enabled servers. With basic file management
(uploading, downloading, deleting, copying), directory management, and
resource locking, the package simplifies interactions with platforms
like **OwnCloud**, **NextCloud**, and other WebDAV-compliant systems.
