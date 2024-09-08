#' Create a WebDAV request for the OwnCloud server
#'
#' This function creates a base WebDAV request for the OwnCloud server with the proper authentication.
#'
#' @param base_url The base URL of the OwnCloud server (e.g., "_server_url_").
#' @param path The specific path for the resource in the OwnCloud WebDAV server.
#' @param dav The WebDAV base path (e.g., "_dav_").
#' @param username The username for OwnCloud authentication. Defaults to the "webdav_USERNAME" environment variable.
#' @param password The password for OwnCloud authentication. Defaults to the "webdav_PASSWORD" environment variable.
#' @param verbose A logical value indicating whether to print detailed debug messages. When set to TRUE, the function will output additional information about its progress and actions. Default is FALSE.
#'
#' @return An `httr2_request` object with authentication and base URL configured.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' webdav_create_request("_server_url_", "_folder_", "_dav_")
#' }
#' @export
webdav_create_request <- function(base_url,
                           path,
                           dav,
                           username = Sys.getenv("OWNCLOUD_USERNAME"),
                           password = Sys.getenv("OWNCLOUD_PASSWORD"),
                           verbose = FALSE) {
  check_and_load_package("httr2")
  check_and_load_package("glue")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(path) || !is.character(path) || nchar(path) == 0) {
    stop("The 'path' parameter is required and must be a non-empty string.")
  }

  if (missing(dav) || !is.character(dav) || nchar(dav) == 0) {
    stop("The 'dav' parameter is required and must be a non-empty string.")
  }

  if (username == "" || password == "") {
    stop("Username or password is not defined. Check your environment variables.")
  }

  # Construct full URL using glue
  url <- glue::glue("{base_url}/remote.php/dav/files/{dav}{path}")
  if (verbose) message(url)
  # Create a base request with authentication
  request(url) %>%
    req_auth_basic(username, password)
}


#' Copy a resource on the OwnCloud server using WebDAV
#'
#' This function copies a resource from one URI to another on the OwnCloud server using the WebDAV COPY method.
#'
#' @param base_url The base URL of the OwnCloud server (e.g., "_server_url_").
#' @param source_path The source path of the resource to copy.
#' @param destination_path The destination path where the resource will be copied.
#' @param dav The WebDAV base path (e.g., "_dav_").
#' @param username The username for OwnCloud authentication.
#' @param password The password for OwnCloud authentication.
#'
#' @return Logical value indicating whether the resource was copied successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' webdav_copy_file("_server_url_", "_file_", "_resource_", "_dav_")
#' }
#' @export
webdav_copy_file <- function(base_url, source_path, destination_path, dav,
                             username = Sys.getenv("OWNCLOUD_USERNAME"),
                             password = Sys.getenv("OWNCLOUD_PASSWORD")) {
  check_and_load_package("httr2")
  check_and_load_package("glue")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(source_path) || !is.character(source_path) || nchar(source_path) == 0) {
    stop("The 'source_path' parameter is required and must be a non-empty string.")
  }

  if (missing(destination_path) || !is.character(destination_path) || nchar(destination_path) == 0) {
    stop("The 'destination_path' parameter is required and must be a non-empty string.")
  }

  if (missing(dav) || !is.character(dav) || nchar(dav) == 0) {
    stop("The 'dav' parameter is required and must be a non-empty string.")
  }

  if (username == "" || password == "") {
    stop("Username or password is not defined. Check your environment variables.")
  }

  # Create the base request
  req <- webdav_create_request(base_url, source_path, dav, username, password)

  # Set the Destination header and use the COPY method
  req <- req %>%
    req_headers(Destination = glue::glue("{base_url}/remote.php/dav/files/{dav}{destination_path}")) %>%
    req_method("COPY")

  # Perform the request and handle the response
  response <- req %>%
    req_perform()

  handle_response(response)

  message("Resource successfully copied from ", source_path, " to ", destination_path)
  return(TRUE)
}


#' Create a collection (directory) on the OwnCloud server using WebDAV
#'
#' This function creates a collection (directory) on the OwnCloud server using the WebDAV MKCOL method.
#'
#' @param base_url The base URL of the OwnCloud server (e.g., "_server_url_").
#' @param folder_path The path of the directory to create.
#' @param dav The WebDAV base path (e.g., "_dav_").
#' @param username The username for OwnCloud authentication.
#' @param password The password for OwnCloud authentication.
#'
#' @return Logical value indicating whether the collection was created successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' webdav_create_directory("_server_url_", "/Shared/der/app_painel/new_folder/", "_dav_")
#' }
#' @export
webdav_create_directory <- function(base_url, folder_path, dav,
                                    username = Sys.getenv("OWNCLOUD_USERNAME"),
                                    password = Sys.getenv("OWNCLOUD_PASSWORD")) {
  check_and_load_package("httr2")
  check_and_load_package("glue")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(folder_path) || !is.character(folder_path) || nchar(folder_path) == 0) {
    stop("The 'folder_path' parameter is required and must be a non-empty string.")
  }

  if (missing(dav) || !is.character(dav) || nchar(dav) == 0) {
    stop("The 'dav' parameter is required and must be a non-empty string.")
  }

  if (username == "" || password == "") {
    stop("Username or password is not defined. Check your environment variables.")
  }

  # Create the base request
  req <- webdav_create_request(base_url, folder_path, dav, username, password)

  # Set the MKCOL method
  req <- req %>%
    req_method("MKCOL")

  # Perform the request and handle the response
  response <- req %>%
    req_perform()

  handle_response(response)

  message("Collection successfully created at: ", folder_path)
  return(TRUE)
}

#' Check if a resource allows locking using PROPFIND
#'
#' This function checks if the resource allows the LOCK method by retrieving properties with PROPFIND.
#'
#' @param base_url The base URL of the OwnCloud server.
#' @param resource_path The path of the resource to check.
#' @param dav The WebDAV base path.
#' @param username The username for OwnCloud authentication.
#' @param password The password for OwnCloud authentication.
#'
#' @return Logical value indicating whether the resource supports locking.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom httr2 request resp_body_string
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @export
check_lock_support_webdav <- function(base_url, resource_path, dav,
                                      username = Sys.getenv("OWNCLOUD_USERNAME"),
                                      password = Sys.getenv("OWNCLOUD_PASSWORD")) {
  check_and_load_package("httr2")
  check_and_load_package("glue")

  # Construct the URL
  webdav_url <- glue("{base_url}/remote.php/dav/files/{dav}{resource_path}")

  # Create the PROPFIND request
  req <- request(webdav_url) %>%
    req_auth_basic(username, password) %>%
    req_method("PROPFIND") %>%
    req_headers("Depth" = "0", "Content-Type" = "application/xml")

  # Perform the request
  response <- req %>%
    req_perform()

  # Check if the response contains lock information
  if (response$status_code < 400) {
    content <- response %>% resp_body_string()
    if (grepl("lockdiscovery", content)) {
      message("The resource supports locking.")
      return(TRUE)
    } else {
      message("The resource does not support locking.")
      return(FALSE)
    }
  } else {
    stop("Error checking the resource: ", response$status_code)
  }
}

#' Lock a resource on the OwnCloud server using WebDAV
#'
#' This function locks a resource on the OwnCloud server using the WebDAV LOCK method.
#'
#' @param base_url The base URL of the OwnCloud server (e.g., "_server_url_").
#' @param resource_path The path of the resource to lock.
#' @param dav The WebDAV base path (e.g., "_dav_").
#' @param lock_type Type of lock ('exclusive' or 'shared').
#' @param username The username for OwnCloud authentication.
#' @param password The password for OwnCloud authentication.
#'
#' @return Logical value indicating whether the resource was locked successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' webdav_lock_resource("_server_url_", "_file_", "_dav_", "exclusive")
#' }
#' @export
webdav_lock_resource <- function(base_url, resource_path, dav, lock_type = "exclusive",
                                 username = Sys.getenv("OWNCLOUD_USERNAME"),
                                 password = Sys.getenv("OWNCLOUD_PASSWORD")) {
  check_and_load_package("httr2")
  check_and_load_package("glue")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(resource_path) || !is.character(resource_path) || nchar(resource_path) == 0) {
    stop("The 'resource_path' parameter is required and must be a non-empty string.")
  }

  if (missing(dav) || !is.character(dav) || nchar(dav) == 0) {
    stop("The 'dav' parameter is required and must be a non-empty string.")
  }

  if (username == "" || password == "") {
    stop("Username or password is not defined. Check your environment variables.")
  }

  # XML body for the lock request
  lock_xml <- glue::glue('
    <?xml version="1.0" encoding="utf-8" ?>
    <D:lockinfo xmlns:D="DAV:">
      <D:lockscope>
        <D:{lock_type} />
      </D:lockscope>
      <D:locktype>
        <D:write />
      </D:locktype>
      <D:owner>
        <D:href>OwnCloud User</D:href>
      </D:owner>
    </D:lockinfo>
  ')

  # Create the base request
  req <- webdav_create_request(base_url, resource_path, dav, username, password)

  # Set the LOCK method and body
  req <- req %>%
    #req_body(lock_xml) %>%
    req_body_raw(charToRaw(lock_xml)) %>%
    req_headers(Content_Type = "application/xml") %>%
    req_method("LOCK")

  # Perform the request and handle the response
  response <- req %>%
    req_perform()

  handle_response(response)

  message("Resource successfully locked at: ", resource_path)
  return(TRUE)
}


#' Upload a file to the OwnCloud server using WebDAV
#'
#' This function uploads a file to a specific folder on the OwnCloud server using WebDAV, with authentication and error handling.
#'
#' @param base_url The base URL of the OwnCloud server (e.g., "_server_url_").
#' @param file_path The local path of the file to be uploaded.
#' @param upload_path The folder path on the OwnCloud server where the file will be uploaded.
#' @param dav The WebDAV base path (e.g., "_dav_").
#' @param username The username for OwnCloud authentication.
#' @param password The password for OwnCloud authentication.
#' @param timeout The timeout for the upload request in seconds (default is 300 seconds).
#' @param verbose A logical value indicating whether to print detailed debug messages. When set to TRUE, the function will output additional information about its progress and actions. Default is FALSE.
#'
#' @return Logical value indicating whether the file was uploaded successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' webdav_upload_file("_server_url_", "_file_", "_folder_", "_dav_")
#' }
#'
#' @export
webdav_upload_file <- function(base_url,
                          file_path,
                          upload_path,
                          dav,
                          username = Sys.getenv("OWNCLOUD_USERNAME"),
                          password = Sys.getenv("OWNCLOUD_PASSWORD"),
                          timeout = 300,
                          verbose = FALSE) {
  check_and_load_package("httr2")
  check_and_load_package("glue")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(file_path) || !file.exists(file_path)) {
    stop("The 'file_path' is required and the file must exist.")
  }

  if (missing(upload_path) || !is.character(upload_path) || nchar(upload_path) == 0) {
    stop("The 'upload_path' is required and must be a non-empty string.")
  }

  if (missing(dav) || !is.character(dav) || nchar(dav) == 0) {
    stop("The 'dav' parameter is required and must be a non-empty string.")
  }

  if (username == "" || password == "") {
    stop("Username or password is not defined. Check your environment variables.")
  }

  # Construct upload URL
  file_name <- basename(file_path)
  upload_url <- glue("{upload_path}/{file_name}")

  # Try to upload the file
  tryCatch({
    raw_file <- readBin(file_path, what = "raw", n = file.info(file_path)$size)

    req <- webdav_create_request(base_url, upload_url, dav, username, password, verbose)

    req <- req %>%
      req_body_raw(raw_file) %>%
      req_method("PUT") %>%
      req_options(timeout = timeout)

    # Perform the upload request
    response <- req %>%
      req_perform()

    handle_response(response)

    message("File successfully uploaded to: ", upload_url, "\n")
    return(TRUE)

  }, error = function(e) {
    message("Error uploading the file: ", conditionMessage(e), "\n")
    return(FALSE)
  })
}


#' List files from a specific folder on OwnCloud server using WebDAV
#'
#' This function lists the files in a specific folder on the OwnCloud server using WebDAV, with authentication and error handling.
#'
#' @param base_url The base URL of the OwnCloud server (e.g., "_server_url_").
#' @param folder_path The path inside OwnCloud where the files are located.
#' @param dav The WebDAV base path (e.g., "_dav_").
#' @param username The username for OwnCloud authentication.
#' @param password The password for OwnCloud authentication.
#' @param depth The depth of the PROPFIND request (default is 1).
#' @param verbose A logical value indicating whether to print detailed debug messages. When set to TRUE, the function will output additional information about its progress and actions. Default is FALSE.
#'
#' @return A tibble with the file names and paths relative to the folder.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' webdav_list_files("_server_url_", "_folder_", "_dav_")
#' }
#' @export
webdav_list_files <- function(
                        base_url,
                        folder_path,
                        dav,
                        username = Sys.getenv("OWNCLOUD_USERNAME"),
                        password = Sys.getenv("OWNCLOUD_PASSWORD"),
                        depth = 1,
                        verbose = FALSE) {
  check_and_load_package("httr2")
  check_and_load_package("xml2")
  check_and_load_package("stringr")
  check_and_load_package("dplyr")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(folder_path) || !is.character(folder_path) || nchar(folder_path) == 0) {
    stop("The 'folder_path' parameter is required and must be a non-empty string.")
  }

  if (missing(dav) || !is.character(dav) || nchar(dav) == 0) {
    stop("The 'dav' parameter is required and must be a non-empty string.")
  }

  if (username == "" || password == "") {
    stop("Username or password is not defined. Check your environment variables.")
  }

  # Try to list files in the folder
  tryCatch({
    req <- webdav_create_request(base_url, folder_path, dav, username, password, verbose)

    req <- req %>%
      req_method("PROPFIND") %>%
      req_headers("Depth" = as.character(depth))

    # Perform the request
    response <- req %>%
      req_perform()

    handle_response(response)

    # Parse XML response to extract file names
    content <- response %>% resp_body_string()
    xml_content <- read_xml(content)

    raw <- xml_content %>%
      xml_find_all(".//d:href") %>%
      xml_text()

    # To pass cran notes: Undefined global functions or variables: (Ignore!)
    file_name = file_path = NULL

    files <- tibble(raw) %>%
      filter(!str_detect(raw, "/$")) %>%
      mutate(file_name = basename(raw),
             file_path = str_remove(raw, glue("(^.*{str_remove(folder_path, '/$')})"))) %>%
      mutate(file_path = str_remove(file_path, file_name)) %>%
      mutate(file_path = str_remove(file_path, "/(?=.)")) %>%
      select(-raw) %>%
      arrange(str_length(file_path), file_name)

    return(files)

  }, error = function(e) {
    message("Error listing files in the folder: ", conditionMessage(e), "\n")
    return(NULL)
  })
}



#' Delete a file or directory from the OwnCloud server using WebDAV
#'
#' This function deletes a file or directory on the OwnCloud server using the WebDAV DELETE method.
#'
#' @param base_url The base URL of the OwnCloud server (e.g., "_server_url_").
#' @param resource_path The path of the file or directory to delete.
#' @param dav The WebDAV base path (e.g., "_dav_").
#' @param username The username for OwnCloud authentication.
#' @param password The password for OwnCloud authentication.
#' @param verbose A logical value indicating whether to print detailed debug messages. When set to TRUE, the function will output additional information about its progress and actions. Default is FALSE.
#' @return Logical value indicating whether the file or directory was deleted successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange
#' @importFrom stringr str_detect str_remove str_length
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' webdav_delete_resource("_server_url_", "_file_", "_dav_")
#' }
#' @export
webdav_delete_resource <- function(base_url, resource_path, dav,
                                   username = Sys.getenv("OWNCLOUD_USERNAME"),
                                   password = Sys.getenv("OWNCLOUD_PASSWORD"),
                                   verbose = FALSE) {
  # Load necessary packages
  check_and_load_package("httr2")
  check_and_load_package("glue")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(resource_path) || !is.character(resource_path) || nchar(resource_path) == 0) {
    stop("The 'resource_path' parameter is required and must be a non-empty string.")
  }

  if (missing(dav) || !is.character(dav) || nchar(dav) == 0) {
    stop("The 'dav' parameter is required and must be a non-empty string.")
  }

  if (username == "" || password == "") {
    stop("Username or password is not defined. Check your environment variables.")
  }

  # Use webdav_create_request to create the request
  req <- webdav_create_request(base_url, resource_path, dav, username, password, verbose = verbose) %>%
    req_method("DELETE")

  # Handle the request using handle_response
  tryCatch({
    response <- req_perform(req)
    handle_response(response)

    message("Resource successfully deleted at: ", resource_path)
    return(TRUE)

  }, error = function(e) {
    message("Error deleting the resource: ", conditionMessage(e))
    return(FALSE)
  })
}
