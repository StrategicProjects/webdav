## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a new release.


Dear Konstanze Lauseker,

Thank you very much for your valuable feedback and suggestions. I have carefully reviewed your comments and made the necessary corrections as outlined below:

>	1.	Single quotes around HTTP methods:

I have removed the single quotes around the HTTP methods (‘COPY’, ‘LOCK’, ‘MKCOL’, ‘MOVE’, and ‘PROPFIND’) as requested. Single quotes are now only used around software or package names.

>	2.	Web reference in the DESCRIPTION file:

I have added the web reference for the API in the form <https://.....>, ensuring that the link is automatically generated without spaces after ‘https:’.

>	3.	Examples wrapped in \dontrun{}:

While addressing the other points, I have updated the examples to utilize a public WebDAV server for testing. This allows the examples to be executed without requiring private authentication credentials. The public server provides a controlled environment to demonstrate functionality, while avoiding issues related to access restrictions. As such, the examples are no longer wrapped in \dontrun{}, and can now be fully executed during testing.
