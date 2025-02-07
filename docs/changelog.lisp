(uiop:define-package #:scrapycl-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:scrapycl-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.2.0 2025-02-07
         "
Changed
=======

These symbols are exported from SCRAPYCL package:

- SCRAPYCL:RESPONSE-URL
- SCRAPYCL:RESPONSE-STATUS
- SCRAPYCL:RESPONSE-BODY
- SCRAPYCL:RESPONSE-HEADERS

These are exported from SCRAPYCL/DOWNLOADER:

- SCRAPYCL/DOWNLOADER:RETRY-REQUEST
- SCRAPYCL/DOWNLOADER:RETRY-IF

Generic-function SCRAPYCL/DOWNLOADER:FETCH now has three optional arguments:

- METHOD argument allows to use HTTP methods other than default GET.
- CONTENT argument allows to send data to the server using these methods.
- INSECURE argument allows to ignore broken SSL certificate of the server.

Also, generic-function SCRAPYCL/DOWNLOADER:FETCH now setups SCRAPYCL/DOWNLOADER:RETRY-REQUEST restart
which can be called using SCRAPYCL/DOWNLOADER:RETRY-REQUEST function.

A function SCRAPYCL/DOWNLOADER:RETRY-IF was added to help call SCRAPYCL/DOWNLOADER:RETRY-REQUEST only
if some predicate on error condition returns T and number of attempts does not exceed a limit.

Fixes
=====

Parsing helper `merge-url-with` now will not fail if given node is NIL.
")
  (0.1.0 2024-03-17
         "* Initial version."))
