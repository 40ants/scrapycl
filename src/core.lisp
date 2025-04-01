(uiop:define-package #:scrapycl
  (:use #:cl)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package)
  (:nicknames #:scrapycl/core)
  (:export #:spider
           #:start
           #:process
           #:enqueue
           #:request
           #:fetch)
  (:export #:url)
  (:export #:stop-output)
  (:export #:fetch-error
           #:scrapycl-error
           #:response-url
           #:response-status
           #:response-body
           #:response-headers)
  (:export #:request-url
           #:request-dont-filter
           #:request)
  (:export #:typed-output)
  (:export #:json-lines
           #:json-list
           #:json-dict
           #:write-as-json)
  (:export #:preview))
(in-package #:scrapycl)


(ignore-words-in-package 'stop-output
                         'url)
