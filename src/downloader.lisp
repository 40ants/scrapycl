(uiop:define-package #:scrapycl/downloader
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:spider
                #:fetch
                #:request
                #:request-url
                #:fetch-error)
  (:import-from #:dex))
(in-package #:scrapycl/downloader)


(defgeneric fetch (spider request &key max-redirects timeout custom-headers)
  (:documentation "Fetches page from request's URL.

                   Returns a multiple values:

                   - A string with HTML response.
                   - URL from which response was received. Might be different from original URL because of redirects.
                   - A hash-table with reponse HTTP headers.")
  (:method ((spider spider) (request request) &key max-redirects
                                                   timeout
                                                   (custom-headers '(("User-Agent" . "ScrapyCL (https://40ants.com/scrapycl/)"))))
    (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
      (multiple-value-bind (body status headers last-uri)
          (dex:get (request-url request)
                   :max-redirects (or max-redirects 5)
                   :read-timeout timeout
                   :connect-timeout timeout
                   :headers custom-headers)
        (unless (= status 200)
          (error 'fetch-error
                 :url last-uri
                 :code status
                 :body body
                 :headers headers))
        (values body
                last-uri
                headers)))))
