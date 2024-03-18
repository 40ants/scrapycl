(uiop:define-package #:scrapycl/types
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:url)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package))
(in-package #:scrapycl/types)


(deftype url ()
  "Represents a URL."
  'string)
