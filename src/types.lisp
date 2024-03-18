(uiop:define-package #:scrapycl/types
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:url))
(in-package #:scrapycl/types)


(deftype url ()
  "Represents a URL."
  'string)
