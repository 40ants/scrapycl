(uiop:define-package #:scrapycl
  (:use #:cl)
  (:nicknames #:scrapycl/core)
  (:export #:spider
           #:start
           #:url
           #:ensure-engine-is-running
           #:process
           #:enqueue
           #:request
           #:fetch))
(in-package #:scrapycl)


(deftype url ()
  'string)
