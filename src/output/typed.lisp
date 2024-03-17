(uiop:define-package #:scrapycl/output/typed
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:export #:typed-output))
(in-package #:scrapycl/output/typed)


(-> typed-output
    ((serapeum:soft-alist-of symbol function))
    (values function &optional))


(defun typed-output (type-to-output-alist)
  (flet ((process (object)
           (cond
             ((eql object 'scrapycl/output:stop-output)
              ;; This kind of object should be translated to all outputs
              (loop for item in type-to-output-alist
                    for output = (cdr item)
                    do (funcall output object)))
             (t
              (loop for (type . output) in type-to-output-alist
                    when (typep object type)
                    do (funcall output object)
                       (return))))))
    (values #'process)))
