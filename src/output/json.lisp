(uiop:define-package #:scrapycl/output/json
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:yason)
  (:import-from #:bt2)
  (:import-from #:closer-mop
                #:class-slots)
  (:import-from #:scrapycl/core
                #:stop-output
                #:write-as-json
                #:json-lines
                #:json-list
                #:json-dict))
(in-package #:scrapycl/output/json)


(defgeneric write-as-json (object stream)
  (:method ((object t) (stream t))
    (yason:encode object stream)
    (values))
  
  (:method ((object standard-object) (stream t))
    (let* ((obj-class (class-of object))
           (slots (class-slots obj-class)))
      (loop with hash-table = (make-hash-table :test 'equal)
            for slot in slots
            for slot-name = (closer-mop:slot-definition-name slot)
            for key = (string-downcase slot-name)
            when (and (not (str:starts-with-p "%" key))
                      (slot-boundp object slot-name))
            do (setf (gethash key hash-table)
                     (slot-value object slot-name))
            finally (yason:encode hash-table stream))
      (values))))


(-> make-json-output
    ((or string pathname)
     &key (:if-exists (or null keyword))
     (:after-stream-opened (or null function))
     (:before-stream-closed (or null function))
     (:before-first-object (or null function))
     (:before-each-object (or null function)))
    (values function &optional))


(defun make-json-output (filename &key (if-exists :supersede)
                                       (after-stream-opened nil)
                                       (before-stream-closed nil)
                                       (before-first-object nil before-first-object-given-p)
                                       (before-each-object nil))
  "Internal function to reuse logic of stream opening and closing."
  (let ((stream nil)
        (stream-closed nil)
        (first-object t)
        (lock (bt2:make-lock :name (fmt "Lock on ~A" filename))))
    (labels ((ensure-stream-is-opened ()
               (unless stream
                 (setf stream
                       (open filename
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists if-exists))
                 (when after-stream-opened
                   (funcall after-stream-opened stream))))
               
             (close-stream ()
               (when stream
                 (when before-stream-closed
                   (funcall before-stream-closed stream))
                 (close stream)
                 (setf stream nil)
                 (setf stream-closed
                       t)))
               
             (serialize (object)
               (bt2:with-lock-held (lock)
                 (when stream-closed
                   (error "Stream to ~A was closed. Create a new JSON output."
                          filename))
                   
                 (ensure-stream-is-opened)
                 (cond
                   ((eql object 'stop-output)
                    (close-stream))
                   (t
                    (let ((hook
                            (cond
                              (first-object
                               (setf first-object nil)
                               ;; Check for before-first-object-given-p
                               ;; allows us to pass NIL in before-first-object
                               ;; argument to suppress any action for first item:
                               (if before-first-object-given-p
                                   before-first-object
                                   before-each-object))
                              (t
                               before-each-object))))
                      (when hook
                        (funcall hook stream)))
                    (write-as-json object stream))))))
      (values #'serialize))))



(-> json-lines
    ((or string pathname) &key (:if-exists (or null keyword)))
    (values function &optional))

(defun json-lines (filename &key (if-exists :supersede))
  (make-json-output filename
                    :if-exists if-exists
                    :before-first-object nil
                    :before-each-object #'terpri))


(-> json-list
    ((or string pathname))
    (values function &optional))

(defun json-list (filename)
  (let ((first-item t))
    (flet ((write-header (stream)
             (write-string "[" stream))
           (write-footer (stream)
             (write-string "]" stream))
           (write-a-comma-if-needed (stream)
             (cond
               (first-item (setf first-item nil))
               (t
                (write-string ", " stream)))))
      (make-json-output filename
                        :after-stream-opened #'write-header
                        :before-stream-closed #'write-footer
                        :before-each-object #'write-a-comma-if-needed))))


(-> json-dict
    ((or string pathname)
     &key (:key string))
    (values function &optional))

(defun json-dict (filename &key (key "items"))
  "Creates an \"output\" callback for serializing objects as a list inside a JSON dictionary."
  (let ((first-item t))
    (flet ((write-header (stream)
             (write-string "{\"" stream)
             (write-string key stream)
             (write-string "\": [" stream))
           (write-footer (stream)
             (write-string "]}" stream))
           (write-a-comma-if-needed (stream)
             (cond
               (first-item (setf first-item nil))
               (t
                (write-string ", " stream)))))
      (make-json-output filename
                        :after-stream-opened #'write-header
                        :before-stream-closed #'write-footer
                        :before-each-object #'write-a-comma-if-needed))))
