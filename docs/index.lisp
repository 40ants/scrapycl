(uiop:define-package #:scrapycl-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:scrapycl-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:scrapycl-docs/tutorial
                #:@tutorial)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:scrapycl-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "scrapycl-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "scrapycl - The web scraping framework for writing crawlers in Common Lisp."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "HTML"
                                   "CL"
                                   "TODO"
                                   "Unlicense"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "API"
                                   "URL"
                                   "SCRAPYCL/SPIDER::%SPIDER-QUEUE"
                                   "URI"
                                   "RPC"
                                   "GIT"))
  (scrapycl system)
  "
[![](https://github-actions.40ants.com/40ants/scrapycl/matrix.svg?only=ci.run-tests)](https://github.com/40ants/scrapycl/actions)

![Quicklisp](http://quickdocs.org/badge/scrapycl.svg)
"
  (@installation section)
  (@tutorial section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :scrapycl)
```
""")


(defautodoc @api (:system "scrapycl"))
