(defsystem "scrapycl-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/scrapycl/"
  :class :package-inferred-system
  :description "Provides tests for scrapycl."
  :source-control (:git "https://github.com/40ants/scrapycl")
  :bug-tracker "https://github.com/40ants/scrapycl/issues"
  :pathname "t"
  :depends-on ("scrapycl-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
