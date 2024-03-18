#-asdf3.1 (error "scrapycl requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "scrapycl"
  :description "The web scraping framework for writing crawlers in Common Lisp."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/scrapycl/"
  :source-control (:git "https://github.com/40ants/scrapycl")
  :bug-tracker "https://github.com/40ants/scrapycl/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("scrapycl/engine"
               "scrapycl/request"
               "scrapycl/spider"
               "scrapycl/downloader"
               "scrapycl/utils"
               "scrapycl/errors"
               "scrapycl/output/json"
               "scrapycl/output/typed")
  :in-order-to ((test-op (test-op "scrapycl-tests"))))


(asdf:register-system-packages "log4cl" '("LOG"))
(asdf:register-system-packages "bordeaux-threads" '("BT2"))
(asdf:register-system-packages "dexador" '("DEX"))
