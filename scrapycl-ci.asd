(defsystem "scrapycl-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/scrapycl/"
  :class :package-inferred-system
  :description "Provides CI settings for scrapycl."
  :source-control (:git "https://github.com/40ants/scrapycl")
  :bug-tracker "https://github.com/40ants/scrapycl/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "scrapycl-ci/ci"))
