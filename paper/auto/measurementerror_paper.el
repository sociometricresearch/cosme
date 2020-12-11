(TeX-add-style-hook
 "measurementerror_paper"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8")))
   (TeX-run-style-hooks
    "latex2e"
    "jss"
    "jss10"
    "inputenc"
    "amsmath")
   (TeX-add-symbols
    "tightlist")
   (LaTeX-add-labels
    "introduction"
    "literature-on-measurement-error"
    "correction-for-simple-concepts"
    "correction-for-simple-concepts-with-complex-concepts"
    "correct-for-complex-concepts-and-complex-concepts"
    "applications-and-illustrations"
    "political-trust-example"
    "read-the-data"
    "analysis"
    "correlations-and-correcting-for-measurement-error"
    "regression-model"
    "summary"
    "acknowledgments"
    "computational-details")
   (LaTeX-add-bibliographies
    "bibliography.bib"))
 :latex)

