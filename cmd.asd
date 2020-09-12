;;;; cmd.asd

(defsystem "cmd"
  :description "A DSL for running external programs"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :class :package-inferred-system
  :depends-on ("cmd/cmd")
  :in-order-to ((test-op (load-op "cmd/test")))
  :perform (test-op (o c) (symbol-call :cmd/test :run-tests))
  :components ((:file "cmd")))
