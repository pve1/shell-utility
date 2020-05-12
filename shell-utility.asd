;;;; shell-utility.asd

(asdf:defsystem #:shell-utility
  :description "A library that let's you generate shell scripts from SBCL."
  :author "Peter von Etter <your.name@example.com.invalid>"
  :license  "LLGPL"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "shell-utility"))
  :depends-on (:alexandria :apply-argv)
  :in-order-to ((test-op (test-op "shell-utility/tests"))))

(asdf:defsystem #:shell-utility/tests
  :serial t
  :components ((:file "shell-utility-tests"))
  :depends-on (:shell-utility :fiveam :alexandria)
  :perform (test-op (o s)
                    (uiop:symbol-call
                     "FIVEAM" "RUN!"
                     (uiop:find-symbol* "ALL-TESTS"
                                        "SHELL-UTILITY/TESTS"))))
