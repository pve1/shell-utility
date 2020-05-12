;;;; shell-utility.lisp
(uiop:define-package :shell-utility/tests
    (:use :cl :alexandria :fiveam :shell-utility))

(in-package #:shell-utility/tests)

(in-suite* all-tests)

(defun run-tests ()
  (fiveam:run! 'all-tests))

(def-test eval ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:eval (+ 1 2))))
             "#!/bin/sh
sbcl \\
--noinform \\
--disable-debugger \\
--quit \\
--eval '(+ 1 2)' \\
\"$@\"
")))

(def-test eval-print ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:eval-print (+ 1 2))))
             "#!/bin/sh
sbcl \\
--noinform \\
--disable-debugger \\
--quit \\
--eval '(PRINT (+ 1 2))' \\
\"$@\"
")))

(def-test load-system ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:load-system :foo)))
             "#!/bin/sh
sbcl \\
--noinform \\
--disable-debugger \\
--quit \\
--eval '(ASDF:LOAD-SYSTEM \"foo\")' \\
\"$@\"
")))


(def-test directory ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:directory "/home/foo/project/")
                (:load "patches.lisp")
                (:core "a-core")))
"#!/bin/sh
sbcl \\
--core '/home/foo/project/a-core' \\
--noinform \\
--disable-debugger \\
--quit \\
--load '/home/foo/project/patches.lisp' \\
\"$@\"
"))

  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:cd "/home/foo/project/")
                (:core "a-core")
                (:load "patches.lisp")))
"#!/bin/sh
cd \"/home/foo/project/\"
sbcl \\
--core 'a-core' \\
--noinform \\
--disable-debugger \\
--quit \\
--load 'patches.lisp' \\
\"$@\"
")))

(def-test save-core ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:directory "/home/foo/project/")
                (:load-system :foo)
                (:load "patches.lisp")
                (:save-core "a-core")))
             "#!/bin/sh
sbcl \\
--noinform \\
--disable-debugger \\
--quit \\
--eval '(ASDF:LOAD-SYSTEM \"foo\")' \\
--load '/home/foo/project/patches.lisp' \\
--eval '(SAVE-LISP-AND-DIE #P\"/home/foo/project/a-core\" :COMPRESSION T :EXECUTABLE T)' \\
\"$@\"
")))

(def-test launch ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:launch foo)))
             "#!/bin/sh
sbcl \\
--noinform \\
--disable-debugger \\
--quit \\
--eval '(ASDF:LOAD-SYSTEM \"shell-utility\")' \\
--eval '(SHELL-UTILITY::LAUNCH/2 (QUOTE SHELL-UTILITY::SBCL) (QUOTE SHELL-UTILITY/TESTS::FOO))' \\
\"$@\"
")))

(def-test repl ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                :repl))
             "#!/bin/sh
sbcl \\
--noinform \\
\"$@\"
"))

  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:repl)))
             "#!/bin/sh
sbcl \\
--noinform \\
\"$@\"
")))

(def-test option ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:option "foo" "bar")))
"#!/bin/sh
sbcl \\
--noinform \\
--disable-debugger \\
--quit \\
--foo 'bar' \\
\"$@\"
")))

(def-test clear-lisp-options ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:clear-lisp-options)))
"#!/bin/sh
sbcl \\
\"$@\"
")))

(def-test executable-name ()
  (is (equal (shell-utility::generate-shell-script-string
              (shell-utility :sbcl
                (:executable "foo")))
"#!/bin/sh
foo \\
--noinform \\
--disable-debugger \\
--quit \\
\"$@\"
")))
