(in-package :cl-user)

(require :asdf)
(require :swank)
(require :shell-utility)

;; Start swank server

(shell-utility:write-shell-utility :sbcl
  (:name "/home/username/bin/start-swank-server")
  (:repl) ;; No --quit or --disable-debugger
  (:load-system :swank)
  (:eval (swank:create-server)))

;; Compile and test a single system.

(shell-utility:write-shell-utility :sbcl
  (:name "/home/username/bin/test-asdf-system")
  (:eval (defun test-system/example (rest)
           (destructuring-bind (system) rest
             (asdf:test-system system))))
  (:launch test-system/example))

;; Prepare a dev core for a system. Suitable for C-u M-x slime

(shell-utility:write-shell-utility :sbcl
  (:name "/home/username/bin/save-dev-core")

  ;; This could go into a separate file that gets loaded instead.
  (:eval (defun save-dev-core/example (rest)
           (destructuring-bind (system) rest
             (asdf:operate 'asdf::prepare-op system)
             ;; SBCL contribs can't be found later so require them here.
             (require :sb-cltl2)
             (require :sb-posix)
             (push (lambda ()
                     (sb-ext:enable-debugger)
                     (asdf:test-system system))
                   sb-ext:*init-hooks*)
             (save-lisp-and-die (format nil "dev-~A.core" system)
                                :executable t
                                :compression t))))

  (:launch save-dev-core/example))
