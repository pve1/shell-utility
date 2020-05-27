;;;; shell-utility.lisp

(in-package #:shell-utility)

;;; ====================================================================
;;;   Shell Utility

(defclass shell-utility ()
  ((executable-name :initarg :executable-name
                    :accessor executable-name
                    :initform nil)
   (options :initarg :options
            :accessor options
            :initform nil)
   (default-directory :initarg :default-directory
                      :accessor %default-directory
                      :initform nil)
   (working-directory :initarg :working-directory
                      :accessor working-directory
                      :initform nil)
   (implicit-commands :initarg :implicit-commands
                      :accessor implicit-commands
                      :initform nil)
   (commands :initarg :commands
             :accessor commands
             :initform nil)
   (script-file :initarg :script-file
                :accessor script-file
                :initform nil)
   (shebang :initarg :shebang
            :accessor shebang
            :initform "#!/bin/sh")
   (launch-options :initarg :launch-options
                   :accessor launch-options
                   :initform nil)))

(defmethod default-directory ((s shell-utility))
  (or (%default-directory s)
      (make-pathname)))

(defun resolve-implementation-symbol (keyword)
  (ecase keyword
    (:sbcl 'sbcl)))

;;; ====================================================================
;;;   SBCL

(defclass sbcl (shell-utility)
  ((executable-name :initform "sbcl")
   (options :initform (list (option "noinform")
                            (option "disable-debugger")
                            (option "quit")))
   (core-compression :initarg :core-compression
                     :accessor core-compression
                     :initform t)))

;;; ====================================================================
;;;   Options

;; This class is used both for lisp options and commands for
;; convenience.

(defclass lisp-option ()
  ((name :initarg :name
         :accessor name
         :initform nil)
   (value :initarg :value
          :accessor value
          :initform nil)))

(defgeneric has-option-p (shell-utility option)
  (:method ((s shell-utility) (option lisp-option))
    (find (name option) (options s) :key #'name :test #'equal))
  (:method ((s shell-utility) (option string))
    (find option (options s) :key #'name :test #'equal)))

(defgeneric remove-option (shell-utility option)
  (:method ((s shell-utility) (option lisp-option))
    (remove-option s (name option)))
  (:method ((s shell-utility) (option string))
    (setf (options s) (remove option (options s)
                              :key #'name :test #'equal))))

(defgeneric add-option (shell-utility option &optional last)
  (:method ((s shell-utility) (option lisp-option) &optional last)
    (unless (has-option-p s option)
      (if last
          (setf (options s) (append (options s) (list option)))
          (push option (options s))))))

(defun option (name &optional value)
  (check-type name string)
  (make-instance 'lisp-option :name name :value value))

(defgeneric as-string (thing)
  (:method ((s string)) s))

;; Probably implementation dependent. At least an option that outputs
;; single dash flags ("-f" etc.) is needed.

(defmethod as-string ((option lisp-option))
  (if (value option)
      (format nil "--~A '~A'" (name option)
              (escape-string
               (typecase (value option)
                 (string (value option))
                 ((or symbol list) (with-standard-io-syntax
                                     (with-output-to-string (s)
                                       (prin1 (value option) s)))))
               "$'" #\\))
      (format nil "--~A" (name option))))

(defun escape-string (string characters-to-escape escape-char)
  (let* ((escape-count 0)
         (new-len 0)
         (new-string))

    (loop :for c :across string
          :do (cond ((find c characters-to-escape)
                     (incf escape-count)
                     (incf new-len 2))
                    (t (incf new-len))))

    (if (zerop escape-count)
        (return-from escape-string string)
        (setf new-string (make-string new-len)))

    (loop :for c :across string
          :for i :from 0
          :do (cond ((find c characters-to-escape)
                     (setf (aref new-string i) escape-char)
                     (incf i)
                     (setf (aref new-string i) c))
                    (t (setf (aref new-string i) c))))
    new-string))

;;; ====================================================================
;;;   Top-level functions

(defun shell-utility* (class &rest commands)
  (make-instance (resolve-implementation-symbol class)
                 :commands commands))

(export 'shell-utility*)

(defmacro shell-utility (class &body commands)
  `(apply 'shell-utility* ',class ',commands))

(export 'shell-utility)

(defun write-shell-utility* (class &rest commands)
  (let ((s (apply 'shell-utility* class commands)))
    (write-shell-script s)))

(export 'write-shell-utility*)

(defmacro write-shell-utility (class &body commands)
  `(apply 'write-shell-utility* ',class ',commands))

(export 'write-shell-utility)


;;; ====================================================================
;;;   Launch

;; Launch a function with a parsed argv and a catch-all handler-case.

;; Example init-function:

;; (defun my-init (rest &key foo bar)
;;   ... )

;; myscript --foo --bar 1 blah blah

;; Or:

;; (defun my-init (rest)
;;   ... )

;; myscript foo bar

(defgeneric launch (function-designator)
  (:method ((s symbol))
    (launch/2 (make-instance 'shell-utility) s))
  (:method ((s function))
    (launch/2 (make-instance 'shell-utility) s)))

(defgeneric launch/2 (shell-utility function-designator)
  (:method ((s symbol) (symbol symbol))
    (launch/2 (make-instance s) (fdefinition symbol)))

  (:method ((s shell-utility) (symbol symbol))
    (launch/2 s (fdefinition symbol)))

  (:method ((s shell-utility) (function function))
    (handler-case (call-init-function s function)
      (serious-condition (x) (format t "~%Received ~A~%" x)))))

(defgeneric call-init-function (shell-utility function-designator)
  (:method ((s shell-utility) (function-designator symbol))
    (call-init-function s (fdefinition function-designator)))

  (:method ((s shell-utility) (function-designator function))
    (let* ((argv (apply-argv:parse-argv (apply-argv:get-argv)))
           (rest-keyword (getf (launch-options s) :rest-keyword)))

      (cond ((and rest-keyword (consp (first argv)))
             (apply function-designator rest-keyword (first argv)
                    (rest argv)))

            (t (apply function-designator argv))))))

;;; ====================================================================
;;;   Preprocess

;; Preprocess commands modify the shell utility in various ways, like
;; the name of the script. They are processed before the script string
;; is generated.

(defgeneric preprocess-command/2r (shell-utility command &rest arguments)
  (:method ((s shell-utility) command &rest arguments)
    (declare (ignore arguments))
    nil))

(defgeneric preprocess-command (shell-utility command)
  (:method ((s shell-utility) (command list))
    (apply #'preprocess-command/2r s (first command) (rest command)))
  (:method ((s shell-utility) (command symbol))
    (preprocess-command/2r s command)))

(defgeneric preprocess-commands (shell-utility)
  (:method ((s shell-utility))
    (dolist (c (commands s))
      (preprocess-command s c))))

(defmacro define-preprocess-command ((var class) (command &rest arguments) &body body)
  (with-gensyms (args)
    `(defmethod preprocess-command/2r ((,var ,class)
                                       (command (eql ',command))
                                       &rest ,args)
       (destructuring-bind ,arguments ,args
         ,@body))))

;;; ====================================================================
;;;   General preprocess commands

(define-preprocess-command (s shell-utility) (:executable name &key merge-default-dir-p)
  (if merge-default-dir-p
      (setf (executable-name s) (merge-pathnames name (default-directory s)))
      (setf (executable-name s) name)))

;; Affects :name, :core, :load, :save-core
(define-preprocess-command (s shell-utility) (:directory dir)
  (setf (%default-directory s) (namestring dir))
  nil)

;; Working directory when running script
(define-preprocess-command (s shell-utility) (:cd dir)
  (if (eq dir :here)
      (setf (working-directory s) "`dirname $(readlink -fn $0)`")
      (setf (working-directory s) (namestring dir)))
  nil)

(define-preprocess-command (s shell-utility) (:clear-lisp-options)
  (setf (options s) nil))

(define-preprocess-command (s shell-utility) (:lisp-option name &optional value)
  (add-option s (option name value) t))

;; Name of the script file
(define-preprocess-command (s shell-utility) (:name name)
  (setf (script-file s) (merge-pathnames name (default-directory s)))
  nil)

;;; ====================================================================
;;;   Preprocess commands for SBCL

;; Load a core
(define-preprocess-command (s sbcl) (:core core)
  (add-option s (option "core"
                        (namestring
                         (merge-pathnames
                          core (default-directory s))))))

(define-preprocess-command (s sbcl) (:quit)
  (add-option s (option "quit")))

;; Interactive mode
(define-preprocess-command (s sbcl) (:repl)
  (remove-option s "quit")
  (remove-option s "disable-debugger"))

;;; ====================================================================
;;;   Interpret commands

(defgeneric interpret-command/2r (shell-utility command &rest arguments)
  (:method ((s shell-utility) command &rest arguments)
    (declare (ignore arguments))
    nil ;; Fails on preprocess commands, so just ignore them for now.
    #+(or) (error "Command ~A not defined." command)))

(defgeneric interpret-command (shell-utility command)
  (:method ((s shell-utility) (command list))
    (apply #'interpret-command/2r s (first command) (rest command)))
  (:method ((s shell-utility) (command symbol))
    (interpret-command/2r s command)))

(defmacro define-command ((var class) (command &rest arguments) &body body)
  (with-gensyms (args)
    `(defmethod interpret-command/2r ((,var ,class)
                                      (command (eql ',command))
                                      &rest ,args)
       (destructuring-bind ,arguments ,args
         ,@body))))

;;; ====================================================================
;;;   General commands

(define-command (s shell-utility) (:option name &optional value)
  (check-type name string)
  (option name value))


;;; ====================================================================
;;;   Commands for SBCL

(define-command (s sbcl) (:load-system system)
  (check-type system (or string symbol))
  (option "eval" (format nil "(ASDF:LOAD-SYSTEM ~S)" (string-downcase system))))

(define-command (s sbcl) (:eval form)
  (option "eval" form))

(define-command (s sbcl) (:load file)
  (check-type file string)
  (option "load" (namestring (merge-pathnames file (default-directory s)))))

(define-command (s sbcl) (:eval-print form)
  (option "eval" `(print ,form)))

(define-command (s sbcl) (:launch form &key rest-keyword)
  (list (interpret-command/2r s :load-system "shell-utility")
        (if rest-keyword
            (option "eval" `(launch/2
                             (make-instance 'sbcl
                                            :launch-options '(:rest-keyword
                                                              ,rest-keyword))
                             ',form))
            (option "eval" `(launch/2 'sbcl ',form)))))

(define-command (s sbcl) (:save-core core-name &rest rest)
  (setf core-name (merge-pathnames core-name (default-directory s)))
  (interpret-command/2r s :eval `(sb-ext:save-lisp-and-die
                                  ,core-name
                                  ,@rest
                                  :compression ,(core-compression s)
                                  :executable t)))

;;; ====================================================================
;;;   Shell script creation

(defmethod generate-shell-script-string ((s shell-utility))
  (preprocess-commands s)
  (with-output-to-string (out)
    (flet ((newline ()
             (princ " \\" out)
             (terpri out)))

      (princ (shebang s) out)
      (terpri out)

      (when (working-directory s)
        (format out "cd ~S~%" (working-directory s)))

      (princ (executable-name s) out)
      (newline)

      (dolist (o (options s))
        (princ (as-string o) out)
        (newline))

      (when (or (implicit-commands s)
                (commands s))
        (loop :for c :in (append (implicit-commands s)
                                 (commands s))
              :for options = (ensure-list (interpret-command s c))
              :do (when options
                    (dolist (o options)
                      (princ (as-string o) out)
                      (newline)))))

      (princ "\"$@\"" out)
      (terpri out))))

;; Note :if-exists :supersede
(defgeneric write-shell-script (shell-utility)
  (:method ((su shell-utility))
    (let* ((instance (generate-shell-script-string su)))
      (check-type (script-file su) (or pathname string))
      (prog1 (with-output-to-file (s (merge-pathnames (script-file su)
                                                      (default-directory su))
                                     :if-exists :supersede)
               (princ instance s))
        (format t "Wrote ~A.~%" (script-file su))
        (uiop:run-program (list "chmod" "u+x" (namestring
                                               (merge-pathnames
                                                (script-file su)
                                                (default-directory su)))))))))
