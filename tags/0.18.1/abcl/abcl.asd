;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

(require 'asdf)
(defpackage :abcl-asdf
  (:use :cl :asdf))
(in-package :abcl-asdf)

;;; Wrapper for all ABCL ASDF definitions.
(defsystem :abcl :version "0.5.0")

(defmethod perform :after ((o load-op) (c (eql (find-system :abcl))))
  (operate 'load-op :abcl-tests :force t)
  (operate 'load-op :abcl-test-lisp :force t)
  (operate 'load-op :cl-bench :force t)
  (operate 'load-op :ansi-compiled :force t)
  (operate 'load-op :ansi-interpreted :force t))

;;;  Run via (asdf:operate 'asdf:test-op :abcl :force t)
(defmethod perform ((o test-op) (c (eql (find-system :abcl))))
  (operate 'test-op :abcl-tests :force t))

;;; A collection of test suites for ABCL.
(defsystem :abcl-tests
  :version "2.0"
  :depends-on (:abcl-test-lisp 
               :ansi-compiled :ansi-interpreted
               :cl-bench))

(defmethod perfom :before ((o test-op (c (eql find-system :abcl-tests))))
  (operate 'load-op :abcl-test-lisp)
  (operate 'load-op :ansi-compiled)
  (operate 'load-op :cl-bench))

;;;  Run via (asdf:operate 'asdf:test-op :abcl-tests :force t)
(defmethod perform ((o test-op) (c (eql (find-system :abcl-tests))))
  ;; Additional test suite invocations would go here.
  (operate 'test-op :abcl-test-lisp) 
  (operate 'test-op :ansi-compiled)
  (operate 'test-op :cl-bench))

;;; Test ABCL with the Lisp unit tests collected in "test/lisp/abcl"
(defsystem :abcl-test-lisp :version "1.1" :components
	   ((:module abcl-rt :pathname "test/lisp/abcl/" :serial t :components
		     ((:file "rt-package") (:file "rt")))
	    (:module package  :depends-on (abcl-rt)
		     :pathname "test/lisp/abcl/" :components
		     ((:file "package")))))
(defmethod perform :before ((o test-op) (c (eql (find-system
                                                 :abcl-test-lisp))))
  (operate 'load-op :abcl-test-lisp :force t))
(defmethod perform ((o test-op) (c (eql (find-system 'abcl-test-lisp))))
   "Invoke tests with (asdf:oos 'asdf:test-op :abcl-test-lisp)."
   (funcall (intern (symbol-name 'run) :abcl-test)))

;;; Test ABCL with the interpreted ANSI tests
(defsystem :ansi-interpreted :version "1.0.1" 
           :components
           ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")))))
(defmethod perform :before ((o test-op) (c (eql (find-system :ansi-interpreted))))
  (operate 'load-op :ansi-interpreted))
(defmethod perform ((o test-op) (c (eql (find-system :ansi-interpreted))))
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests nil))

;;; Test ABCL with the compiled ANSI tests
(defsystem :ansi-compiled :version "1.0.1" 
           :components
           ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")))))
(defmethod perform :before ((o test-op) (c (eql (find-system :ansi-compiled))))
  (operate 'load-op :ansi-compiled))
(defmethod perform ((o test-op) (c (eql (find-system :ansi-compiled))))
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests t))


;;; Test ABCL with CL-BENCH 
(defsystem :cl-bench :components
           ((:module cl-bench-package :pathname "../cl-bench/"
                    :components ((:file "defpackage")))
            (:module cl-bench-wrapper :pathname "test/lisp/cl-bench/" 
                     :depends-on (cl-bench-package) :components
                     ((:file "wrapper")))))
(defmethod perform :before ((o test-op) (c (eql (find-system :cl-bench))))
  (operate 'load-op :cl-bench :force t))
(defmethod perform ((o test-op) (c (eql (find-system :cl-bench))))
  (funcall (intern (symbol-name 'run) :abcl.test.cl-bench)))
 
;;; Build ABCL from a Lisp.
;;; aka the "Lisp-hosted build system"
;;; Works for: abcl, sbcl, clisp, cmu, lispworks, allegro, openmcl
(defsystem :build-abcl :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))



