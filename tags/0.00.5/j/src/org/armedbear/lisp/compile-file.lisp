;;; compile-file.lisp
;;;
;;; Copyright (C) 2004-2005 Peter Graves
;;; $Id: compile-file.lisp,v 1.91 2005-05-25 23:55:41 piso Exp $
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(in-package #:system)

(require 'jvm)

(defvar *fbound-names*)

(defvar *class-number*)

(defvar *output-file-pathname*)

(defun next-classfile-name ()
  (let ((name (%format nil "~A-~D"
                       (pathname-name *output-file-pathname*)
                       (incf *class-number*))))
    (namestring (merge-pathnames (make-pathname :name name :type "cls")
                                 *output-file-pathname*))))

(defmacro report-error (&rest forms)
  `(handler-case (progn ,@forms)
     (compiler-unsupported-feature-error (condition)
       (fresh-line)
       (%format t "; UNSUPPORTED-FEATURE: ~A~%" condition)
       (values nil condition))
     #+nil
     (error (condition)
       (fresh-line)
       (%format t "~A Note: ~A~%" (jvm::load-verbose-prefix) condition)
       (values nil condition))
     ))

;; Dummy function. Should never be called.
(defun dummy (&rest ignored)
  (assert nil))

(defun verify-load (classfile)
  (and classfile
       (let ((*load-truename* *output-file-pathname*))
         (report-error
          (load-compiled-function classfile)))))

(defun dump-list (object stream)
  (cond ((and (eq (car object) 'QUOTE) (= (length object) 2))
         (write-char #\' stream)
         (dump-object (cadr object) stream))
        (t
         (write-char #\( stream)
         (loop
           (dump-object (car object) stream)
           (setf object (cdr object))
           (when (null object)
             (return))
           (when (> (charpos stream) 80)
             (terpri stream))
           (write-char #\space stream)
           (when (atom object)
             (write-char #\. stream)
             (write-char #\space stream)
             (dump-object object stream)
             (return)))
         (write-char #\) stream))))

(defun dump-vector (object stream)
  (write-string "#(" stream)
  (let ((length (length object)))
    (when (> length 0)
      (dotimes (i (1- length))
        (dump-object (aref object i) stream)
        (when (> (charpos stream) 80)
          (terpri stream))
        (write-char #\space stream))
      (dump-object (aref object (1- length)) stream))
    (write-char #\) stream)))

(defun dump-structure (object stream)
  (multiple-value-bind (creation-form initialization-form)
      (make-load-form object)
    (write-string "#." stream)
    (if initialization-form
        (let* ((instance (gensym))
               load-form)
          (setf initialization-form
                (subst instance object initialization-form))
          (setf initialization-form
                (subst instance (list 'quote instance) initialization-form
                       :test #'equal))
          (setf load-form `(progn
                             (let ((,instance ,creation-form))
                               ,initialization-form
                               ,instance)))
          (dump-object load-form stream))
        (dump-object creation-form stream))))

(defun dump-object (object stream)
  (cond ((consp object)
         (dump-list object stream))
        ((stringp object)
         (write object :stream stream))
        ((bit-vector-p object)
         (write object :stream stream))
        ((vectorp object)
         (dump-vector object stream))
        ((structure-object-p object)
         (dump-structure object stream))
        (t
         (write object :stream stream))))

(defun dump-form (form stream)
  (when (and (consp form) (neq (%car form) 'QUOTE))
    (let ((*print-fasl* t)
          (*print-level* nil)
          (*print-length* nil)
          (*print-circle* nil))
      (if (eq (car form) 'IMPORT)
          ;; Make sure package prefix is printed when symbols are imported.
          (let ((*package* +keyword-package+))
            (dump-object form stream))
          (dump-object form stream)))
    (terpri stream)))

(defun process-defconstant (form stream)
  ;; "If a DEFCONSTANT form appears as a top level form, the compiler
  ;; must recognize that [the] name names a constant variable. An
  ;; implementation may choose to evaluate the value-form at compile
  ;; time, load time, or both. Therefore, users must ensure that the
  ;; initial-value can be evaluated at compile time (regardless of
  ;; whether or not references to name appear in the file) and that
  ;; it always evaluates to the same value."
  (eval form)
  (cond ((structure-object-p (third form))
         (multiple-value-bind (creation-form initialization-form)
             (make-load-form (third form))
           (dump-form (list 'DEFCONSTANT (second form) creation-form) stream)))
        (t
         (dump-form form stream))))

(defun process-toplevel-form (form stream compile-time-too)
  (cond ((atom form)
         (when compile-time-too
           (eval form)))
        (t
         (let ((first (first form)))
           (case (car form)
             (MACROLET
              (process-toplevel-macrolet form stream compile-time-too)
              (return-from process-toplevel-form))
             ((IN-PACKAGE DEFPACKAGE)
              (eval form))
             ((DEFVAR DEFPARAMETER)
              (if compile-time-too
                  (eval form)
                  ;; "If a DEFVAR or DEFPARAMETER form appears as a top level form,
                  ;; the compiler must recognize that the name has been proclaimed
                  ;; special. However, it must neither evaluate the initial-value
                  ;; form nor assign the dynamic variable named NAME at compile
                  ;; time."
                  (let ((name (second form)))
                    (%defvar name))))
             (DEFCONSTANT
              (process-defconstant form stream)
              (return-from process-toplevel-form))
             (DEFUN
              (let* ((name (second form))
                     (block-name (fdefinition-block-name name)))
                (when *compile-print*
                  (format t "; Processing function ~A~%" name))
                (let* ((lambda-list (third form))
                       (body (nthcdr 3 form))
                       (jvm::*speed* jvm::*speed*)
                       (jvm::*safety* jvm::*safety*)
                       (jvm::*debug* jvm::*debug*))
                  (multiple-value-bind (body decls)
                      (parse-body body)
                    (let* ((expr `(lambda ,lambda-list ,@decls (block ,block-name ,@body)))
                           (classfile-name (next-classfile-name))
                           (classfile (report-error
                                       (jvm:compile-defun name expr nil classfile-name)))
                           (compiled-function (verify-load classfile)))
                      (cond (compiled-function
                             (when *compile-print*
                               (format t ";  ~A => ~A.cls~%" name
                                       (pathname-name (pathname classfile-name))))
                             (setf form
                                   `(fset ',name
                                          (load-compiled-function ,(file-namestring classfile))
                                          ,*source-position*
                                          ',lambda-list))
                             (when compile-time-too
                               (fset name compiled-function)))
                            (t
                             ;; FIXME This should be a warning or error of some sort...
                             (format *error-output* "; Unable to compile function ~A~%" name)
                             (let ((precompiled-function (precompile-form expr nil)))
                               (setf form
                                     `(fset ',name
                                            ,precompiled-function
                                            ,*source-position*
                                            ',lambda-list)))
                             (when compile-time-too
                               (eval form))))))
                  (push name jvm::*functions-defined-in-current-file*)
                  (jvm::note-name-defined name)
                  ;; If NAME is not fbound, provide a dummy definition so that
                  ;; getSymbolFunctionOrDie() will succeed when we try to verify that
                  ;; functions defined later in the same file can be loaded correctly.
                  (unless (fboundp name)
                    (setf (fdefinition name) #'dummy)
                    (push name *fbound-names*)))))
             ((DEFGENERIC DEFMETHOD)
              (when *compile-print*
                (let ((*print-length* 2)
                      (*print-level* 2))
                  (format t "; Processing ~S~%" form)))
              (jvm::note-name-defined (second form))
              (process-toplevel-form (macroexpand-1 form *compile-file-environment*)
                                     stream compile-time-too)
              (return-from process-toplevel-form))
             (DEFMACRO
              (let ((name (second form)))
                (when *compile-print*
                  (format t "; Processing macro ~A~%" name))
                (eval form)
                (let* ((expr (function-lambda-expression (macro-function name)))
                       (classfile-name (next-classfile-name))
                       (classfile
                        (ignore-errors
                         (jvm:compile-defun nil expr nil classfile-name))))
                  (if (verify-load classfile)
                      (progn
                        (when *compile-print*
                          (format t ";  Macro ~A => ~A.cls~%" name
                                  (pathname-name (pathname classfile-name))))
                        (setf form
                              (if (special-operator-p name)
                                  `(%put ',name 'macroexpand-macro
                                         (make-macro ',name
                                                     (load-compiled-function
                                                      ,(file-namestring classfile))))
                                  `(fset ',name
                                         (make-macro ',name
                                                     (load-compiled-function
                                                      ,(file-namestring classfile)))
                                         ,*source-position*
                                         ',(third form)))))
                      ;; FIXME error or warning
                      (format *error-output* "; Unable to compile macro ~A~%" name)))))
             (DEFTYPE
              (eval form))
             (EVAL-WHEN
              (multiple-value-bind (ct lt e)
                  (parse-eval-when-situations (cadr form))
                (let ((new-compile-time-too (or ct
                                                (and compile-time-too e)))
                      (body (cddr form)))
                  (cond (lt
                         (process-toplevel-progn body stream new-compile-time-too))
                        (new-compile-time-too
                         (eval `(progn ,@body)))))
                (return-from process-toplevel-form)))
             (LOCALLY
              ;; FIXME Need to handle special declarations too!
              (let ((jvm:*speed*  jvm:*speed*)
                    (jvm:*safety* jvm:*safety*)
                    (jvm:*debug*  jvm:*debug*))
                (multiple-value-bind (forms decls)
                    (parse-body (cdr form) nil)
                  (jvm::process-optimization-declarations decls)
                  (process-toplevel-progn forms stream compile-time-too)
                  (return-from process-toplevel-form))))
             (PROGN
              (process-toplevel-progn (cdr form) stream compile-time-too)
              (return-from process-toplevel-form))
             (DECLARE
              (jvm::compiler-style-warn "Misplaced declaration: ~S" form))
             (t
              (when (and (symbolp (car form))
                         (macro-function (car form) *compile-file-environment*))
                ;; Note that we want MACROEXPAND-1 and not MACROEXPAND here, in
                ;; case the form being expanded expands into something that needs
                ;; special handling by PROCESS-TOPLEVEL-FORM (e.g. DEFMACRO).
                (process-toplevel-form (macroexpand-1 form *compile-file-environment*)
                                       stream compile-time-too)
                (return-from process-toplevel-form))

              (when compile-time-too
                (eval form))

              (cond ((eq first 'QUOTE)
                     (setf form (precompile-form form nil)))
                    ((eq first '%PUT)
                     (setf form (precompile-form form nil)))
                    ((and (memq first '(EXPORT REQUIRE PROVIDE SHADOW))
                          (or (keywordp (second form))
                              (and (listp (second form))
                                   (eq (first (second form)) 'QUOTE))))
                     (setf form (precompile-form form nil)))
                    ((and (eq first '%SET-FDEFINITION)
                          (eq (car (second form)) 'QUOTE)
                          (consp (third form))
                          (eq (%car (third form)) 'FUNCTION)
                          (symbolp (cadr (third form))))
                     (setf form (precompile-form form nil)))
                    ((memq first '(LET LET*))
                     (let ((body (cddr form)))
                       (if (dolist (subform body nil)
                             (when (and (consp subform) (eq (%car subform) 'DEFUN))
                               (return t)))
                           (setf form (convert-toplevel-form form))
                           (setf form (precompile-form form nil)))))
                    ((eq first 'mop::ensure-method)
                     (setf form (convert-ensure-method form)))
                    (t
                     (setf form (precompile-form form nil)))))))))
  (dump-form form stream))

(defun convert-ensure-method (form)
  (c-e-m-1 form :function)
  (c-e-m-1 form :fast-function)
  (precompile-form form nil))

(defun c-e-m-1 (form key)
  (let* ((tail (cddr form))
         (function-form (getf tail key)))
    (when (and function-form (consp function-form)
               (eq (%car function-form) 'FUNCTION))
      (let ((lambda-expression (cadr function-form)))
        (let* ((jvm::*speed* jvm::*speed*)
               (jvm::*safety* jvm::*safety*)
               (jvm::*debug* jvm::*debug*))
          (let* ((classfile-name (next-classfile-name))
                 (classfile (report-error
                             (jvm:compile-defun nil lambda-expression nil classfile-name)))
                 (compiled-function (verify-load classfile)))
            (cond (compiled-function
                   (when *compile-print*
                     (format t ";  method => ~A.cls~%"
                             (pathname-name (pathname classfile-name))))
                   (setf (getf tail key)
                         `(load-compiled-function ,(file-namestring classfile))))
                  (t
                   ;; FIXME This should be a warning or error of some sort...
                   (format *error-output* "; Unable to compile method~%")))))))))

(defun convert-toplevel-form (form)
  (when *compile-print*
    (let ((*print-length* 2)
          (*print-level* 2))
      (format t "; Converting ~S~%" form)))
  (let* ((expr `(lambda () ,form))
         (classfile-name (next-classfile-name))
         (classfile (report-error
                     (jvm:compile-defun nil expr nil classfile-name)))
         (compiled-function (verify-load classfile)))
    (setf form
          (if compiled-function
              `(funcall (load-compiled-function ,(file-namestring classfile)))
              (precompile-form form nil)))))


(defun process-toplevel-macrolet (form stream compile-time-too)
  (let ((*compile-file-environment* (make-environment *compile-file-environment*)))
    (dolist (definition (cadr form))
      (environment-add-macro-definition *compile-file-environment*
                                        (car definition)
                                        (make-macro (car definition)
                                                    (make-expander-for-macrolet definition))))
    (dolist (body-form (cddr form))
      (process-toplevel-form body-form stream compile-time-too))))

(defun process-toplevel-progn (forms stream compile-time-too)
  (dolist (form forms)
    (process-toplevel-form form stream compile-time-too)))

;;; Adapted from SBCL.
;;; Parse an EVAL-WHEN situations list, returning three flags,
;;; (VALUES COMPILE-TOPLEVEL LOAD-TOPLEVEL EXECUTE), indicating
;;; the types of situations present in the list.
(defun parse-eval-when-situations (situations)
  (when (or (not (listp situations))
	    (set-difference situations
			    '(:compile-toplevel
			      compile
			      :load-toplevel
			      load
			      :execute
			      eval)))
    (error "Bad EVAL-WHEN situation list: ~S." situations))
  (values (intersection '(:compile-toplevel compile) situations)
	  (intersection '(:load-toplevel load) situations)
	  (intersection '(:execute eval) situations)))

(defun compile-file (input-file
                     &key
                     output-file
                     ((:verbose *compile-verbose*) *compile-verbose*)
                     ((:print *compile-print*) *compile-print*)
                     external-format)
  (unless (or (and (probe-file input-file) (not (file-directory-p input-file)))
              (pathname-type input-file))
    (let ((pathname (merge-pathnames (make-pathname :type "lisp") input-file)))
      (when (probe-file pathname)
        (setf input-file pathname))))
  (setf output-file (if output-file
                        (merge-pathnames output-file *default-pathname-defaults*)
                        (compile-file-pathname input-file)))
  (let* ((*output-file-pathname* output-file)
         (type (pathname-type output-file))
         (temp-file (merge-pathnames (make-pathname :type (concatenate 'string type "-tmp"))
                                     output-file))
         (warnings-p t)
         (failure-p t))
    (with-open-file (in input-file :direction :input)
      (let* ((*compile-file-pathname* (pathname in))
             (*compile-file-truename* (truename in))
             (*class-number* 0)
             (namestring (namestring *compile-file-truename*))
             (start (get-internal-real-time))
             elapsed)
        (when *compile-verbose*
          (format t "; Compiling ~A ...~%" namestring))
        (with-compilation-unit ()
          (with-open-file (out temp-file :direction :output :if-exists :supersede)
            (let ((*readtable* *readtable*)
                  (*package* *package*)
                  (jvm:*speed* jvm:*speed*)
                  (jvm:*safety* jvm:*safety*)
                  (jvm:*debug* jvm:*debug*)
                  (jvm::*functions-defined-in-current-file* '())
                  (*fbound-names* '()))
              (write "; -*- Mode: Lisp -*-" :escape nil :stream out)
              (terpri out)
              (let ((*package* (find-package '#:cl)))
                (write (list 'init-fasl :version *fasl-version*) :stream out)
                (terpri out)
                (write (list 'setq '*fasl-source* *compile-file-truename*) :stream out)
                (terpri out))
              (loop
                (let* ((*source-position* (file-position in))
                       (jvm::*source-line-number* (stream-line-number in))
                       (form (read in nil in)))
                  (when (eq form in)
                    (return))
                  (process-toplevel-form form out nil)))
              (dolist (name *fbound-names*)
                (fmakunbound name))))
          (cond ((zerop (+ jvm::*errors* jvm::*warnings* jvm::*style-warnings*))
                 (setf warnings-p nil failure-p nil))
                ((zerop (+ jvm::*errors* jvm::*warnings*))
                 (setf failure-p nil))))
        (setf elapsed (/ (- (get-internal-real-time) start) 1000.0))
        (rename-file temp-file output-file)
        (when *compile-verbose*
          (format t "~&; Compiled ~A (~A seconds)~%" namestring elapsed))))
    (values (truename output-file) warnings-p failure-p)))

(defmacro defun (name lambda-list &body body &environment env)
    (multiple-value-bind (body decls doc)
        (sys::parse-body body)
      (let* ((block-name (fdefinition-block-name name))
             (lambda-expression `(lambda ,lambda-list ,@decls (block ,block-name ,@body))))
        (cond (*compile-file-truename*
               `(sys::fset ',name ,lambda-expression))
              (t
               (when (and env (sys::empty-environment-p env))
                 (setf env nil))
               (when (null env)
                 (setf lambda-expression (precompile-form lambda-expression nil)))
               `(%defun ',name ,lambda-expression))))))
