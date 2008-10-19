;;; abcl-script.lisp
;;;
;;; Copyright (C) 2008 Alessio Stalla
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

(in-package :abcl-script)

(defvar *java-interface-implementations* (make-hash-table :test #'equal))

(defconstant +global-scope+
  (jfield "javax.script.ScriptContext" "GLOBAL_SCOPE"))

(defconstant +engine-scope+
  (jfield "javax.script.ScriptContext" "ENGINE_SCOPE"))

(defconstant +put-binding+ (jmethod "javax.script.Bindings"
				    "put"
				    "java.lang.String"
				    "java.lang.Object"))

(defconstant +get-bindings+ (jmethod "javax.script.ScriptContext"
				     "getBindings"
				     "int"))

(defun generate-bindings (bindings)
  (let ((*package* (find-package :abcl-script-user)))
    (mapcar (lambda (binding) (list (read-from-string (car binding))
				    (cdr binding)))
	    bindings)))

(defun generate-java-bindings (bindings-list actual-bindings java-bindings)
  (loop :for binding  :in actual-bindings
	:for jbinding :in bindings-list
	:collect `(jcall +put-binding+
		   ,java-bindings ,(car jbinding) ,(car binding))))

(defun eval-script (global-bindings engine-bindings stdin stdout
		    code-string script-context)
  (let ((*package* (find-package :abcl-script-user))
	(*standard-input* stdin)
	(*standard-output* stdout)
	(actual-global-bindings (generate-bindings global-bindings))
	(actual-engine-bindings (generate-bindings engine-bindings)))
    (eval `(let ((*standard-input* ,stdin)
		 (*standard-output* ,stdout)
		 (*package* (find-package :abcl-script-user)))
	    (let (,@actual-global-bindings)
	      (let (,@actual-engine-bindings)
		(prog1
		    (progn
		      ,@(read-from-string
			 (concatenate 'string "(" code-string ")")))
		  (finish-output *standard-output*)
		  ,@(generate-java-bindings
		     global-bindings 
		     actual-global-bindings
		     (jcall +get-bindings+ script-context +global-scope+))
		  ,@(generate-java-bindings
		     engine-bindings 
		     actual-engine-bindings
		     (jcall +get-bindings+ script-context +engine-scope+)))))))))

(defstruct (java-interface-implementation (:type list))
  (method-definitions (list) :type list))

(defun define-java-interface-implementation (interface &rest method-definitions)
  (register-java-interface-implementation
   (canonicalize-interface interface)
   (make-java-interface-implementation :method-definitions method-definitions)))

(defun canonicalize-interface (interface)
  (cond
    ((stringp interface) interface)
    ((jclass-interface-p interface) (jclass-name interface))
    (t (error "not an interface: ~A" interface))))

(defun register-java-interface-implementation (interface implementation)
  (setf (gethash (canonicalize-interface interface)
		 *java-interface-implementations*)
	(implement-java-interface interface implementation)))

(defun find-java-interface-implementation (interface)
  (gethash (canonicalize-interface interface)
	   *java-interface-implementations*))

(defun implement-java-interface (interface implementation)
  (apply #'jinterface-implementation
	 `(,interface
	   ,@(java-interface-implementation-method-definitions implementation))))