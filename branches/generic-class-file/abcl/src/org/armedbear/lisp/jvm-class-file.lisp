;;; jvm-class-file.lisp
;;;
;;; Copyright (C) 2010 Erik Huelsmann
;;; $Id: compiler-pass2.lisp 12311 2009-12-28 23:11:35Z ehuelsmann $
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

(in-package "JVM")

#|

The general design of the class-file writer is to have generic
- human readable - representations of the class being generated
during the construction and manipulation phases.

After completing the creation/manipulation of the class, all its
components will be finalized. This process translates readable
(e.g. string) representations to indices to be stored on disc.

The only thing to be done after finalization is sending the
output to a stream ("writing").


Finalization happens highest-level first. As an example, take a
method with exception handlers. The exception handlers are stored
as attributes in the class file structure. They are children of the
method's Code attribute. In this example, the body of the Code
attribute (the higher level) gets finalized before the attributes.
The reason to do so is that the exceptions need to refer to labels
(offsets) in the Code segment.


|#


(defun map-primitive-type (type)
  (case type
    (:int      "I")
    (:long     "J")
    (:float    "F")
    (:double   "D")
    (:boolean  "Z")
    (:char     "C")
    (:byte     "B")
    (:short    "S")
    (:void     "V")))


#|

The `class-name' facility helps to abstract from "this instruction takes
a reference" and "this instruction takes a class name". We simply pass
the class name around and the instructions themselves know which
representation to use.

|#

(defstruct (class-name (:conc-name class-)
                       (:constructor %make-class-name))
  name-internal
  ref
  array-ref)

(defun make-class-name (name)
  (setf name (substitute #\/ #\. name))
  (%make-class-name :name-internal name
                    :ref (concatenate 'string "L" name ";")
                    :array-ref (concatenate 'string "[L" name ";")))

(defmacro define-class-name (symbol java-dotted-name &optional documentation)
  `(defconstant ,symbol (make-class-name ,java-dotted-name)
     ,documentation))

(define-class-name +!java-object+ "java.lang.Object")
(define-class-name +!java-string+ "java.lang.String")
(define-class-name +!lisp-object+ "org.armedbear.lisp.LispObject")
(define-class-name +!lisp-simple-string+ "org.armedbear.lisp.SimpleString")
(define-class-name +!lisp+ "org.armedbear.lisp.Lisp")
(define-class-name +!lisp-nil+ "org.armedbear.lisp.Nil")
(define-class-name +!lisp-class+ "org.armedbear.lisp.LispClass")
(define-class-name +!lisp-symbol+ "org.armedbear.lisp.Symbol")
(define-class-name +!lisp-thread+ "org.armedbear.lisp.LispThread")
(define-class-name +!lisp-closure-binding+ "org.armedbear.lisp.ClosureBinding")
(define-class-name +!lisp-integer+ "org.armedbear.lisp.Integer")
(define-class-name +!lisp-fixnum+ "org.armedbear.lisp.Fixnum")
(define-class-name +!lisp-bignum+ "org.armedbear.lisp.Bignum")
(define-class-name +!lisp-single-float+ "org.armedbear.lisp.SingleFloat")
(define-class-name +!lisp-double-float+ "org.armedbear.lisp.DoubleFloat")
(define-class-name +!lisp-cons+ "org.armedbear.lisp.Cons")
(define-class-name +!lisp-load+ "org.armedbear.lisp.Load")
(define-class-name +!lisp-character+ "org.armedbear.lisp.Character")
(define-class-name +!lisp-simple-vector+ "org.armedbear.lisp.SimpleVector")
(define-class-name +!lisp-abstract-string+ "org.armedbear.lisp.AbstractString")
(define-class-name +!lisp-abstract-vector+ "org.armedbear.lisp.AbstractVector")
(define-class-name +!lisp-abstract-bit-vector+
    "org.armedbear.lisp.AbstractBitVector")
(define-class-name +!lisp-environment+ "org.armedbear.lisp.Environment")
(define-class-name +!lisp-special-binding+ "org.armedbear.lisp.SpecialBinding")
(define-class-name +!lisp-special-binding-mark+
    "org.armedbear.lisp.SpecialBindingMark")
(define-class-name +!lisp-throw+ "org.armedbear.lisp.Throw")
(define-class-name +!lisp-return+ "org.armedbear.lisp.Return")
(define-class-name +!lisp-go+ "org.armedbear.lisp.Go")
(define-class-name +!lisp-primitive+ "org.armedbear.lisp.Primitive")
(define-class-name +!lisp-compiled-closure+
    "org.armedbear.lisp.CompiledClosure")
(define-class-name +!lisp-eql-hash-table+ "org.armedbear.lisp.EqlHashTable")
(define-class-name +!lisp-package+ "org.armedbear.lisp.Package")
(define-class-name +!lisp-readtable+ "org.armedbear.lisp.Readtable")
(define-class-name +!lisp-stream+ "org.armedbear.lisp.Stream")
(define-class-name +!lisp-closure+ "org.armedbear.lisp.Closure")
(define-class-name +!lisp-closure-parameter+
    "org.armedbear.lisp.Closure$Parameter")
(define-class-name +!fasl-loader+ "org.armedbear.lisp.FaslClassLoader")


(defun descriptor (method-name return-type &rest argument-types)
  (format nil "~A(~{~A~}~A)" method-name
          (mapcar #'(lambda (arg-type)
                      (if (keywordp arg-type)
                          (map-primitive-type arg-type)
                          (class-ref arg-type)))
                  argument-types)
          (if (keywordp return-type)
              (map-primitive-type return-type)
              (class-name-internal return-type))))





(defstruct pool
  (count 1)  ;; ####  why count 1???
  entries-list
  (entries (make-hash-table :test #'equal :size 2048 :rehash-size 2.0)))

(defstruct constant
  tag
  index)

(defparameter +constant-type-map+
  '((:class          7 1)
    (:field-ref      9 1)
    (:method-ref    10 1)
    ;; (:interface-method-ref 11)
    (:string         8 1)
    (:integer        3 1)
    (:float          4 1)
    (:long           5 2)
    (:double         6 2)
    (:name-and-type 12 1)
    (:utf8           1 1)))

(defstruct (constant-class (:include constant
                                     (tag 7)))
  name)

(defstruct (constant-member-ref (:include constant))
  class
  name/type)

(defstruct (constant-string (:constructor make-constant-string (value-index))
                            (:include constant
                                      (tag 8)))
  value-index) ;;; #### is this the value or the value index???

(defstruct (constant-float/int (:include constant))
  value)

(defstruct (constant-double/long (:include constant))
  value)

(defstruct (constant-name/type (:include constant))
  name-index
  descriptor-index)

(defstruct (constant-utf8 (:include constant))
  value)


;; Need to add pool/constant creation addition routines here;
;; all routines have 2 branches: return existing or push new.

(defun pool-add-string (pool string)
  (let ((entry (gethash (pool-entries string))))
    (unless entry
      (setf entry (make-constant-string (pool-count pool) string))
      (push entry (pool-entries-list pool))
      (incf (pool-count pool)))
    (constant-index entry)))



(defstruct (class-file (:constructor %make-class-file))
  constants
  access-flags
  class
  superclass
  ;; interfaces
  fields
  methods
  attributes
  )

(defun class-add-field (class field)
  (push field (class-file-fields class)))

(defun class-field (class name)
  (find name (class-file-fields class)
        :test #'string= :key #'field-name))

(defun class-add-method (class method)
  (push method (class-file-methods class)))

(defun class-methods-by-name (class name)
  (remove (map-method-name name) (class-file-methods class)
          :test-not #'string= :key #'method-name))

(defun class-method (class descriptor)
  (find descriptor (class-file-methods class)
        :test #'string= :key #'method-name))


(defun finalize-class-file (class)

  ;; constant pool contains constants finalized on addition;
  ;; no need for additional finalization

  (setf (class-file-access-flags class)
        (map-flags (class-file-access-flags class)))
  ;; (finalize-class-name )
  ;;  (finalize-interfaces)
  (dolist (field (class-file-fields class))
    (finalize-field field class))
  (dolist (method (class-file-methods class))
    (finalize-method method class))
  ;; top-level attributes (no parent attributes to refer to)
  (finalize-attributes (class-file-attributes class) nil class)

)

(defun !write-class-file (class stream)
  ;; all components need to finalize themselves:
  ;;  the constant pool needs to be complete before we start
  ;;  writing our output.

  ;; header
  (write-u4 #xCAFEBABE stream)
  (write-u2 3 stream)
  (write-u2 45 stream)

   ;; constants pool
  (write-constants (class-file-constants class) stream)
  ;; flags
  (write-u2  (class-file-access-flags class) stream)
  ;; class name
  (write-u2 (class-file-class class) stream)
  ;; superclass
  (write-u2 (class-file-superclass class) stream)

  ;; interfaces
  (write-u2 0 stream)

  ;; fields
  (write-u2 (length (class-file-fields class)) stream)
  (dolist (field (class-file-fields class))
    (!write-field field stream))

  ;; methods
  (write-u2 (length (class-file-methods class)) stream)
  (dolist (method (class-file-methods class))
    (!write-method method stream))

  ;; attributes
  (write-attributes (class-file-attributes class) stream))

(defun write-constants (constants stream)
  (write-u2 (pool-count constants) stream)
  (dolist (entry (reverse (pool-entries-list constants)))
    (let ((tag (constant-tag entry)))
    (write-u1 tag stream)
    (case tag
      (1 ; UTF8
       (write-utf8 (constant-utf8-value entry) stream))
      ((3 4) ; int
       (write-u4 (constant-float/int-value entry) stream))
      ((5 6) ; long double
       (write-u4 (second entry) stream)
       (write-u4 (third entry) stream))
      ((9 10 11 12) ; fieldref methodref InterfaceMethodref nameAndType
       (write-u2 (second entry) stream)
       (write-u2 (third entry) stream))
      ((7 8) ; class string
       (write-u2 (constant-class-name entry) stream))
      (t
       (error "write-constant-pool-entry unhandled tag ~D~%" tag))))))

#|

ABCL doesn't use interfaces, so don't implement it here at this time

(defstruct interface)

|#


(defparameter +access-flags-map+
  '((:public       #x0001)
    (:private      #x0002)
    (:protected    #x0004)
    (:static       #x0008)
    (:final        #x0010)
    (:volatile     #x0040)
    (:synchronized #x0020)
    (:transient    #x0080)
    (:native       #x0100)
    (:abstract     #x0400)
    (:strict       #x0800)))

(defun map-flags (flags)
  (reduce #'(lambda (x y)
              (logior (or (when (member (car x) flags)
                            (second x))
                          0) y)
              (logior (or )))
          :initial-value 0))

(defstruct (field (:constructor %make-field))
  access-flags
  name
  descriptor
  attributes
  )

(defun make-field (name type &key (flags '(:public)))
  (%make-field :access-flags flags
               :name name
               :descriptor (map-primitive-type type)))

(defun add-field-attribute (field attribute)
  (push attribute (field-attributes field)))


(defun finalize-field (field class)
  (declare (ignore class field))
  (error "Not implemented"))

(defun !write-field (field stream)
  (declare (ignore field stream))
  (error "Not implemented"))


(defstruct (method (:constructor %!make-method))
  access-flags
  name
  descriptor
  attributes
  arg-count ;; not in the class file,
            ;; but required for setting up CODE attribute
  )


(defun map-method-name (name)
  (cond
    ((eq name :class-constructor)
     "<clinit>")
    ((eq name :constructor)
     "<init>")
    (t name)))

(defun !make-method-descriptor (name return &rest args)
  (apply #'concatenate (append (list 'string (map-method-name name) "(")
                               (mapcar #'map-primitive-type args)
                               (list ")" return))))

(defun !make-method (name return args &key (flags '(:public)))
  (setf name (map-method-name name))
  (%make-method :descriptor (apply #'make-method-descriptor
                                   name return args)
                :access-flags flags
                :name name
                :arg-count (if (member :static flags)
                               (length args)
                               (1+ (length args))))) ;; implicit 'this'

(defun method-add-attribute (method attribute)
  (push attribute (method-attributes method)))

(defun method-attribute (method name)
  (find name (method-attributes method)
        :test #'string= :key #'attribute-name))


(defun finalize-method (method class)
  (declare (ignore method class))
  (error "Not implemented"))


(defun !write-method (method stream)
  (declare (ignore method stream))
  (error "Not implemented"))

(defstruct attribute
  name

  ;; not in the class file:
  finalizer  ;; function of 3 arguments: the attribute, parent and class-file
  writer     ;; function of 2 arguments: the attribute and the output stream
  )

(defun finalize-attributes (attributes att class)
  (dolist (attribute attributes)
    ;; assure header: make sure 'name' is in the pool
    (setf (attribute-name attribute)
          (pool-add-string (class-file-constants class)
                           (attribute-name attribute)))
    ;; we're saving "root" attributes: attributes which have no parent
    (funcall (attribute-finalizer attribute) attribute att class)))

(defun write-attributes (attributes stream)
  (write-u2 (length attributes) stream)
  (dolist (attribute attributes)
    (write-u2 (attribute-name attribute) stream)
    ;; set up a bulk catcher for (UNSIGNED-BYTE 8)
    ;; since we need to know the attribute length (excluding the header)
    (let ((local-stream (sys::%make-byte-array-output-stream)))
      (funcall (attribute-writer attribute) attribute local-stream)
      (let ((array (sys::%get-output-stream-array local-stream)))
        (write-u2 (length array) stream)
        (write-sequence array stream)))))



(defstruct (code-attribute (:conc-name code-)
                           (:include attribute
                                     (name "Code")
                                     (finalizer #'!finalize-code)
                                     (writer #'!write-code))
                           (:constructor %make-code-attribute))
  max-stack
  max-locals
  code
  attributes
  ;; labels contains offsets into the code array after it's finalized
  (labels (make-hash-table :test #'eq))

  ;; fields not in the class file start here
  current-local ;; used for handling nested WITH-CODE-TO-METHOD blocks
  )


(defun code-label-offset (code label)
  (gethash label (code-labels code)))

(defun (setf code-label-offset) (offset code label)
  (setf (gethash label (code-labels code)) offset))

(defun !finalize-code (code class)
  (let ((c (coerce (resolve-instructions (code-code code)) 'vector)))
    (setf (code-max-stack code) (analyze-stack c)
          (code-code code) (code-bytes c)))
  (finalize-attributes (code-attributes code) code class))

(defun !write-code (code stream)
  (write-u2 (code-max-stack code) stream)
  (write-u2 (code-max-locals code) stream)
  (let ((code-array (code-code code)))
    (write-u4 (length code-array) stream)
    (dotimes (i (length code-array))
      (write-u1 (svref code-array i) stream)))
  (write-attributes (code-attributes code) stream))

(defun make-code-attribute (method)
  (%make-code-attribute :max-locals (method-arg-count method)))

(defun code-add-attribute (code attribute)
  (push attribute (code-attributes code)))

(defun code-attribute (code name)
  (find name (code-attributes code)
        :test #'string= :key #'attribute-name))



(defvar *current-code-attribute*)

(defun save-code-specials (code)
  (setf (code-code code) *code*
        (code-max-locals code) *registers-allocated*
        (code-exception-handlers code) *handlers*
        (code-current-local code) *register*))

(defun restore-code-specials (code)
  (setf *code* (code-code code)
        *registers-allocated* (code-max-locals code)
        *register* (code-current-local code)))

(defmacro with-code-to-method ((method &key safe-nesting) &body body)
  (let ((m (gensym))
        (c (gensym)))
    `(progn
       ,@(when safe-nesting
           `((when *current-code-attribute*
               (save-code-specials *current-code-attribute*))))
       (let* ((,m ,method)
              (,c (method-attribute ,m "Code"))
              (*code* (code-code ,c))
              (*registers-allocated* (code-max-locals ,c))
              (*register* (code-current-local ,c))
              (*current-code-attribute* ,c))
         ,@body
         (setf (code-code ,c) *code*
               (code-exception-handlers ,c) *handlers*
               (code-max-locals ,c) *registers-allocated*))
       ,@(when safe-nesting
           `((when *current-code-attribute*
               (restore-code-specials *current-code-attribute*)))))))

(defstruct (exceptions-attribute (:constructor make-exceptions)
                                 (:conc-name exceptions-)
                                 (:include attribute
                                           (name "Exceptions")
                                           (finalizer #'finalize-exceptions)
                                           (writer #'write-exceptions)))
  exceptions)

(defun finalize-exceptions (exceptions code class)
  (dolist (exception (exceptions-exceptions exceptions))
    ;; no need to finalize `catch-type': it's already the index required
    (setf (exception-start-pc exception)
          (code-label-offset code (exception-start-pc exception))
          (exception-end-pc exception)
          (code-label-offset code (exception-end-pc exception))
          (exception-handler-pc exception)
          (code-label-offset code (exception-handler-pc exception))
          (exception-catch-type exception)
          (pool-add-string (class-file-constants class)
                           (exception-catch-type exception))))
  ;;(finalize-attributes (exceptions-attributes exception) exceptions class)
  )


(defun write-exceptions (exceptions stream)
  ; number of entries
  (write-u2 (length (exceptions-exceptions exceptions)) stream)
  (dolist (exception (exceptions-exceptions exceptions))
    (write-u2 (exception-start-pc exception) stream)
    (write-u2 (exception-end-pc exception) stream)
    (write-u2 (exception-handler-pc exception) stream)
    (write-u2 (exception-catch-type exception) stream)))

(defun code-add-exception (code start end handler type)
  (when (null (code-attribute code "Exceptions"))
    (code-add-attribute code (make-exceptions)))
  (push (make-exception :start-pc start
                        :end-pc end
                        :handler-pc handler
                        :catch-type type)
        (exceptions-exceptions (code-attribute code "Exceptions"))))

(defstruct exception
  start-pc    ;; label target
  end-pc      ;; label target
  handler-pc  ;; label target
  catch-type  ;; a string for a specific type, or NIL for all
  )

(defstruct (source-file-attribute (:conc-name source-)
                                  (:include attribute
                                            (name "SourceFile")))
  filename)

(defstruct (line-numbers-attribute (:include attribute
                                             (name "LineNumberTable")))
  line-numbers)

(defstruct line-number
  start-pc
  line)

(defstruct (local-variables-attribute (:conc-name local-var-)
                                      (:include attribute
                                                (name "LocalVariableTable")))
  locals)

(defstruct (local-variable (:conc-name local-))
  start-pc
  length
  name
  descriptor
  index)

#|

;; this is the minimal sequence we need to support:

;;  create a class file structure
;;  add methods
;;  add code to the methods, switching from one method to the other
;;  finalize the methods, one by one
;;  write the class file

to support the sequence above, we probably need to
be able to

- find methods by signature
- find the method's code attribute
- add code to the code attribute
- finalize the code attribute contents (blocking it for further addition)
- 


|#

