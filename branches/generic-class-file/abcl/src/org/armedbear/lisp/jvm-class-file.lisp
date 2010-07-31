;;; jvm-class-file.lisp
;;;
;;; Copyright (C) 2010 Erik Huelsmann
;;; $Id$
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
  "Maps a symbolic primitive type name to its Java string representation."
  (case type
    (:int        "I")
    (:long       "J")
    (:float      "F")
    (:double     "D")
    (:boolean    "Z")
    (:char       "C")
    (:byte       "B")
    (:short      "S")
    ((nil :void) "V")))


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
  "Creates a `class-name' structure for the class or interface `name'.

`name' should be specified using Java representation, which is converted
to 'internal' (JVM) representation by this function."
  (setf name (substitute #\/ #\. name))
  (%make-class-name :name-internal name
                    :ref (concatenate 'string "L" name ";")
                    :array-ref (concatenate 'string "[L" name ";")))

(defmacro define-class-name (symbol java-dotted-name &optional documentation)
  "Convenience macro to define constants for `class-name' structures,
initialized from the `java-dotted-name'."
  `(defconstant ,symbol (make-class-name ,java-dotted-name)
     ,documentation))

(define-class-name +java-object+ "java.lang.Object")
(define-class-name +java-string+ "java.lang.String")
(define-class-name +lisp-object+ "org.armedbear.lisp.LispObject")
(define-class-name +lisp-simple-string+ "org.armedbear.lisp.SimpleString")
(define-class-name +lisp+ "org.armedbear.lisp.Lisp")
(define-class-name +lisp-nil+ "org.armedbear.lisp.Nil")
(define-class-name +lisp-class+ "org.armedbear.lisp.LispClass")
(define-class-name +lisp-symbol+ "org.armedbear.lisp.Symbol")
(define-class-name +lisp-thread+ "org.armedbear.lisp.LispThread")
(define-class-name +lisp-closure-binding+ "org.armedbear.lisp.ClosureBinding")
(define-class-name +lisp-integer+ "org.armedbear.lisp.LispInteger")
(define-class-name +!lisp-fixnum+ "org.armedbear.lisp.Fixnum")
(define-class-name +!lisp-bignum+ "org.armedbear.lisp.Bignum")
(define-class-name +!lisp-single-float+ "org.armedbear.lisp.SingleFloat")
(define-class-name +!lisp-double-float+ "org.armedbear.lisp.DoubleFloat")
(define-class-name +lisp-cons+ "org.armedbear.lisp.Cons")
(define-class-name +lisp-load+ "org.armedbear.lisp.Load")
(define-class-name +lisp-character+ "org.armedbear.lisp.LispCharacter")
(define-class-name +lisp-structure-object+ "org.armedbear.lisp.StructureObject")
(define-class-name +lisp-simple-vector+ "org.armedbear.lisp.SimpleVector")
(define-class-name +lisp-abstract-string+ "org.armedbear.lisp.AbstractString")
(define-class-name +lisp-abstract-vector+ "org.armedbear.lisp.AbstractVector")
(define-class-name +lisp-abstract-bit-vector+
    "org.armedbear.lisp.AbstractBitVector")
(define-class-name +lisp-environment+ "org.armedbear.lisp.Environment")
(define-class-name +lisp-special-binding+ "org.armedbear.lisp.SpecialBinding")
(define-class-name +lisp-special-bindings-mark+
    "org.armedbear.lisp.SpecialBindingsMark")
(define-class-name +lisp-throw+ "org.armedbear.lisp.Throw")
(define-class-name +lisp-return+ "org.armedbear.lisp.Return")
(define-class-name +lisp-go+ "org.armedbear.lisp.Go")
(define-class-name +lisp-primitive+ "org.armedbear.lisp.Primitive")
(define-class-name +lisp-eql-hash-table+ "org.armedbear.lisp.EqlHashTable")
(define-class-name +lisp-hash-table+ "org.armedbear.lisp.HashTable")
(define-class-name +lisp-package+ "org.armedbear.lisp.Package")
(define-class-name +lisp-readtable+ "org.armedbear.lisp.Readtable")
(define-class-name +lisp-stream+ "org.armedbear.lisp.Stream")
(define-class-name +lisp-closure+ "org.armedbear.lisp.Closure")
(define-class-name +lisp-compiled-closure+ "org.armedbear.lisp.CompiledClosure")
(define-class-name +lisp-closure-parameter+
    "org.armedbear.lisp.Closure$Parameter")
(define-class-name +!fasl-loader+ "org.armedbear.lisp.FaslClassLoader")

#|

Lisp-side descriptor representation:

 - list: a list starting with a method return value, followed by
     the argument types
 - keyword: the primitive type associated with that keyword
 - class-name structure instance: the class-ref value

The latter two can be converted to a Java representation using
the `internal-field-ref' function, the former is to be fed to
`descriptor'.

|#

(defun internal-field-type (field-type)
  "Returns a string containing the JVM-internal representation
of `field-type', which should either be a symbol identifying a primitive
type, or a `class-name' structure identifying a class or interface."
  (if (symbolp field-type)
      (map-primitive-type field-type)
      (class-name-internal field-type)))

(defun internal-field-ref (field-type)
  "Returns a string containing the JVM-internal representation of a reference
to `field-type', which should either be a symbol identifying a primitive
type, or a `class-name' structure identifying a class or interface."
  (if (symbolp field-type)
      (map-primitive-type field-type)
      (class-ref field-type)))

(defun descriptor (return-type &rest argument-types)
  "Returns a string describing the `return-type' and `argument-types'
in JVM-internal representation."
  (format nil "(~{~A~})~A" (mapcar #'internal-field-ref argument-types)
          (internal-field-ref return-type)))


(defstruct pool
  ;; `index' contains the index of the last allocated slot (0 == empty)
  ;; "A constant pool entry is considered valid if it has
  ;; an index greater than 0 (zero) and less than pool-count"
  (index 0)
  entries-list
  ;; the entries hash stores raw values, except in case of string and
  ;; utf8, because both are string values
  (entries (make-hash-table :test #'equal :size 2048 :rehash-size 2.0)))


(defstruct constant
  "Structure to be included in all constant sub-types."
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

(defstruct (constant-class (:constructor make-constant-class (index name-index))
                           (:include constant
                                     (tag 7)))
  name-index)

(defstruct (constant-member-ref (:constructor
                                 %make-constant-member-ref
                                     (tag index class-index name/type-index))
                                (:include constant))
  class-index
  name/type-index)

(declaim (inline make-constant-field-ref make-constant-method-ref
                 make-constant-interface-method-ref))
(defun make-constant-field-ref (index class-index name/type-index)
  "Creates a `constant-member-ref' instance containing a field reference."
  (%make-constant-member-ref 9 index class-index name/type-index))

(defun make-constant-method-ref (index class-index name/type-index)
  "Creates a `constant-member-ref' instance containing a method reference."
  (%make-constant-member-ref 10 index class-index name/type-index))

(defun make-constant-interface-method-ref (index class-index name/type-index)
  "Creates a `constant-member-ref' instance containing an
interface-method reference."
  (%make-constant-member-ref 11 index class-index name/type-index))

(defstruct (constant-string (:constructor
                             make-constant-string (index value-index))
                            (:include constant
                                      (tag 8)))
  value-index)

(defstruct (constant-float/int (:constructor
                                %make-constant-float/int (tag index value))
                               (:include constant))
  value)

(declaim (inline make-constant-float make-constant-int))
(defun make-constant-float (index value)
  "Creates a `constant-float/int' structure instance containing a float."
  (%make-constant-float/int 4 index value))

(defun make-constant-int (index value)
  "Creates a `constant-float/int' structure instance containing an int."
  (%make-constant-float/int 3 index value))

(defstruct (constant-double/long (:constructor
                                  %make-constant-double/long (tag index value))
                                 (:include constant))
  value)

(declaim (inline make-constant-double make-constant-float))
(defun make-constant-double (index value)
  "Creates a `constant-double/long' structure instance containing a double."
  (%make-constant-double/long 6 index value))

(defun make-constant-long (index value)
  "Creates a `constant-double/long' structure instance containing a long."
  (%make-constant-double/long 5 index value))

(defstruct (constant-name/type (:constructor
                                make-constant-name/type (index
                                                         name-index
                                                         descriptor-index))
                               (:include constant
                                         (tag 12)))
  name-index
  descriptor-index)

(defstruct (constant-utf8 (:constructor make-constant-utf8 (index value))
                          (:include constant
                                    (tag 1)))
  value)


(defun pool-add-class (pool class)
  "Returns the index of the constant-pool class item for `class'.

`class' must be an instance of `class-name'."
  (let ((entry (gethash class (pool-entries pool))))
    (unless entry
      (let ((utf8 (pool-add-utf8 pool (class-name-internal class))))
        (setf entry
              (make-constant-class (incf (pool-index pool)) utf8)
              (gethash class (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-field-ref (pool class name type)
  "Returns the index of the constant-pool item which denotes a reference
to the `name' field of the `class', being of `type'.

`class' should be an instance of `class-name'.
`name' is a string.
`type' is a field-type (see `internal-field-type')"
  (let ((entry (gethash (acons name type class) (pool-entries pool))))
    (unless entry
      (let ((c (pool-add-class pool class))
            (n/t (pool-add-name/type pool name type)))
        (setf entry (make-constant-field-ref (incf (pool-index pool)) c n/t)
            (gethash (acons name type class) (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-method-ref (pool class name type)
  "Returns the index of the constant-pool item which denotes a reference
to the method with `name' in `class', which is of `type'.

Here, `type' is a method descriptor, which defines the argument types
and return type. `class' is an instance of `class-name'."
  (let ((entry (gethash (acons name type class) (pool-entries pool))))
    (unless entry
      (let ((c (pool-add-class pool class))
            (n/t (pool-add-name/type pool name type)))
        (setf entry (make-constant-method-ref (incf (pool-index pool)) c n/t)
              (gethash (acons name type class) (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-interface-method-ref (pool class name type)
  "Returns the index of the constant-pool item which denotes a reference to
the method `name' in the interface `class', which is of `type'.

See `pool-add-method-ref' for remarks."
  (let ((entry (gethash (acons name type class) (pool-entries pool))))
    (unless entry
      (let ((c (pool-add-class pool class))
            (n/t (pool-add-name/type pool name type)))
        (setf entry
            (make-constant-interface-method-ref (incf (pool-index pool)) c n/t)
            (gethash (acons name type class) (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-string (pool string)
  "Returns the index of the constant-pool item denoting the string."
  (let ((entry (gethash (cons 8 string) ;; 8 == string-tag
                        (pool-entries pool))))
    (unless entry
      (let ((utf8 (pool-add-utf8 pool string)))
        (setf entry (make-constant-string (incf (pool-index pool)) utf8)
              (gethash (cons 8 string) (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-int (pool int)
  "Returns the index of the constant-pool item denoting the int."
  (let ((entry (gethash (cons 3 int) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-int (incf (pool-index pool)) int)
            (gethash (cons 3 int) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-float (pool float)
  "Returns the index of the constant-pool item denoting the float."
  (let ((entry (gethash (cons 4 float) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-float (incf (pool-index pool)) float)
            (gethash (cons 4 float) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-long (pool long)
  "Returns the index of the constant-pool item denoting the long."
  (let ((entry (gethash (cons 5 long) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-long (incf (pool-index pool)) long)
            (gethash (cons 5 long) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool))
      (incf (pool-index pool))) ;; double index increase; long takes 2 slots
    (constant-index entry)))

(defun pool-add-double (pool double)
  "Returns the index of the constant-pool item denoting the double."
  (let ((entry (gethash (cons 6 double) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-double (incf (pool-index pool)) double)
            (gethash (cons 6 double) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool))
      (incf (pool-index pool))) ;; double index increase; 'double' takes 2 slots
    (constant-index entry)))

(defun pool-add-name/type (pool name type)
  "Returns the index of the constant-pool item denoting
the name/type identifier."
  (let ((entry (gethash (cons name type) (pool-entries pool)))
        (internal-type (if (listp type)
                           (apply #'descriptor type)
                           (internal-field-ref type))))
    (unless entry
      (let ((n (pool-add-utf8 pool name))
            (i-t (pool-add-utf8 pool internal-type)))
        (setf entry (make-constant-name/type (incf (pool-index pool)) n i-t)
              (gethash (cons name type) (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defun pool-add-utf8 (pool utf8-as-string)
  "Returns the index of the textual value that will be stored in the
class file as UTF-8 encoded data."
  (let ((entry (gethash (cons 11 utf8-as-string) ;; 11 == utf8
                        (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-utf8 (incf (pool-index pool)) utf8-as-string)
            (gethash (cons 11 utf8-as-string) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool)))
    (constant-index entry)))

(defstruct (class-file (:constructor
                        !make-class-file (class superclass access-flags)))
  (constants (make-pool))
  access-flags
  class
  superclass
  ;; interfaces
  fields
  methods
  attributes)

(defun class-add-field (class field)
  "Adds a `field' created by `make-field'."
  (push field (class-file-fields class)))

(defun class-field (class name)
  "Finds a field by name." ;; ### strictly speaking, a field is uniquely
  ;; identified by its name and type, not by the name alone.
  (find name (class-file-fields class)
        :test #'string= :key #'field-name))

(defun class-add-method (class method)
  "Adds a `method' to `class'; the method must have been created using
`make-method'."
  (push method (class-file-methods class)))

(defun class-methods-by-name (class name)
  "Returns all methods which have `name'."
  (remove name (class-file-methods class)
          :test-not #'string= :key #'method-name))

(defun class-method (class name return &rest args)
  "Return the method which is (uniquely) identified by its name AND descriptor."
  (let ((return-and-args (cons return args)))
    (find-if #'(lambda (c)
                 (and (string= (method-name c) name)
                      (equal (method-descriptor c) return-and-args)))
             (class-file-methods class))))

(defun class-add-attribute (class attribute)
  "Adds `attribute' to the class; attributes must be instances of
structure classes which include the `attribute' structure class."
  (push attribute (class-file-attributes class)))

(defun class-attribute (class name)
  "Returns the attribute which is named `name'."
  (find name (class-file-attributes class)
        :test #'string= :key #'attribute-name))


(defun finalize-class-file (class)
  "Transforms the representation of the class-file from one
which allows easy modification to one which works best for serialization.

The class can't be modified after finalization."

  ;; constant pool contains constants finalized on addition;
  ;; no need for additional finalization

  (setf (class-file-access-flags class)
        (map-flags (class-file-access-flags class)))
  (setf (class-file-superclass class)
        (pool-add-class (class-file-constants class)
                        (class-file-superclass class))
        (class-file-class class)
        (pool-add-class (class-file-constants class)
                        (class-file-class class)))
  ;;  (finalize-interfaces)
  (dolist (field (class-file-fields class))
    (finalize-field field class))
  (dolist (method (class-file-methods class))
    (finalize-method method class))
  ;; top-level attributes (no parent attributes to refer to)
  (finalize-attributes (class-file-attributes class) nil class))

(defun !write-class-file (class stream)
  "Serializes `class' to `stream', after it has been finalized."

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


(defvar *jvm-class-debug-pool* nil
  "When bound to a non-NIL value, enables output to *standard-output*
to allow debugging output of the constant section of the class file.")

(defun write-constants (constants stream)
  "Writes the constant section given in `constants' to the class file `stream'."
  (let ((pool-index 0))
    (write-u2 (1+ (pool-index constants)) stream)
    (when *jvm-class-debug-pool*
      (sys::%format t "pool count ~A~%" (pool-index constants)))
    (dolist (entry (reverse (pool-entries-list constants)))
      (incf pool-index)
      (let ((tag (constant-tag entry)))
        (when *jvm-class-debug-pool*
          (print-constant entry t))
        (write-u1 tag stream)
        (case tag
          (1                            ; UTF8
           (write-utf8 (constant-utf8-value entry) stream))
          ((3 4)                        ; float int
           (write-u4 (constant-float/int-value entry) stream))
          ((5 6)                        ; long double
           (write-u4 (logand (ash (constant-double/long-value entry) -32)
                             #xFFFFffff) stream)
           (write-u4 (logand (constant-double/long-value entry) #xFFFFffff)
                     stream))
          ((9 10 11)           ; fieldref methodref InterfaceMethodref
           (write-u2 (constant-member-ref-class-index entry) stream)
           (write-u2 (constant-member-ref-name/type-index entry) stream))
          (12                           ; nameAndType
           (write-u2 (constant-name/type-name-index entry) stream)
           (write-u2 (constant-name/type-descriptor-index entry) stream))
          (7                            ; class
           (write-u2 (constant-class-name-index entry) stream))
          (8                            ; string
           (write-u2 (constant-string-value-index entry) stream))
          (t
           (error "write-constant-pool-entry unhandled tag ~D~%" tag)))))))


(defun print-constant (entry stream)
  "Debugging helper to print the content of a constant-pool entry."
  (let ((tag (constant-tag entry))
        (index (constant-index entry)))
    (sys::%format stream "pool element ~a, tag ~a, " index tag)
    (case tag
      (1     (sys::%format t "utf8: ~a~%" (constant-utf8-value entry)))
      ((3 4) (sys::%format t "f/i: ~a~%" (constant-float/int-value entry)))
      ((5 6) (sys::%format t "d/l: ~a~%" (constant-double/long-value entry)))
      ((9 10 11) (sys::%format t "ref: ~a,~a~%"
                               (constant-member-ref-class-index entry)
                               (constant-member-ref-name/type-index entry)))
      (12 (sys::%format t "n/t: ~a,~a~%"
                        (constant-name/type-name-index entry)
                        (constant-name/type-descriptor-index entry)))
      (7 (sys::%format t "cls: ~a~%" (constant-class-name-index entry)))
      (8 (sys::%format t "str: ~a~%" (constant-string-value-index entry))))))


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
    (:strict       #x0800))
  "List of keyword symbols used for human readable representation of (access)
flags and their binary values.")

(defun map-flags (flags)
  "Calculates the bitmap of the flags from a list of symbols."
  (reduce #'(lambda (y x)
              (logior (or (when (member (car x) flags)
                            (second x))
                          0) y))
          +access-flags-map+
          :initial-value 0))

(defstruct (field (:constructor %make-field))
  ""
  access-flags
  name
  descriptor
  attributes)

(defun make-field (name type &key (flags '(:public)))
  
  (%make-field :access-flags flags
               :name name
               :descriptor type))

(defun field-add-attribute (field attribute)
  (push attribute (field-attributes field)))

(defun field-attribute (field name)
  (find name (field-attributes field)
        :test #'string= :key #'attribute-name))

(defun finalize-field (field class)
  (let ((pool (class-file-constants class)))
    (setf (field-access-flags field)
          (map-flags (field-access-flags field))
          (field-descriptor field)
          (pool-add-utf8 pool (internal-field-type (field-descriptor field)))
          (field-name field)
          (pool-add-utf8 pool (field-name field))))
  (finalize-attributes (field-attributes field) nil class))

(defun !write-field (field stream)
  (write-u2 (field-access-flags field) stream)
  (write-u2 (field-name field) stream)
  (write-u2 (field-descriptor field) stream)
  (write-attributes (field-attributes field) stream))


(defstruct (method (:constructor %!make-method))
  access-flags
  name
  descriptor
  attributes)


(defun map-method-name (name)
  "Methods should be identified by strings containing their names, or,
be one of two keyword identifiers to identify special methods:

 * :class-constructor
 * :constructor
"
  (cond
    ((eq name :class-constructor)
     "<clinit>")
    ((eq name :constructor)
     "<init>")
    (t name)))

(defun !make-method (name return args &key (flags '(:public)))
  (%!make-method :descriptor (cons return args)
                :access-flags flags
                :name name))

(defun method-add-attribute (method attribute)
  "Add `attribute' to the list of attributes of `method',
returning `attribute'."
  (push attribute (method-attributes method))
  attribute)

(defun method-add-code (method)
  "Creates an (empty) 'Code' attribute for the method,
returning the created attribute."
  (method-add-attribute
   method
   (make-code-attribute (+ (length (cdr (method-descriptor method)))
                           (if (member :static (method-access-flags method))
                               0 1))))) ;; 1 == implicit 'this'

(defun method-ensure-code (method)
  "Ensures the existence of a 'Code' attribute for the method,
returning the attribute."
  (let ((code (method-attribute method "Code")))
    (if (null code)
        (method-add-code method)
        code)))

(defun method-attribute (method name)
  (find name (method-attributes method)
        :test #'string= :key #'attribute-name))


(defun finalize-method (method class)
  (let ((pool (class-file-constants class)))
    (setf (method-access-flags method)
          (map-flags (method-access-flags method))
          (method-descriptor method)
          (pool-add-utf8 pool (apply #'descriptor (method-descriptor method)))
          (method-name method)
          (pool-add-utf8 pool (map-method-name (method-name method)))))
  (finalize-attributes (method-attributes method) nil class))


(defun !write-method (method stream)
  (write-u2 (method-access-flags method) stream)
  (write-u2 (method-name method) stream)
  (sys::%format t "method-name: ~a~%" (method-name method))
  (write-u2 (method-descriptor method) stream)
  (write-attributes (method-attributes method) stream))

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
          (pool-add-utf8 (class-file-constants class)
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
        (write-u4 (length array) stream)
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
  exception-handlers
  attributes

  ;; fields not in the class file start here

  ;; labels contains offsets into the code array after it's finalized
  labels ;; an alist

  current-local) ;; used for handling nested WITH-CODE-TO-METHOD blocks



(defun code-label-offset (code label)
  (cdr (assoc label (code-labels code))))

(defun (setf code-label-offset) (offset code label)
  (setf (code-labels code)
        (acons label offset (code-labels code))))



(defun !finalize-code (code parent class)
  (declare (ignore parent))
  (let ((c (resolve-instructions (coerce (reverse (code-code code)) 'vector))))
    (setf (code-max-stack code) (analyze-stack c))
    (multiple-value-bind
          (c labels)
        (code-bytes c)
      (setf (code-code code) c
            (code-labels code) labels)))

  (dolist (exception (code-exception-handlers code))
    (setf (exception-start-pc exception)
          (code-label-offset code (exception-start-pc exception))
          (exception-end-pc exception)
          (code-label-offset code (exception-end-pc exception))
          (exception-handler-pc exception)
          (code-label-offset code (exception-handler-pc exception))
          (exception-catch-type exception)
          (if (null (exception-catch-type exception))
              0  ;; generic 'catch all' class index number
              (pool-add-class (class-file-constants class)
                              (exception-catch-type exception)))))

  (finalize-attributes (code-attributes code) code class))

(defun !write-code (code stream)
  (sys::%format t "max-stack: ~a~%" (code-max-stack code))
  (write-u2 (code-max-stack code) stream)
  (sys::%format t "max-locals: ~a~%" (code-max-locals code))
  (write-u2 (code-max-locals code) stream)
  (let ((code-array (code-code code)))
    (sys::%format t "length: ~a~%" (length code-array))
    (write-u4 (length code-array) stream)
    (dotimes (i (length code-array))
      (write-u1 (svref code-array i) stream)))

  (write-u2 (length (code-exception-handlers code)) stream)
  (dolist (exception (reverse (code-exception-handlers code)))
    (sys::%format t "start-pc: ~a~%" (exception-start-pc exception))
    (write-u2 (exception-start-pc exception) stream)
    (sys::%format t "end-pc: ~a~%" (exception-end-pc exception))
    (write-u2 (exception-end-pc exception) stream)
    (sys::%format t "handler-pc: ~a~%" (exception-handler-pc exception))
    (write-u2 (exception-handler-pc exception) stream)
    (write-u2 (exception-catch-type exception) stream))

  (write-attributes (code-attributes code) stream))

(defun make-code-attribute (arg-count)
  "Creates an empty 'Code' attribute for a method which takes
`arg-count` parameters, including the implicit `this` parameter."
  (%make-code-attribute :max-locals arg-count))

(defun code-add-attribute (code attribute)
  "Adds `attribute' to `code', returning `attribute'."
  (push attribute (code-attributes code))
  attribute)

(defun code-attribute (code name)
  (find name (code-attributes code)
        :test #'string= :key #'attribute-name))


(defun code-add-exception-handler (code start end handler type)
  (push (make-exception :start-pc start
                        :end-pc end
                        :handler-pc handler
                        :catch-type type)
        (code-exception-handlers code)))

(defun add-exception-handler (start end handler type)
  (code-add-exception-handler *current-code-attribute* start end handler type))

(defstruct exception
  start-pc    ;; label target
  end-pc      ;; label target
  handler-pc  ;; label target
  catch-type  ;; a string for a specific type, or NIL for all
  )


(defvar *current-code-attribute* nil)

(defun save-code-specials (code)
  (setf (code-code code) *code*
        (code-max-locals code) *registers-allocated*
;;        (code-exception-handlers code) *handlers*
        (code-current-local code) *register*))

(defun restore-code-specials (code)
  (setf *code* (code-code code)
;;        *handlers* (code-exception-handlers code)
        *registers-allocated* (code-max-locals code)
        *register* (code-current-local code)))

(defmacro with-code-to-method ((class-file method &key safe-nesting) &body body)
  (let ((m (gensym))
        (c (gensym)))
    `(progn
       ,@(when safe-nesting
           `((when *current-code-attribute*
               (save-code-specials *current-code-attribute*))))
       (let* ((,m ,method)
              (,c (method-ensure-code method))
              (*pool* (class-file-constants ,class-file))
              (*code* (code-code ,c))
              (*registers-allocated* (code-max-locals ,c))
              (*register* (code-current-local ,c))
              (*current-code-attribute* ,c))
         ,@body
         (setf (code-code ,c) *code*
;;               (code-exception-handlers ,c) *handlers*
               (code-max-locals ,c) *registers-allocated*))
       ,@(when safe-nesting
           `((when *current-code-attribute*
               (restore-code-specials *current-code-attribute*)))))))


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

