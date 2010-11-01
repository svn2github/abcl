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
  "Used for class identification.

The caller should instantiate only one `class-name' per class, as they are
used as class identifiers and compared using EQ.

Some instructions need a class argument, others need a reference identifier.
This class is used to abstract from the difference."
  name-internal
  ref
  array-class ;; cached array class reference
  ;; keeping a reference to the associated array class allows class
  ;; name comparisons to be EQ: all classes should exist only once,
  )

(defun make-class-name (name)
  "Creates a `class-name' structure for the class or interface `name'.

`name' should be specified using Java representation, which is converted
to 'internal' (JVM) representation by this function."
  (setf name (substitute #\/ #\. name))
  (%make-class-name :name-internal name
                    :ref (concatenate 'string "L" name ";")))

(defun class-array (class-name)
  "Returns a class-name representing an array of `class-name'.
For multi-dimensional arrays, call this function multiple times, using
its own result.

This function can be called multiple times on the same `class-name' without
violating the 'only one instance' requirement: the returned value is cached
and used on successive calls."
  (unless (class-array-class class-name)
    ;; Alessio Stalla found by dumping a class file that the JVM uses
    ;; the same representation (ie '[L<class-name>;') in CHECKCAST as
    ;; it does in field references, meaning the class name and class ref
    ;; are identified by the same string
    (let ((name-and-ref (concatenate 'string "[" (class-ref class-name))))
      (setf (class-array-class class-name)
            (%make-class-name :name-internal name-and-ref
                              :ref name-and-ref))))
  (class-array-class class-name))

(defmacro define-class-name (symbol java-dotted-name &optional documentation)
  "Convenience macro to define constants for `class-name' structures,
initialized from the `java-dotted-name'."
  `(defconstant ,symbol (make-class-name ,java-dotted-name)
     ,documentation))

(define-class-name +java-object+ "java.lang.Object")
(define-class-name +java-string+ "java.lang.String")
(define-class-name +java-system+ "java.lang.System")
(define-class-name +java-class+ "java.lang.Class")
(define-class-name +lisp-object+ "org.armedbear.lisp.LispObject")
(define-class-name +dyn-linkage+ "java.dyn.Linkage")
(define-class-name +dyn-invokedynamic+ "java.dyn.InvokeDynamic")
(defconstant +lisp-object-array+ (class-array +lisp-object+))
(define-class-name +lisp-simple-string+ "org.armedbear.lisp.SimpleString")
(define-class-name +lisp+ "org.armedbear.lisp.Lisp")
(define-class-name +lisp-nil+ "org.armedbear.lisp.Nil")
(define-class-name +lisp-class+ "org.armedbear.lisp.LispClass")
(define-class-name +lisp-symbol+ "org.armedbear.lisp.Symbol")
(define-class-name +lisp-thread+ "org.armedbear.lisp.LispThread")
(define-class-name +lisp-closure-binding+ "org.armedbear.lisp.ClosureBinding")
(defconstant +closure-binding-array+ (class-array +lisp-closure-binding+))
(define-class-name +lisp-integer+ "org.armedbear.lisp.LispInteger")
(define-class-name +lisp-fixnum+ "org.armedbear.lisp.Fixnum")
(defconstant +lisp-fixnum-array+ (class-array +lisp-fixnum+))
(define-class-name +lisp-bignum+ "org.armedbear.lisp.Bignum")
(define-class-name +lisp-single-float+ "org.armedbear.lisp.SingleFloat")
(define-class-name +lisp-double-float+ "org.armedbear.lisp.DoubleFloat")
(define-class-name +lisp-cons+ "org.armedbear.lisp.Cons")
(define-class-name +lisp-load+ "org.armedbear.lisp.Load")
(define-class-name +lisp-character+ "org.armedbear.lisp.LispCharacter")
(defconstant +lisp-character-array+ (class-array +lisp-character+))
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
(define-class-name +lisp-function+ "org.armedbear.lisp.Function")
(define-class-name +lisp-eql-hash-table+ "org.armedbear.lisp.EqlHashTable")
(define-class-name +lisp-hash-table+ "org.armedbear.lisp.HashTable")
(define-class-name +lisp-package+ "org.armedbear.lisp.Package")
(define-class-name +lisp-readtable+ "org.armedbear.lisp.Readtable")
(define-class-name +lisp-stream+ "org.armedbear.lisp.Stream")
(define-class-name +lisp-closure+ "org.armedbear.lisp.Closure")
(define-class-name +lisp-compiled-closure+ "org.armedbear.lisp.CompiledClosure")
(define-class-name +lisp-closure-parameter+
    "org.armedbear.lisp.Closure$Parameter")
(defconstant +lisp-closure-parameter-array+
  (class-array +lisp-closure-parameter+))

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
  (let* ((arg-strings (mapcar #'internal-field-ref argument-types))
         (ret-string (internal-field-ref return-type))
         (size (+ 2 (reduce #'+ arg-strings
                            :key #'length
                            :initial-value (length ret-string))))
         (str (make-array size :fill-pointer 0 :element-type 'character)))
    (with-output-to-string (s str)
      (princ #\( s)
      (dolist (arg-string arg-strings)
        (princ arg-string s))
      (princ #\) s)
      (princ ret-string s))
    ;(sys::%format t "descriptor ~S ~S -> ~S~%" return-type argument-types str)
    str)
;;  (format nil "(~{~A~})~A" 
;;          (internal-field-ref return-type))
  )

(defun descriptor-stack-effect (return-type &rest argument-types)
  "Returns the effect on the stack position of the `argument-types' and
`return-type' of a method call.

If the method consumes an implicit `this' argument, this function does not
take that effect into account."
  (flet ((type-stack-effect (arg)
           (case arg
             ((:long :double) 2)
             ((nil :void) 0)
             (otherwise 1))))
    (+ (reduce #'- argument-types
               :key #'type-stack-effect
               :initial-value 0)
       (type-stack-effect return-type))))


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

(defun constant-type (constant)
  (car (find (constant-tag constant) +constant-type-map+ :key #'cadr)))

(defstruct (constant-class (:constructor make-constant-class (index name-index))
                           (:include constant
                                     (tag 7)))
  "Structure holding information on a 'class' type item in the constant pool."
  name-index)

(defstruct (constant-member-ref (:constructor
                                 %make-constant-member-ref
                                     (tag index class name/type))
                                (:include constant))
  "Structure holding information on a member reference type item
(a field, method or interface method reference) in the constant pool."
  class
  name/type)

(declaim (inline make-constant-field-ref make-constant-method-ref
                 make-constant-interface-method-ref))
(defun make-constant-field-ref (index class name/type)
  "Creates a `constant-member-ref' instance containing a field reference."
  (%make-constant-member-ref 9 index class name/type))

(defun make-constant-method-ref (index class name/type)
  "Creates a `constant-member-ref' instance containing a method reference."
  (%make-constant-member-ref 10 index class name/type))

(defun make-constant-interface-method-ref (index class name/type)
  "Creates a `constant-member-ref' instance containing an
interface-method reference."
  (%make-constant-member-ref 11 index class name/type))

(defstruct (constant-string (:constructor
                             make-constant-string (index value-index))
                            (:include constant
                                      (tag 8)))
  "Structure holding information on a 'string' type item in the constant pool."
  value-index)

(defstruct (constant-float/int (:constructor
                                %make-constant-float/int (tag index value))
                               (:include constant))
  "Structure holding information on a 'float' or 'integer' type item
in the constant pool."
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
  "Structure holding information on a 'double' or 'long' type item
in the constant pool."
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
                                                         name
							 type
                                                         descriptor))
                               (:include constant
                                         (tag 12)))
  "Structure holding information on a 'name-and-type' type item in the
constant pool; this type of element is used by 'member-ref' type items."
  name
  type
  descriptor)

(defstruct (constant-utf8 (:constructor make-constant-utf8 (index value))
                          (:include constant
                                    (tag 1)))
  "Structure holding information on a 'utf8' type item in the constant pool;

This type of item is used for text representation of identifiers
and string contents."
  value)


(defun pool-add-class (pool class)
  "Returns the constant-pool class item for `class'.

`class' must be an instance of `class-name'."
  (let ((entry (gethash class (pool-entries pool))))
    (unless entry
      (let ((utf8 (constant-index (pool-add-utf8 pool (class-name-internal class)))))
        (setf entry
              (make-constant-class (incf (pool-index pool)) utf8)
              (gethash class (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    entry))

(defun pool-add-field-ref (pool class name type)
  "Returns the constant-pool item which denotes a reference
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
    entry))

(defun pool-add-method-ref (pool class name type)
  "Returns the constant-pool item which denotes a reference
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
    entry))

(defun pool-add-interface-method-ref (pool class name type)
  "Returns the constant-pool item which denotes a reference to
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
    entry))

(defun pool-add-string (pool string)
  "Returns the constant-pool item denoting the string."
  (let ((entry (gethash (cons 8 string) ;; 8 == string-tag
                        (pool-entries pool))))
    (unless entry
      (let ((utf8 (pool-add-utf8 pool string)))
        (setf entry (make-constant-string (incf (pool-index pool))
					  (constant-index utf8))
              (gethash (cons 8 string) (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    entry))

(defun pool-add-int (pool int)
  "Returns the constant-pool item denoting the int."
  (let ((entry (gethash (cons 3 int) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-int (incf (pool-index pool)) int)
            (gethash (cons 3 int) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool)))
    entry))

(defun pool-add-float (pool float)
  "Returns the constant-pool item denoting the float."
  (let ((entry (gethash (cons 4 float) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-float (incf (pool-index pool))
                                       (sys::%float-bits float))
            (gethash (cons 4 float) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool)))
    entry))

(defun pool-add-long (pool long)
  "Returns the constant-pool item denoting the long."
  (let ((entry (gethash (cons 5 long) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-long (incf (pool-index pool)) long)
            (gethash (cons 5 long) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool))
      (incf (pool-index pool))) ;; double index increase; long takes 2 slots
    entry))

(defun pool-add-double (pool double)
  "Returns constant-pool item denoting the double."
  (let ((entry (gethash (cons 6 double) (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-double (incf (pool-index pool))
                                        (sys::%float-bits double))
            (gethash (cons 6 double) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool))
      (incf (pool-index pool))) ;; double index increase; 'double' takes 2 slots
    entry))

(defun pool-add-name/type (pool name type)
  "Returns the constant-pool item denoting the name/type identifier."
  (let ((entry (gethash (cons name type) (pool-entries pool)))
        (internal-type (if (listp type)
                           (apply #'descriptor type)
                           (internal-field-ref type))))
    (unless entry
      (let ((n (pool-add-utf8 pool name))
            (i-t (pool-add-utf8 pool internal-type)))
        (setf entry (make-constant-name/type
		     (incf (pool-index pool)) n type i-t)
              (gethash (cons name type) (pool-entries pool)) entry))
      (push entry (pool-entries-list pool)))
    entry))

(defun pool-add-utf8 (pool utf8-as-string)
  "Returns the textual value that will be stored in the class file as UTF-8 encoded data."
  (let ((entry (gethash (cons 11 utf8-as-string) ;; 11 == utf8
                        (pool-entries pool))))
    (unless entry
      (setf entry (make-constant-utf8 (incf (pool-index pool)) utf8-as-string)
            (gethash (cons 11 utf8-as-string) (pool-entries pool)) entry)
      (push entry (pool-entries-list pool)))
    entry))

(defstruct (class-file (:constructor
                        make-class-file (class superclass access-flags)))
  "Holds the components of a class file."
  (constants (make-pool))
  (major-version 51)
  (minor-version 0)
  access-flags
  class
  superclass
  ;; support for implementing interfaces not yet available
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
  (remove (map-method-name name) (class-file-methods class)
          :test-not #'string= :key #'method-name))

(defun class-method (class name return &rest args)
  "Return the method which is (uniquely) identified by its name AND descriptor."
  (let ((return-and-args (cons return args))
        (name (map-method-name name)))
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
        (constant-index (pool-add-class (class-file-constants class)
					(class-file-superclass class)))
        (class-file-class class)
        (constant-index (pool-add-class (class-file-constants class)
					(class-file-class class))))
  ;;  (finalize-interfaces)
  (dolist (field (class-file-fields class))
    (finalize-field field class))
  (dolist (method (class-file-methods class))
    (finalize-method method class))
  ;; top-level attributes (no parent attributes to refer to)
  (finalize-attributes (class-file-attributes class) nil class))


(declaim (inline write-u1 write-u2 write-u4 write-s4))
(defun write-u1 (n stream)
  (declare (optimize speed))
  (declare (type (unsigned-byte 8) n))
  (declare (type stream stream))
  (write-8-bits n stream))

(defknown write-u2 (t t) t)
(defun write-u2 (n stream)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) n))
  (declare (type stream stream))
  (write-8-bits (logand (ash n -8) #xFF) stream)
  (write-8-bits (logand n #xFF) stream))

(defknown write-u4 (integer stream) t)
(defun write-u4 (n stream)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) n))
  (write-u2 (logand (ash n -16) #xFFFF) stream)
  (write-u2 (logand n #xFFFF) stream))

(declaim (ftype (function (t t) t) write-s4))
(defun write-s4 (n stream)
  (declare (optimize speed))
  (cond ((minusp n)
         (write-u4 (1+ (logxor (- n) #xFFFFFFFF)) stream))
        (t
         (write-u4 n stream))))

(declaim (ftype (function (t t t) t) write-ascii))
(defun write-ascii (string length stream)
  (declare (type string string))
  (declare (type (unsigned-byte 16) length))
  (declare (type stream stream))
  (write-u2 length stream)
  (dotimes (i length)
    (declare (type (unsigned-byte 16) i))
    (write-8-bits (char-code (char string i)) stream)))


(declaim (ftype (function (t t) t) write-utf8))
(defun write-utf8 (string stream)
  (declare (optimize speed))
  (declare (type string string))
  (declare (type stream stream))
  (let ((length (length string))
        (must-convert nil))
    (declare (type fixnum length))
    (dotimes (i length)
      (declare (type fixnum i))
      (unless (< 0 (char-code (char string i)) #x80)
        (setf must-convert t)
        (return)))
    (if must-convert
        (let ((octets (make-array (* length 2)
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0)))
          (declare (type (vector (unsigned-byte 8)) octets))
          (dotimes (i length)
            (declare (type fixnum i))
            (let* ((c (char string i))
                   (n (char-code c)))
              (cond ((zerop n)
                     (vector-push-extend #xC0 octets)
                     (vector-push-extend #x80 octets))
                    ((< 0 n #x80)
                     (vector-push-extend n octets))
                    (t
                     (let ((char-octets (char-to-utf8 c)))
                       (dotimes (j (length char-octets))
                         (declare (type fixnum j))
                         (vector-push-extend (svref char-octets j) octets)))))))
          (write-u2 (length octets) stream)
          (dotimes (i (length octets))
            (declare (type fixnum i))
            (write-8-bits (aref octets i) stream)))
        (write-ascii string length stream))))


(defun write-class-file (class stream)
  "Serializes `class' to `stream', after it has been finalized."

  ;; header
  (write-u4 #xCAFEBABE stream)
  (write-u2 (class-file-minor-version class) stream)
  (write-u2 (class-file-major-version class) stream)

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
    (write-field field stream))

  ;; methods
  (write-u2 (length (class-file-methods class)) stream)
  (dolist (method (class-file-methods class))
    (write-method method stream))

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
           (write-u2 (constant-index (constant-member-ref-class entry)) stream)
           (write-u2 (constant-index (constant-member-ref-name/type entry)) stream))
          (12                           ; nameAndType
           (write-u2 (constant-index (constant-name/type-name entry)) stream)
           (write-u2 (constant-index (constant-name/type-descriptor entry)) stream))
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
                               (constant-member-ref-class entry)
                               (constant-member-ref-name/type entry)))
      (12 (sys::%format t "n/t: ~a,~a~%"
                        (constant-name/type-name entry)
                        (constant-name/type-descriptor entry)))
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
  "Holds information on the properties of fields in the class(-file)."
  access-flags
  name
  descriptor
  attributes)

(defun make-field (name type &key (flags '(:public)))
  "Creates a field for addition to a class file."
  (%make-field :access-flags flags
               :name name
               :descriptor type))

(defun field-add-attribute (field attribute)
  "Adds an attribute to a field."
  (push attribute (field-attributes field)))

(defun field-attribute (field name)
  "Retrieves an attribute named `name' of `field'.

Returns NIL if the attribute isn't found."
  (find name (field-attributes field)
        :test #'string= :key #'attribute-name))

(defun finalize-field (field class)
  "Prepares `field' for serialization."
  (let ((pool (class-file-constants class)))
    (setf (field-access-flags field)
          (map-flags (field-access-flags field))
          (field-descriptor field)
          (constant-index (pool-add-utf8 pool (internal-field-ref (field-descriptor field))))
          (field-name field)
          (constant-index (pool-add-utf8 pool (field-name field)))))
  (finalize-attributes (field-attributes field) nil class))

(defun write-field (field stream)
  "Writes classfile representation of `field' to `stream'."
  (write-u2 (field-access-flags field) stream)
  (write-u2 (field-name field) stream)
  (write-u2 (field-descriptor field) stream)
  (write-attributes (field-attributes field) stream))


(defstruct (method (:constructor %make-method)
                   (:conc-name method-))
  "Holds information on the properties of methods in the class(-file)."
  access-flags
  name
  descriptor
  attributes)


(defun map-method-name (name)
  "Methods should be identified by strings containing their names, or,
be one of two keyword identifiers to identify special methods:

 * :static-initializer
 * :constructor
"
  (cond
    ((eq name :static-initializer)
     "<clinit>")
    ((eq name :constructor)
     "<init>")
    (t name)))

(defun make-method (name return args &key (flags '(:public)))
  "Creates a method for addition to a class file."
  (%make-method :descriptor (cons return args)
                :access-flags flags
                :name (map-method-name name)))

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
   (make-code-attribute (compute-initial-method-locals method))))

(defun method-ensure-code (method)
  "Ensures the existence of a 'Code' attribute for the method,
returning the attribute."
  (let ((code (method-attribute method "Code")))
    (if (null code)
        (method-add-code method)
        code)))

(defun method-attribute (method name)
  "Returns the first attribute of `method' with `name'."
  (find name (method-attributes method)
        :test #'string= :key #'attribute-name))


(defun finalize-method (method class)
  "Prepares `method' for serialization."
  (let ((pool (class-file-constants class)))
    (setf (method-access-flags method)
          (map-flags (method-access-flags method))
          (method-descriptor method)
          (constant-index (pool-add-utf8 pool (apply #'descriptor (method-descriptor method))))
          (method-name method)
          (constant-index (pool-add-utf8 pool (method-name method)))))
    (finalize-attributes (method-attributes method) method class))


(defun write-method (method stream)
  "Write class file representation of `method' to `stream'."
  (write-u2 (method-access-flags method) stream)
  (write-u2 (method-name method) stream)
  ;;(sys::%format t "method-name: ~a~%" (method-name method))
  (write-u2 (method-descriptor method) stream)
  (write-attributes (method-attributes method) stream))

(defstruct attribute
  "Parent attribute structure to be included into other attributes, mainly
to define common fields.

Having common fields allows common driver code for
finalizing and serializing attributes."
  name

  ;; not in the class file:
  finalizer  ;; function of 3 arguments: the attribute, parent and class-file
  writer     ;; function of 2 arguments: the attribute and the output stream
  )

(defun finalize-attributes (attributes att class)
  "Prepare `attributes' (a list) of attribute `att' list for serialization."
  (dolist (attribute attributes)
    ;; assure header: make sure 'name' is in the pool
    (setf (attribute-name attribute)
          (constant-index (pool-add-utf8 (class-file-constants class)
					 (attribute-name attribute))))
    ;; we're saving "root" attributes: attributes which have no parent
    (funcall (attribute-finalizer attribute) attribute att class)))

(defun write-attributes (attributes stream)
  "Writes the `attributes' to `stream'."
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
                                     (finalizer #'finalize-code-attribute)
                                     (writer #'write-code-attribute))
                           (:constructor %make-code-attribute))
  "The attribute containing the actual JVM byte code;
an attribute of a method."
  max-stack
  max-locals
  code
  exception-handlers
  attributes

  ;; fields not in the class file start here

  ;; labels contains offsets into the code array after it's finalized
  labels ;; an alist

  ;; these are used for handling nested WITH-CODE-TO-METHOD blocks
  (current-local 0)
  computed-locals)



(defun code-label-offset (code label)
  "Retrieves the `label' offset within a `code' attribute after the
attribute has been finalized."
  (cdr (assoc label (code-labels code))))

(defun (setf code-label-offset) (offset code label)
  "Sets the `label' offset within a `code' attribute after the attribute
has been finalized."
  (setf (code-labels code)
        (acons label offset (code-labels code))))

(defun finalize-code-attribute (code parent class)
  "Prepares the `code' attribute for serialization, within method `parent'."
  (let* ((handlers (code-exception-handlers code))
         (c (finalize-code
                     (code-code code)
                     (nconc (mapcar #'exception-start-pc handlers)
                            (mapcar #'exception-end-pc handlers)
                            (mapcar #'exception-handler-pc handlers))
                     t))
	 (compute-stack-map-table-p (>= (class-file-major-version class) 50)))
    (unless (code-max-stack code)
      (setf (code-max-stack code)
            (analyze-stack c (mapcar #'exception-handler-pc handlers))))
    (unless (code-max-locals code)
      (setf (code-max-locals code)
            (analyze-locals code)))
    (multiple-value-bind
          (c labels stack-map-table)
        (resolve-code code c class parent compute-stack-map-table-p)
      (setf (code-code code) c
            (code-labels code) labels)
      (when compute-stack-map-table-p
	(code-add-attribute
	 code
	 (make-stack-map-table-attribute :entries stack-map-table)))))

  (setf (code-exception-handlers code)
        (remove-if #'(lambda (h)
                       (eql (code-label-offset code (exception-start-pc h))
                            (code-label-offset code (exception-end-pc h))))
                   (code-exception-handlers code)))

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
              (constant-index (pool-add-class (class-file-constants class)
					      (exception-catch-type exception))))))

  (finalize-attributes (code-attributes code) code class))

(defun write-code-attribute (code stream)
  "Writes the attribute `code' to `stream'."
  ;;(sys::%format t "max-stack: ~a~%" (code-max-stack code))
  (write-u2 (code-max-stack code) stream)
  ;;(sys::%format t "max-locals: ~a~%" (code-max-locals code))
  (write-u2 (code-max-locals code) stream)
  (let ((code-array (code-code code)))
    ;;(sys::%format t "length: ~a~%" (length code-array))
    (write-u4 (length code-array) stream)
    (dotimes (i (length code-array))
      (write-u1 (svref code-array i) stream)))

  (write-u2 (length (code-exception-handlers code)) stream)
  (dolist (exception (reverse (code-exception-handlers code)))
    ;;(sys::%format t "start-pc: ~a~%" (exception-start-pc exception))
    (write-u2 (exception-start-pc exception) stream)
    ;;(sys::%format t "end-pc: ~a~%" (exception-end-pc exception))
    (write-u2 (exception-end-pc exception) stream)
    ;;(sys::%format t "handler-pc: ~a~%" (exception-handler-pc exception))
    (write-u2 (exception-handler-pc exception) stream)
    (write-u2 (exception-catch-type exception) stream))

  (write-attributes (code-attributes code) stream))

(defun make-code-attribute (locals)
  "Creates an empty 'Code' attribute for a method which takes
`arg-count` parameters, including the implicit `this` parameter."
  (%make-code-attribute :max-locals (length locals)
			:computed-locals locals))

(defun code-add-attribute (code attribute)
  "Adds `attribute' to `code', returning `attribute'."
  (push attribute (code-attributes code))
  attribute)

(defun code-attribute (code name)
  "Returns an attribute of `code' identified by `name'."
  (find name (code-attributes code)
        :test #'string= :key #'attribute-name))


(defun code-add-exception-handler (code start end handler type)
  "Adds an exception handler to `code' protecting the region from
labels `start' to `end' (inclusive) from exception `type' - where
a value of NIL indicates all types. Upon an exception of the given
type, control is transferred to label `handler'."
  (push (make-exception :start-pc start
                        :end-pc end
                        :handler-pc handler
                        :catch-type type)
        (code-exception-handlers code)))

(defun resolve-code (code-attr code class method compute-stack-map-table-p)
  "Walks the code, replacing symbolic labels with numeric offsets, and optionally computing the stack map table."
  (let* ((length 0)
	 labels ;; alist
	 stack-map-table
	 (computing-stack-map-table compute-stack-map-table-p)
	 (*code-locals* (code-computed-locals code-attr))
	 *code-stack*)
#||	 (*basic-block* (when compute-stack-map-table-p
			  (make-basic-block
			   :offset 0
			   :input-locals
			   (method-initial-locals method))))
	 (root-block *basic-block*)
	 *basic-blocks*)||#
    (declare (type (unsigned-byte 16) length))
    ;; Pass 1: calculate label offsets and overall length and, if
    ;; compute-stack-map-table-p is true, also simulate the effect of the
    ;; instructions on the stack and locals.
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
	(setf (instruction-offset instruction) length)
	;;(sys::format t "simulating instruction ~S ~S stack ~S locals ~S ~%"
	;;opcode (mapcar #'type-of (instruction-args instruction))
	;;(length *code-stack*) (length *code-locals*))
	(if computing-stack-map-table
	    (progn
	      (when (= opcode 202) ;;label: simulate a jump
		(record-jump-to-label (first (instruction-args instruction))))
	      (simulate-instruction-effect instruction)
	      ;;Simulation must be stopped if we encounter a goto, it will be
	      ;;resumed by the next label that is the target of a jump
	      (setf computing-stack-map-table (not (unconditional-jump-p opcode))))
	    (when (and (= opcode 202) ; LABEL
		       (get (first (instruction-args instruction))
			    'jump-target-p))
	      (simulate-instruction-effect instruction)
	      (setf computing-stack-map-table t)))
        (if (= opcode 202) ; LABEL
            (let ((label (car (instruction-args instruction))))
              (set label length)
              (setf labels
                    (acons label length labels)))
	    (incf length (opcode-size opcode)))))
    ;;Pass 2 (optional): compute the stack map table
    (when compute-stack-map-table-p
      (let ((last-frame-offset 0)
	    (must-emit-frame nil))
	(dotimes (i (length code))
	  (let ((instruction (aref code i))
		(make-variable-info (lambda (type)
				      (smf-type->variable-info type class))))
	    (cond
	      ((= (instruction-opcode instruction) 202) ; LABEL
	       (let* ((label (car (instruction-args instruction)))
		      (offset (symbol-value label))
		      (*print-circle* t))
		 (if (get label 'jump-target-p)
		     (let ((frame
			    (make-stack-map-full-frame
			     :offset-delta (- offset last-frame-offset)
			     :locals
			     (mapcar make-variable-info
				     (instruction-input-locals instruction))
			     :stack-items
			     (mapcar make-variable-info
				     (instruction-input-stack instruction)))))
		       (push frame stack-map-table)
		       (sys::%format t "emit frame ~S @ ~A (~A)~%"
				     frame offset (- offset last-frame-offset))
		       (setf last-frame-offset offset))
		     (sys::%format t "error - label not target of a jump: ~S~%" label))
		 )))))))
    ;;Pass 3: replace labels with calculated offsets.
    (let ((index 0))
      (declare (type (unsigned-byte 16) index))
      (dotimes (i (length code))
        (declare (type (unsigned-byte 16) i))
        (let ((instruction (aref code i)))
          (when (branch-p (instruction-opcode instruction))
            (let* ((label (car (instruction-args instruction)))
                   (offset (- (the (unsigned-byte 16)
                                (symbol-value (the symbol label)))
                              index)))
              (setf (instruction-args instruction) (s2 offset))))
          (unless (= (instruction-opcode instruction) 202) ; LABEL
            (incf index (opcode-size (instruction-opcode instruction)))))))
    ;;Pass 4: expand instructions into bytes,
    ;;skipping LABEL pseudo-instructions.
    (let ((bytes (make-array length))
          (index 0))
      (declare (type (unsigned-byte 16) index))
      (dotimes (i (length code))
        (declare (type (unsigned-byte 16) i))
        (let ((instruction (aref code i)))
          (unless (= (instruction-opcode instruction) 202) ; LABEL
            (setf (svref bytes index) (instruction-opcode instruction))
            (incf index)
            (dolist (arg (instruction-args instruction))
	      (if (constant-p arg)
		  (let ((idx (constant-index arg))
			(opcode (instruction-opcode instruction)))
		    (if (or (<= 178 opcode 187)
			    (= opcode 189)
			    (= opcode 192)
			    (= opcode 193))
			(let ((idx (u2 idx)))
			  (setf (svref bytes index) (car idx)
				(svref bytes (1+ index)) (cadr idx))
			  (incf index 2))
			(progn
			  (setf (svref bytes index) idx)
			  (incf index))))
		  (progn
		    (setf (svref bytes index) arg)
		    (incf index)))))))
      (values bytes labels (nreverse stack-map-table)))))

(defun unconditional-jump-p (opcode)
  (= opcode 167))

(defstruct exception
  "Exception handler information.

After finalization, the fields contain offsets instead of labels."
  start-pc    ;; label target
  end-pc      ;; label target
  handler-pc  ;; label target
  catch-type  ;; a string for a specific type, or NIL for all
  )


(defstruct (constant-value-attribute (:conc-name constant-value-)
                                     (:include attribute
                                               (name "ConstantValue")
                                               ;; finalizer
                                               ;; writer
                                               ))
  "An attribute of a field of primitive type.

"
  ;;; ### TODO
  )


(defstruct (checked-exceptions-attribute
             (:conc-name checked-)
             (:include attribute
                       (name "Exceptions")
                       (finalizer #'finalize-checked-exceptions)
                       (writer #'write-checked-exceptions)))
  "An attribute of `code-attribute', "
  table ;; a list of checked classes corresponding to Java's 'throws'
)

(defun finalize-checked-exceptions (checked-exceptions code class)
  (declare (ignorable code class))

  "Prepare `checked-exceptions' for serialization."
  (setf (checked-table checked-exceptions)
        (mapcar #'(lambda (exception)
                    (constant-index (pool-add-class (class-file-constants class)
						    exception)))
                (checked-table checked-exceptions))))

(defun write-checked-exceptions (checked-exceptions stream)
  "Write `checked-exceptions' to `stream' in class file representation."
  (write-u2 (length (checked-table checked-exceptions)) stream)
  (dolist (exception (reverse (checked-table checked-exceptions)))
    (write-u2 exception stream)))

;; Can't be used yet: serialization missing
(defstruct (deprecated-attribute (:include attribute
                                           (name "Deprecated")
                                           (finalizer (constantly nil))
                                           (writer (constantly nil))))
  ;; finalizer and writer need to do nothing: Deprecated attributes are empty
  "An attribute of a class file, field or method, indicating the element
to which it has been attached has been superseded.")

(defvar *current-code-attribute* nil)
(defvar *method* nil)

(defun save-code-specials (code)
  (setf (code-code code) *code*
        (code-max-locals code) *registers-allocated*
        (code-current-local code) *register*))

(defun restore-code-specials (code)
  (setf *code* (code-code code)
        *registers-allocated* (code-max-locals code)
        *register* (code-current-local code)))

(defmacro with-code-to-method ((class-file method)
                               &body body)
  (let ((m (gensym))
        (c (gensym)))
    `(progn
       (when *current-code-attribute*
         (save-code-specials *current-code-attribute*))
       (let* ((,m ,method)
	      (*method* ,m)
              (,c (method-ensure-code ,method))
              (*pool* (class-file-constants ,class-file))
              (*code* (code-code ,c))
              (*registers-allocated* (code-max-locals ,c))
              (*register* (code-current-local ,c))
              (*current-code-attribute* ,c))
         ,@body
         (setf (code-code ,c) *code*
               (code-current-local ,c) *register*
               (code-max-locals ,c) *registers-allocated*))
       (when *current-code-attribute*
         (restore-code-specials *current-code-attribute*)))))


(defstruct (source-file-attribute (:conc-name source-)
                                  (:include attribute
                                            (name "SourceFile")
                                            (finalizer #'finalize-source-file)
                                            (writer #'write-source-file)))
  "An attribute of the class file indicating which source file
it was compiled from."
  filename)

(defun finalize-source-file (source-file code class)
  (declare (ignorable code class))
  (setf (source-filename source-file)
        (constant-index (pool-add-utf8 (class-file-constants class)
				       (source-filename source-file)))))

(defun write-source-file (source-file stream)
  (write-u2 (source-filename source-file) stream))


(defstruct (synthetic-attribute (:include attribute
                                          (name "Synthetic")
                                          (finalizer (constantly nil))
                                          (writer (constantly nil))))
  ;; finalizer and writer need to do nothing: Synthetic attributes are empty
  "An attribute of a class file, field or method to mark that it wasn't
included in the sources - but was generated artificially.")


(defstruct (line-numbers-attribute
             (:conc-name line-numbers-)
             (:include attribute
                       (name "LineNumberTable")
                       (finalizer #'finalize-line-numbers)
                       (writer #'write-line-numbers)))
  "An attribute of `code-attribute', containing a mapping of offsets
within the code section to the line numbers in the source file."
  table ;; a list of line-number structures, in reverse order
  )

(defstruct line-number
  start-pc  ;; a label, before finalization, or 0 for "start of function"
  line)

(defun finalize-line-numbers (line-numbers code class)
  (declare (ignorable code class))
  (dolist (line-number (line-numbers-table line-numbers))
    (unless (zerop (line-number-start-pc line-number))
      (setf (line-number-start-pc line-number)
            (code-label-offset code (line-number-start-pc line-number))))))

(defun write-line-numbers (line-numbers stream)
  (write-u2 (length (line-numbers-table line-numbers)) stream)
  (dolist (line-number (reverse (line-numbers-table line-numbers)))
    (write-u2 (line-number-start-pc line-number) stream)
    (write-u2 (line-number-line line-number) stream)))

(defun line-numbers-add-line (line-numbers start-pc line)
  (push (make-line-number :start-pc start-pc :line line)
        (line-numbers-table line-numbers)))

(defstruct (local-variables-attribute
             (:conc-name local-var-)
             (:include attribute
                       (name "LocalVariableTable")
                       (finalizer #'finalize-local-variables)
                       (writer #'write-local-variables)))
  "An attribute of the `code-attribute', containing a table of local variable
names, their type and their scope of validity."
  table ;; a list of local-variable structures, in reverse order
  )

(defstruct (local-variable (:conc-name local-))
  start-pc  ;; a label, before finalization
  length    ;; a label (at the ending position) before finalization
  name
  descriptor
  index ;; The index of the variable inside the block of locals
  )

(defun finalize-local-variables (local-variables code class)
  (dolist (local-variable (local-var-table local-variables))
    (setf (local-start-pc local-variable)
          (code-label-offset code (local-start-pc local-variable))
          (local-length local-variable)
          ;; calculate 'length' from the distance between 2 labels
          (- (code-label-offset code (local-length local-variable))
             (local-start-pc local-variable))
          (local-name local-variable)
          (constant-index (pool-add-utf8 (class-file-constants class)
					 (local-name local-variable)))
          (local-descriptor local-variable)
          (constant-index (pool-add-utf8 (class-file-constants class)
					 (local-descriptor local-variable))))))

(defun write-local-variables (local-variables stream)
  (write-u2 (length (local-var-table local-variables)) stream)
  (dolist (local-variable (reverse (local-var-table local-variables)))
    (write-u2 (local-start-pc local-variable) stream)
    (write-u2 (local-length local-variable) stream)
    (write-u2 (local-name local-variable) stream)
    (write-u2 (local-descriptor local-variable) stream)
    (write-u2 (local-index local-variable) stream)))

;;Support for the StackMapTable attribute used by the typechecking verifier
;;from class file version number 50.0 onward (astalla)

(defstruct (stack-map-table-attribute
	     (:conc-name stack-map-table-)
	     (:include attribute
		       (name "StackMapTable")
		       (finalizer #'finalize-stack-map-table-attribute)
		       (writer #'write-stack-map-table-attribute)))
	     ;(:constructor %make-stack-map-table-attribute))
  "The attribute containing the stack map table, a map from bytecode offsets to frames containing information about the types of locals and values on the operand stack at that offset. This is an attribute of a method."
  entries)

(defun finalize-stack-map-table-attribute (table parent class)
  "Prepares the `stack-map-table' attribute for serialization, within method `parent': replaces all virtual types in the stack map frames with variable-info objects."
  (declare (ignore parent class)) ;;TODO
  table)

(defun write-stack-map-table-attribute (table stream)
  (write-u2 (length (stack-map-table-entries table)) stream)
  (dolist (frame (stack-map-table-entries table))
    (funcall (frame-writer frame) frame stream)))

(defstruct (stack-map-frame (:conc-name frame-))
  offset-delta
  writer)

(defstruct (stack-map-full-frame
	     (:conc-name full-frame-)
	     (:include stack-map-frame
		       (writer #'write-stack-map-full-frame)))
  locals
  stack-items)

(defun write-stack-map-full-frame (frame stream)
  (write-u1 255 stream)
  (write-u2 (frame-offset-delta frame) stream)
;;  (write-u2 0 stream)
;;  (write-u2 0 stream)
;;  (return-from write-stack-map-full-frame)
  (write-u2 (length (full-frame-locals frame)) stream)
  (dolist (local (full-frame-locals frame))
    (funcall (verification-type-info-writer local) local stream))
  (write-u2 (length (full-frame-stack-items frame)) stream)
  (dolist (stack-item (full-frame-stack-items frame))
    (funcall (verification-type-info-writer stack-item) stack-item stream)))

(defstruct verification-type-info tag (writer #'write-simple-verification-type-info))

(defstruct (top-variable-info (:include verification-type-info (tag 0))))
(defstruct (integer-variable-info (:include verification-type-info (tag 1))))
(defstruct (float-variable-info (:include verification-type-info (tag 2))))
(defstruct (double-variable-info (:include verification-type-info (tag 3))))
(defstruct (long-variable-info (:include verification-type-info (tag 4))))
(defstruct (null-variable-info (:include verification-type-info (tag 5))))
(defstruct (uninitialized-this-variable-info (:include verification-type-info (tag 6))))
(defstruct (object-variable-info
	     (:include verification-type-info
		       (tag 7) (writer #'write-object-variable-info)))
  constant-pool-index)
(defstruct (uninitialized-variable-info
	     (:include verification-type-info
		       (tag 8) (writer #'write-unitialized-variable-info)))
  offset)

(defun write-simple-verification-type-info (vti stream)
  (write-u1 (verification-type-info-tag vti) stream))
(defun write-object-variable-info (vti stream)
  (write-u1 (verification-type-info-tag vti) stream)
  (write-u2 (object-variable-info-constant-pool-index vti) stream))
(defun write-uninitialized-variable-info (vti stream)
  (write-u1 (verification-type-info-tag vti) stream)
  (write-u2 (uninitialized-variable-info-offset vti) stream))

(defun compute-initial-method-locals (method)
  (let (locals)
    (unless (member :static (method-access-flags method))
      (if (string= "<init>" (method-name method))
	  ;;the method is a constructor.
	  (push :uninitialized-this locals)
	  ;;the method is an instance method.
	  (push :this locals)))
    (dolist (x (cdr (method-descriptor method)))
      (push x locals))
    (nreverse locals)))

(defun smf-type->variable-info (type class)
  (cond
    ((eq type :this)
     (make-object-variable-info :constant-pool-index (class-file-class class)))
    ((eq type :int) (make-integer-variable-info))
    ((typep type 'constant-class)
     (make-object-variable-info :constant-pool-index (constant-index type)))
    ((typep type 'class-name)
     (make-object-variable-info
      :constant-pool-index
      (constant-index (pool-add-class (class-file-constants class) type))))
    (t (sys::%format t "Don't know how to translate type ~S~%" type) type)))

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

