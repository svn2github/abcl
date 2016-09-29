;;; featurep.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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

;;; Adapted from SBCL.

(in-package #:extensions)

(export 'featurep)

(defun featurep (form)
  (if (atom form)
      (not (null (memq form *features*)))
      (case (car form)
        ((:not not)
         (if (cddr form)
             (error "Too many subexpressions in feature expression: ~S" form)
             (not (featurep (cadr form)))))
        ((:and and)
         (dolist (subform (cdr form) t)
           (unless (featurep subform) (return))))
        ((:or or)
         (dolist (subform (cdr form) nil)
           (when (featurep subform) (return t))))
        (t
         (error "Unknown operator in feature expression: ~S" form)))))

;;;; Cribbed from ASDF 3.1.7; duplicated to establish runtime conditionals before ASDF is constructed
(defun os-macosx-p ()
  "Is the underlying operating system MacOS X?"
  ;; OS-MACOSX is not mutually exclusive with OS-UNIX,
  ;; in fact the former implies the latter.
  (featurep '(:or :darwin (:and :allegro :macosx) (:and :clisp :macos))))

(defun os-unix-p ()
  "Is the underlying operating system some Unix variant?"
  (or (featurep '(:or :unix :cygwin)) (os-macosx-p)))

(defun os-windows-p ()
  "Is the underlying operating system Microsoft Windows?"
  (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32 :mingw64))))

(defun os-genera-p ()
  "Is the underlying operating system Genera (running on a Symbolics Lisp Machine)?"
  (featurep :genera))

(defun os-oldmac-p ()
  "Is the underlying operating system an (emulated?) MacOS 9 or earlier?"
  (featurep :mcl))

(defun os-haiku-p ()
  "Is the underlying operating system Haiku?"
  (featurep :haiku))

(export '(os-unix-p os-windows-p))

