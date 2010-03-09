;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; common.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(in-package :scpb)


;;; Common Functions

;; Paul Graham, On Lisp
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun quit ()
  (cl-user::quit))


(defun rl2str (reversed-list)
  (coerce (reverse reversed-list) 'string))


(let ((time-units (/ 1.0 internal-time-units-per-second)))
  (defun sbcl-uptime (&key (offset 0))
    (+ (* (get-internal-real-time) time-units)
       offset)))
