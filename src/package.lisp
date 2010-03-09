;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; package.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(in-package :cl-user)


(defpackage :cl-starcraft-proxybot
  (:nicknames :scpb)
  (:use :cl :parse-number :usocket))
