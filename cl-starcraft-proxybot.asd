;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cl-starcraft-proxybot.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(in-package :cl-user)


(asdf:defsystem :cl-starcraft-proxybot
  :version "0.1"
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "config")
                             (:file "common")
                             (:file "network")
                             (:file "commands")
                             (:file "cl-starcraft-proxybot"))))
  :depends-on (:bordeaux-threads :parse-number :usocket))
