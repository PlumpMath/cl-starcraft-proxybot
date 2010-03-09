;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; config.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(in-package :scpb)


;;; Main global struct that's passed around.

(defstruct sc-bot
  (id nil)
  (game-ended nil)
  (map-name "" :type string)
  (map-width 0 :type fixnum)
  (map-height 0 :type fixnum)
  (map nil)
  (players nil))

(defparameter *sc-bot* nil)


;;; Parameters accessed from different threads and their locks.

(defparameter *command-queue* nil)
(defparameter *command-queue-lock* (bt:make-lock "command-queue-lock"))

(defparameter *current-status* nil)
(defparameter *current-status-lock* (bt:make-lock "current-status-lock"))


;;; Socket Variables

(defvar *pb* nil)
(defvar *pb-socket* nil)


;;; Network Formats / Protocols

(defparameter *section-markers* '(#\newline #\: #\;))

(defparameter *new-game-message-format* '(:player-id -> parse-nr
                                          :players   -> parse-players))

(defparameter *players-format* '(:id   -> parse-nr
                                 :race -> parse-str
                                 :name -> parse-str
                                 :type -> parse-nr
                                 :ally -> parse-nr))

(defparameter *status-message-format* '(:minerals        -> parse-nr
                                        :gas             -> parse-nr
                                        :supply-used     -> parse-nr
                                        :supply-total    -> parse-nr
                                        :research-status -> parse-str
                                        :upgrade-status  -> parse-str
                                        :units           -> parse-units))

(defparameter *units-format* '(:id            -> parse-nr
                               :player-id     -> parse-nr
                               :type-id       -> parse-nr
                               :tile-x        -> parse-nr
                               :tile-y        -> parse-nr
                               :health        -> parse-nr
                               :shields       -> parse-nr
                               :energy        -> parse-nr
                               :build-time    -> parse-nr
                               :train-time    -> parse-nr
                               :research-time -> parse-nr
                               :upgrade-time  -> parse-nr
                               :order-timer   -> parse-nr
                               :order-id      -> parse-nr
                               :lifted        -> parse-bool
                               :resources     -> parse-nr
                               :addon-id      -> parse-nr
                               :mine-count    -> parse-nr))
