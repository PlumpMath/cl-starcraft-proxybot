;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; fatalist.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(asdf:oos 'asdf:load-op :cl-starcraft-proxybot)
(in-package :scpb)


;;; Override the default #'bot-think.
;;;
;;; This is a really simple (and stupid) example.  It says hi on the SC
;;; console after 5 seconds, 3 seconds later it sends your minions mining,
;;; then it congratulates you after 20 seconds and leaves the game.

(defun bot-think ()
  (loop until *current-status*)  ; Wait for the status updates to start.
  (loop with hello = nil
        with gg = nil
        with leave = nil
        with start-time = (get-universal-time)
        until (sc-bot-game-ended *sc-bot*)
        do (sleep 1)
           (when (and (>= (- (get-universal-time) start-time) 5) (not hello))
             (say-hello)
             (setf hello t))
           (when (and (>= (- (get-universal-time) start-time) 8)
                      (bt:acquire-lock *current-status-lock*))
             (let ((status (copy-seq *current-status*)))
               (bt:release-lock *current-status-lock*)
               (loop for unit in (getf status :units)
                     for mineral in (loop for m in (getf status :units)
                                          when (= (getf m :type-id) 176)
                                            collect m)
                     for mid = (getf mineral :id)
                     for uid = (getf unit :id)
                     do (when (= (getf unit :order-id) 3)
                          (right-click-unit uid mid)))))
           (when (and (>= (- (get-universal-time) start-time) 20) (not gg))
             (say-gg)
             (setf gg t))
           (when (and (>= (- (get-universal-time) start-time) 24) (not leave))
             (leave-game)
             (setf leave t))))
