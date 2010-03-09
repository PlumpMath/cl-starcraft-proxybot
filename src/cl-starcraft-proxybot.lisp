;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cl-starcraft-proxybot.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(in-package :scpb)


;;; BOT-THINK function.  This function should be overridden by your own
;;; #'bot-think.  Currently it does nothing except print a status update
;;; every second.

(defun bot-think ()
  (sleep 1)
  (format t (mkstr "[bot-think] This function does nothing and needs to be "
                   "overridden~%by your own.~%")))


;;; ON-FRAME function.  This keeps running until the game is ended and
;;; iterates every time there's a new status update from StarCraft.

(defun on-frame ()
  (setf (sc-bot-game-ended *sc-bot*) nil)
  (loop until (sc-bot-game-ended *sc-bot*)
        do (pb-wait-for-input)  ; see how it goes with 0 sleep-time
           (let* ((status (parse-status (pb-receive)))
                  (commands "commands"))
             (cond ((getf status :ended)
                    (format t "Game ended, the winner is: ~A~%."
                            (loop with winner = (getf status :ended)
                                  for p in (sc-bot-players *sc-bot*)
                                  thereis (when (= (getf p :id) winner)
                                            (mkstr (getf p :name) " ("
                                                   (getf p :race) ")"))))
                    (setf (sc-bot-game-ended *sc-bot*) t))
                   (status
                    (when (bt:acquire-lock *current-status-lock* t)
                      (setf *current-status* status)
                      (bt:release-lock *current-status-lock*))))
             (when (bt:acquire-lock *command-queue-lock* t)
               (loop for command in (reverse *command-queue*)
                     do (setf commands (mkstr commands ":" command)))
               (pb-send commands)
               (setf *command-queue* nil)
               (bt:release-lock *command-queue-lock*)))))


;;; Main Program

(defun pb-start (&key (host 0) (port 12345) (element-type 'character))
  (format t "~&Creating socket (host: ~A; port: ~A; element-type: ~A)...~%"
          host port element-type)
  (pb-socket-listen :host host :port port :element-type element-type)
  (format t "Waiting for connection from StarCraft...~%")
  (pb-socket-accept)
  (format t "Connected, waiting for new game info...~%")
  (pb-wait-for-input)
  (let ((game-info (parse-new-game (pb-receive)))
        locations map width height)
    (format t "  ~S~%" game-info)
    (pb-set-options)
    (format t "Waiting for locations... ")
    (force-output)
    (pb-wait-for-input)
    (setf locations (parse-locations (pb-receive)))
    (format t "~S~%" locations)
    (format t "Waiting for map... ")
    (force-output)
    (pb-wait-for-input)
    (setf map (parse-map (pb-receive)))
    (setf width (getf map :width)
          height (getf map :height))
    (format t "~A (~A x ~A)~%" (getf map :name) width height)
    (setf *sc-bot* (make-sc-bot :id (getf game-info :player-id)
                                :map-name (getf map :name)
                                :map-width width :map-height height
                                :map (map2array width height (getf map :map))
                                :players (getf game-info :players))))
  (format t "Starting on-frame thread...~%")
  (bt:make-thread #'on-frame :name "on-frame")
  (format t "Starting bot-think thread...~%")
  (bt:make-thread #'bot-think :name "bot-think")
  (bt:all-threads))
