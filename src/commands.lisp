;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; commands.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(in-package :scpb)


;;; Main function to add commands to the queue.

(defun add-command (command &optional (lock t))
  (when lock (bt:acquire-lock *command-queue-lock* t))
  (push command *command-queue*)
  (when lock (bt:release-lock *command-queue-lock*)))


;;; Command Functions
;;;
;;; See: o ProxyBot-2.6.1/starcraftbot/proxybot/command/Command.java
;;;      o ProxyBot-2.6.1/starcraftbot/proxybot/command/CommandQueue.java

(defun attack-move (unit-id x y)
  (add-command (mkstr 1 ";" unit-id ";" x ";" y ";0")))


(defun attack-unit (unit-id target-id)
  (add-command (mkstr 2 ";" unit-id ";" target-id ";0;0")))


(defun right-click (unit-id x y)
  (add-command (mkstr 3 ";" unit-id ";" x ";" y ";0")))


(defun right-click-unit (unit-id target-id)
  (add-command (mkstr 4 ";" unit-id ";" target-id ";0;0")))


(defun train (unit-id type-id)
  (add-command (mkstr 5 ";" unit-id ";" type-id ";0;0")))


(defun build (unit-id tile-x tile-y type-id)
  (add-command (mkstr 6 ";" unit-id ";" tile-x ";" tile-y ";" type-id)))


(defun build-addon (unit-id type-id)
  (add-command (mkstr 7 ";" unit-id ";" type-id ";0;0")))


(defun research (unit-id tech-type-id)
  (add-command (mkstr 8 ";" unit-id ";" tech-type-id ";0;0")))


(defun upgrade (unit-id upgrade-type-id)
  (add-command (mkstr 9 ";" unit-id ";" upgrade-type-id ";0;0")))


(defun stop (unit-id)
  (add-command (mkstr 10 ";" unit-id ";0;0;0")))


(defun hold-position (unit-id)
  (add-command (mkstr 11 ";" unit-id ";0;0;0")))


(defun patrol (unit-id x y)
  (add-command (mkstr 12 ";" unit-id ";" x ";" y ";0")))


(defun follow (unit-id target-id)
  (add-command (mkstr 13 ";" unit-id ";" target-id ";0;0")))


(defun set-rally-position (unit-id x y)
  (add-command (mkstr 14 ";" unit-id ";" x ";" y ";0")))


(defun set-rally-unit (unit-id target-id)
  (add-command (mkstr 15 ";" unit-id ";" target-id ";0;0")))


(defun repair (unit-id target-id)
  (add-command (mkstr 16 ";" unit-id ";" target-id ";0;0")))


(defun morph (unit-id type-id)
  (add-command (mkstr 17 ";" unit-id ";" type-id ";0;0")))


(defun burrow (unit-id)
  (add-command (mkstr 18 ";" unit-id ";0;0;0")))


(defun unburrow (unit-id)
  (add-command (mkstr 19 ";" unit-id ";0;0;0")))


(defun siege (unit-id)
  (add-command (mkstr 20 ";" unit-id ";0;0;0")))


(defun unsiege (unit-id)
  (add-command (mkstr 21 ";" unit-id ";0;0;0")))


(defun cloak (unit-id)
  (add-command (mkstr 22 ";" unit-id ";0;0;0")))


(defun decloak (unit-id)
  (add-command (mkstr 23 ";" unit-id ";0;0;0")))


(defun lift (unit-id)
  (add-command (mkstr 24 ";" unit-id ";0;0;0")))


(defun land (unit-id tile-x tile-y)
  (add-command (mkstr 25 ";" unit-id ";" tile-x ";" tile-y ";0")))


(defun load-unit (unit-id target-id)
  (add-command (mkstr 26 ";" unit-id ";" target-id ";0;0")))


(defun unload (unit-id target-id)
  (add-command (mkstr 27 ";" unit-id ";" target-id ";0;0")))


(defun unload-all (unit-id)
  (add-command (mkstr 28 ";" unit-id ";0;0;0")))


(defun unload-all-position (unit-id x y)
  (add-command (mkstr 29 ";" unit-id ";" x ";" y ";0")))


(defun cancel-construction (unit-id)
  (add-command (mkstr 30 ";" unit-id ";0;0;0")))


(defun halt-construction (unit-id)
  (add-command (mkstr 31 ";" unit-id ";0;0;0")))


(defun cancel-morph (unit-id)
  (add-command (mkstr 32 ";" unit-id ";0;0;0")))


(defun cancel-train (unit-id)
  (add-command (mkstr 33 ";" unit-id ";0;0;0")))


(defun cancel-train-slot (unit-id slot)
  (add-command (mkstr 34 ";" unit-id ";" slot ";0;0")))


(defun cancel-addon (unit-id)
  (add-command (mkstr 35 ";" unit-id ";0;0;0")))


(defun cancel-research (unit-id)
  (add-command (mkstr 36 ";" unit-id ";0;0;0")))


(defun cancel-upgrade (unit-id)
  (add-command (mkstr 37 ";" unit-id ";0;0;0")))


(defun use-tech (unit-id tech-type-id)
  (add-command (mkstr 38 ";" unit-id ";" tech-type-id ";0;0")))


(defun use-tech-position (unit-id tech-type-id x y)
  (add-command (mkstr 39 ";" unit-id ";" tech-type-id ";" x ";" y)))


(defun use-tech-target (unit-id tech-type-id target-id)
  (add-command (mkstr 40 ";" unit-id ";" tech-type-id ";" target-id ";0")))


(defun game-speed (speed)
  (add-command (mkstr 41 ";" speed ";0;0;0")))


(defun screen-position (x y)
  (add-command (mkstr 42 ";" x ";" y ";0;0")))


(defun line-map (x1 y1 x2 y2)
  (add-command (mkstr 43 ";" x1 ";" y1 ";" x2 ";" y2)))


(defun line-screen (x1 y1 x2 y2)
  (add-command (mkstr 44 ";" x1 ";" y1 ";" x2 ";" y2)))


(defun circle-map (x y radius &optional (filled nil))
  (add-command (mkstr 45 ";" x ";" y ";" radius ";" (if filled "1" "0"))))


(defun circle-screen (x y radius &optional (filled nil))
  (add-command (mkstr 46 ";" x ";" y ";" radius ";" (if filled "1" "0"))))


(defun rect-map (left top right bottom)
  (add-command (mkstr 47 ";" left ";" top ";" right ";" bottom)))


(defun rect-screen (left top right bottom)
  (add-command (mkstr 48 ";" left ";" top ";" right ";" bottom)))


(defun box-map (left top right bottom)
  (add-command (mkstr 49 ";" left ";" top ";" right ";" bottom)))


(defun box-screen (left top right bottom)
  (add-command (mkstr 50 ";" left ";" top ";" right ";" bottom)))


(defun color (color)
  (add-command (mkstr 51 ";" (case color
                               (:red    0)  (:green 1)  (:blue    2)
                               (:yellow 3)  (:cyan  4)  (:magenta 5)
                               (:orange 6)  (:black 7)  (:white   8) (:gray 9)
                               (otherwise
                                (format t (mkstr "[color] Unknown color: ~S. "
                                                 "Defaulting to red.") color)
                                0))
                      ";0;0;0")))


(defun leave-game ()
  (add-command (mkstr 52 ";0;0;0;0")))


(defun say-hello ()
  (add-command (mkstr 53 ";0;0;0;0")))


(defun say-gg ()
  (add-command (mkstr 54 ";0;0;0;0")))
