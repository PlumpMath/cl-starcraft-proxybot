;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; network.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in this project's directory for more info.

(in-package :scpb)


;;; Predicates

(defun endedp (string)
  (when (>= (length string) 6)
    (string= (subseq string 0 6) "ended:")))


(defun locationsp (string)
  (equal (subseq string 0 10) "Locations:"))


(defun new-game-p (string)
  (equal (subseq string 0 8) "NewGame;"))


(defun statusp (string)
  (when (>= (length string) 2)
    (string= (subseq string 0 2) "s;")))


;;; Parsers

(defun map2array (width height map)
  (loop with array = (make-array (list width height))
        for y from 0 below height
        do (loop for x from 0 below width
                 for index = (* x y 3)  ; 3 elements per map position
                 for tile-height = (elt map index)
                 for tile-buildable = (elt map (+ index 1))
                 for tile-walkable = (elt map (+ index 2))
                 do (setf (aref array x y)
                          (list tile-height tile-buildable tile-walkable)))
        finally (return array)))


(defun parse-bool (status-string)
  (loop for c across status-string
        for i from 1
        if (member c *section-markers*)
          do (loop-finish)
          else collect c into nr
        finally (return (list (cond ((char= (first nr) #\0) nil)
                                    ((char= (first nr) #\1) t)
                                    (t (error (mkstr "[parse-bool] Unknown "
                                                     "boolean: ~S") nr)))
                              (if (>= i (length status-string))
                                  nil
                                  (subseq status-string i))))))


(defun parse-nr (status-string)
  (loop for c across status-string
        for i from 1
        if (member c *section-markers*)
          do (loop-finish)
          else collect c into nr
        finally (return (list (parse-number (coerce nr 'string))
                              (if (>= i (length status-string))
                                  nil
                                  (subseq status-string i))))))


(defun parse-str (status-string)
  (loop for c across status-string
        for i from 1
        if (member c *section-markers*)
          do (loop-finish)
          else collect c into nr
        finally (return (list (coerce nr 'string)
                              (if (>= i (length status-string))
                                  nil
                                  (subseq status-string i))))))


(defun parse-locations (string)
  (unless (locationsp string)
    (format t "[parse-locations] Not a Locations string: ~S.~%" string)
    (return-from parse-locations))
  (loop with locations = nil  with x = nil  with y = nil  with section = :x
        for c across (subseq string 10)
        do (cond ((or (char= c #\:) (char= c #\newline) (char= c #\return))
                  (push (list (parse-number (rl2str x))
                              (parse-number (rl2str y)))
                        locations)
                  (setf x nil  y nil  section :x))
                 ((char= c #\;) (setf section :y))
                 ((equal section :x) (push c x))
                 ((equal section :y) (push c y))
                 (t (error (mkstr "[parse-locations] Unknown combination: "
                                    "c=~S and section=~S") c section)))
        finally (return (reverse locations))))


(defun parse-map (string)
  (loop with name = nil
        with width = nil
        with height = nil
        with map = nil
        with section = :name
        for c across string
        do (cond ((and (char= c #\:) (equal section :name))
                  (setf section :width))
                 ((and (char= c #\:) (equal section :width))
                  (setf section :height))
                 ((and (char= c #\:) (equal section :height))
                  (setf section :map))
                 ((equal section :name) (push c name))
                 ((equal section :width) (push c width))
                 ((equal section :height) (push c height))
                 ((equal section :map) (push c map))
                 (t (error (mkstr "[parse-map] Unknown combination: c=~S and "
                                  "section=~S") c section)))
          finally (return (list :name (rl2str name)
                                :width (parse-number (rl2str width))
                                :height (parse-number (rl2str height))
                                :map (rl2str map)))))


(defun parse-new-game (string)
  (when (not (new-game-p string))
    (format t "[parse-new-game] Not a NewGame string: ~S.~%" string)
    (return-from parse-new-game))
  (loop with result = nil
        with fmt = (copy-seq *new-game-message-format*)
        with info = (subseq string 8)
        while fmt
        while info
        for parse-result = (funcall (third fmt) info)
        do (push (first fmt) result)
           (push (first parse-result) result)
           (setf info (second parse-result)
                 fmt (cdddr fmt))
        finally (return (reverse result))))


(defun parse-players (players-string)
    (loop with result = nil
          with fmt = (copy-seq *players-format*)
          with player = nil
          while fmt
          while players-string
          for parse-result = (funcall (third fmt) players-string)
          do (push (first fmt) player)
             (push (first parse-result) player)
             (setf players-string (second parse-result)
                   fmt (cdddr fmt))
             (when (null fmt)
               (push (reverse player) result)
               (setf fmt (copy-seq *players-format*)
                     player nil))
          finally (return (list (reverse result) nil))))


(defun parse-status (string)
  (cond ((endedp string)
         (return-from parse-status
                (list :ended
                      (parse-number (subseq string 6 (- (length string) 1))))))
        ((not (statusp string))
         (format t "[parse-status] Not a status string: ~S.~%" string)
         (return-from parse-status)))
  (loop with result = nil
        with fmt = (copy-seq *status-message-format*)
        with status = (subseq string 2)
        while fmt
        while status
        for parse-result = (funcall (third fmt) status)
        do (push (first fmt) result)
           (push (first parse-result) result)
           (setf status (second parse-result)
                 fmt (cdddr fmt))
        finally (return (reverse result))))


(defun parse-units (units-string)
  (loop with result = nil
        with fmt = (copy-seq *units-format*)
        with unit = nil
        while units-string
        for parse-result = (funcall (third fmt) units-string)
        do (push (first fmt) unit)
           (push (first parse-result) unit)
           (setf units-string (second parse-result)
                 fmt (cdddr fmt))
           (when (null fmt)
             (push (reverse unit) result)
             (setf fmt (copy-seq *units-format*)
                   unit nil))
        finally (return (list (reverse result) nil))))


;;; Socket Functions

(defun pb-listen ()
  (listen (socket-stream *pb*)))


;; XXX: Is this still the case?:
;; My parsers depend on the newline being at the end of the received
;; responses hence no "until (char= c #\newline)".
(defun pb-receive ()
  (loop with newline = nil
        until newline
        for c = (read-char (socket-stream *pb*))
        when (char= c #\newline) do (setf newline t)
        collect c into result
        finally (return (coerce result 'string))))


(defun pb-send (command)
  (princ command (socket-stream *pb*))
  (force-output (socket-stream *pb*)))


(defun pb-set-options (&key (user-input nil) (complete-map-info nil)
                            (log-commands t) (terrain-analysis nil))
  (let ((ui (if user-input "1" "0"))
        (cmi (if complete-map-info "1" "0"))
        (lc (if log-commands "1" "0"))
        (ta (if terrain-analysis "1" "0")))
    (format t (mkstr "Setting options to: user-input: ~A, "
                     "complete-map-info: ~A,~%"
                     "                    log-commands: ~A, "
                     "terrain-analysis: ~A...~%")
            user-input complete-map-info log-commands terrain-analysis)
    (pb-send (mkstr ui cmi lc ta))))


(defun pb-socket-accept ()
  (when *pb-socket*
    (setf *pb* (socket-accept *pb-socket*))))


(defun pb-socket-listen (&key (host 0) (port 12345) (element-type 'character))
  (unless *pb-socket*
    (setf *pb-socket* (socket-listen host port :element-type element-type))))


;; XXX: Figure out whether calling (sleep 0) is different from not calling
;; XXX: it wrt threads yielding, etc.
(defun pb-wait-for-input (&optional (sleep-time 0))
  (loop until (pb-listen)
        do (sleep sleep-time)))
