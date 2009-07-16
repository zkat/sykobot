;;;; Copyright 2009 Kat Marchan
;;;;;;;
;;;;;;; This file is part of sykobot.
;;;;;;;
;;;;;;; For licensing and warranty information, refer to COPYING
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(deflistener scan-for-now-playing
  (when (and (> (length *message*) 3)
             (STRING-equal (subseq *message* 0 3) "np:"))
    (send-reply *bot* *channel* *sender*
                (random-elt '("Nice song, man!"
                              "You know your classics!"
                              "You call that music ?"
			      "mind the guitar solo !"
                              "In your pants."
                              "Tool is much better than that!"
                              "That's nothing compared to Britney.")))))
