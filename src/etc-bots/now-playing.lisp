(in-package :sykobot)

(deflistener scan-for-now-playing
  (when (and (> (length *message*) 3)
             (STRING-equal (subseq *message* 0 3) "np:"))
    (send-reply *bot* *channel* *sender*
                (random-elt '("Nice song, man!"
                              "You know your classics!"
                              "What a noise!"
                              "In your pants."
                              "Tool is much better than that!"
                              "That's nothing compared to Britney.")))))
