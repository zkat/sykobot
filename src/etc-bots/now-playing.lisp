(in-package: sykobot)

(deflistener scan-for-now-playing (and (> (length *message*) 3)(STRING-equal (subseq *message* 0 3) "np:") (send-reply *bot* *channel* *sender* (random-elt (list "nice song man" "you know your classics" "wut an noise!" "in your pants" "tool is much better than that!" "that's nothing compared to britney")))))
