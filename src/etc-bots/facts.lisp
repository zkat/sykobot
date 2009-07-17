;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defproto facts-bot ((proto 'command-bot))
  ((facts (make-hash-table :test #'equalp))))

(defreply init-sheep :after ((bot (proto 'facts-bot)) &key)
  (setf (facts bot) (make-hash-table :test #'equalp)))

(defreply init-bot :after ((bot (proto 'facts-bot)))
  (load-facts bot))

;;; Limiting
;;; With 110 facts the size is 4467 bytes.
;;; Assuming linear growth (is this valid?) that's 40 bytes per fact.
;;; the average human knows 200k words (apparently)
;;; so limit this to 100k facts = 3.8mb ~
(defvar *max-facts-to-store* 100000)
(defvar *facts-to-remove* 1000)
(defvar *facts-write-interval* 5)
;;;;;;;;;;;;;;

;;; Facts
(defcommand fact ("(\\S+)*" topic)
  "Syntax: 'fact <topic>' - Returns a known fact concerning <topic>."
  (get-fact *bot* topic))

(defcommand random-fact ()
  "Syntax 'random-fact' - Returns a random fact from the bot's knowledge base."
  (get-fact *bot*
	    (random-elt (hash-table-keys (facts *bot*)))))

(defun facts-db (bot)
  (merge-pathnames "fact-table.db" (bot-dir bot)))

(defmessage load-facts (bot))
(defmessage save-facts (bot))
(defmessage set-fact (bot noun info))
(defmessage get-fact (bot noun))
(defmessage has-fact (bot noun))
(defmessage erase-some-facts (bot))
(defmessage erase-all-facts (bot))

(defreply load-facts ((bot (proto 'facts-bot)))
  (when (probe-file (facts-db bot))
    (setf (facts bot)
          (cl-store:restore (facts-db bot)))))

(defreply save-facts ((bot (proto 'facts-bot)))
  (when (zerop (mod (hash-table-count (facts bot)) *facts-write-interval*))
    (cl-store:store (facts bot) (facts-db bot))))

(defreply set-fact ((bot (proto 'facts-bot)) noun info)
  (when (> (hash-table-count (facts bot)) *max-facts-to-store*)
    (erase-some-facts bot))
  (setf (gethash noun (facts bot)) info))

(defreply set-fact :after ((bot (proto 'facts-bot)) noun info)
  (declare (ignore noun info))
  (save-facts bot))

(defreply get-fact ((bot (proto 'facts-bot)) noun)
  (multiple-value-bind (info hasp)
      (gethash noun (facts bot))
    (if hasp
        info
        (build-string "I know nothing about ~A" noun))))

(defreply has-fact ((bot (proto 'facts-bot)) noun)
  (multiple-value-bind (info hasp)
      (gethash noun (facts bot))
    (declare (ignore info))
    hasp))

(defreply erase-some-facts ((bot (proto 'facts-bot)))
  (let ((keys (hash-table-keys (facts bot))))
    (dotimes (n *facts-to-remove*) ; remove approximately *facts-to-remove* facts
      (remhash (random-elt keys) (facts bot)))))


(defreply erase-all-facts ((bot (proto 'facts-bot)))
  (clrhash (facts bot)))
(defreply erase-all-facts :after ((bot (proto 'facts-bot)))
  (save-facts bot))

(defun split-into-sub-statements (statement)
  (split "\\s*(,|but|however|whereas|although|\\; |\\. )\\s*" statement))

(deflistener scan-for-fact
  (let* ((articles '("a " "an " "the " "this " "that "))
         (verbs '(" am" " is" " are" " isn\\'t" " ain\\'t" "\\'s"
                  " likes" " uses" " has" " fails" " wins" " can" " can't"))
         (regex (build-string ".*?(~{~A~^|~})*(\\w+)(~{~A~^|~})\\s+(.+)" articles verbs)))

    (loop for statement in (split-into-sub-statements *message*)
       do (do-register-groups (article noun verb info)
              (regex statement)
            (if article
                (set-fact *bot* noun (build-string "~A ~A~A ~A" article noun verb info))
                (set-fact *bot* noun (build-string "~A~A ~A" noun verb info)))))))



