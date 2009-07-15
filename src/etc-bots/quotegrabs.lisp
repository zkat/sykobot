;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;; Quotes

(defproto quotes-bot ((proto 'command-bot))
  ((quotes (make-hash-table :test #'equalp))
   (last-said (make-hash-table :test #'equalp))))

(defreply init-sheep :after ((bot (proto 'quotes-bot)) &key)
  (setf (quotes bot) (make-hash-table :test #'equalp))
  (setf (last-said bot) (make-hash-table :test #'equalp)))

(defreply init-bot :after ((bot (proto 'quotes-bot)))
	  (load-quotes bot))

(defreply join :after ((bot (proto 'quotes-bot)) channel)
  (setf (gethash channel (last-said bot))
	(make-hash-table :test #'equalp)))



;;; Commands
(defcommand grab ("(.*)" nick)
  "Syntax: 'grab <nick>' - Grabs the last thing someone said, and remembers it as a quote."
  (if (equalp nick *sender*)
      "*swat* No grabbing yourself in public. That's rude."
      (let ((to-grab (get-last-said-for-nick *bot* nick *channel*)))
        (if to-grab
            (progn
              (add-quote *bot* nick *sender* *channel* to-grab)
              "Tada!")
            "Nothing to grab"))))

(defcommand random-quote ("(.*)" nick)
  "Syntax: 'random-quote [<nick>]' - Returns a random quote. If <nick> is provided, it only~
 gets a random quote from that nick."
  (if (< 0 (length nick))
      (pretty-print-quote (get-random-quote *bot* nick))
      (pretty-print-quote
       (get-random-quote *bot*
			 (random-elt (hash-table-keys (quotes *bot*)))))))

(defcommand quote ("(.*)" nick)
  "Syntax: 'quote <nick>' - Returns the last thing grabbed from <nick>."
  (pretty-print-quote (get-last-quote *bot* nick)))

;;; utility
(defun quotes-db (bot)
  (merge-pathnames "quote-table.db" (bot-dir bot)))

(defun make-quote (speaker grabber channel text)
  (list speaker grabber channel text (get-universal-time)))

;;; messages
(defmessage save-quotes (bot))
(defmessage load-quotes (bot))
(defmessage add-quote (bot speaker grabber channel text))
(defmessage get-quotes (bot nick))

(defreply load-quotes ((bot (proto 'quotes-bot)))
  (when (probe-file (quotes-db bot))
    (setf (quotes bot)
          (cl-store:restore (quotes-db bot)))))

(defreply save-quotes ((bot (proto 'quotes-bot)))
  (cl-store:store (quotes bot) (quotes-db bot)))

(defreply add-quote ((bot (proto 'quotes-bot)) speaker grabber channel text)
  (push (make-quote speaker grabber channel text)
        (gethash speaker (quotes bot))))
(defreply add-quote :after ((bot (proto 'quotes-bot)) speaker grabber channel text)
          (declare (ignore speaker grabber channel text))
          (save-quotes bot))

(defreply get-quotes ((bot (proto 'quotes-bot)) nick)
  (gethash nick (quotes bot)))

;;;
(defun get-random-quote (bot nick)
  (random-elt (get-quotes bot nick)))

(defun get-last-quote (bot nick)
  (car (get-quotes bot nick)))

(defun pretty-print-quote (quote)
  (if quote
      (destructuring-bind (speaker grabber channel text time-grabbed) quote
        (declare (ignore grabber channel time-grabbed))
        (build-string "~A: ~A" speaker text))
      "That person evidently never said anything worthy of note"))

;;; Listener for grabbing
(defun update-last-said-for-nick (bot nick channel text)
  (when (gethash channel (last-said bot))
    (setf (gethash nick (gethash channel (last-said bot))) text)))

(defun get-last-said-for-nick (bot nick channel)
  (gethash nick (gethash channel (last-said bot))))

(deflistener remember-last-thing-said
  (update-last-said-for-nick *bot* *sender* *channel* *message*))
