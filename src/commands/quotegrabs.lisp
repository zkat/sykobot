(in-package :sykobot)

;;; Quotes

;;; Commands
(defcommand grab ("(.*)" nick)
  (let ((to-grab (get-last-said-for-nick nick)))
    (when to-grab
      (add-quote *bot* nick *sender* *channel* to-grab)
      (cmd-msg "Tada!"))))


(defcommand rq ("(.*)" nick)
  (cmd-msg "~A,  ~A" 
	   *sender*
	   (pretty-print-quote (get-random-quote *bot* nick))))
	   

(defcommand q ("(.*)" nick)
  (cmd-msg "getting last quote for ~A (not really)" nick))

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

(defreply load-quotes ((bot (proto 'sykobot)))
  (when (probe-file (quotes-db bot))
    (setf (quotes bot)
	  (cl-store:restore (quotes-db bot)))))

(defreply save-quotes ((bot (proto 'sykobot)))
  (cl-store:store (quotes bot) (quotes-db bot)))

(defreply add-quote ((bot (proto 'sykobot)) speaker grabber channel text)
  (push (make-quote speaker grabber channel text)
	(gethash speaker (quotes bot))))
(defreply add-quote :after ((bot (proto 'sykobot)) speaker grabber channel text)
	  (declare (ignore speaker grabber channel text))
	  (save-quotes bot))

(defreply get-quotes ((bot (proto 'sykobot)) nick)
  (gethash nick (quotes bot)))

;;;

(defun get-random-quote (bot nick)
  (random-elt (get-quotes bot nick)))

(defun pretty-print-quote (quote)
  (destructuring-bind (speaker grabber channel text time-grabbed) quote
    (declare (ignore grabber channel time-grabbed))
    (format nil "~A: ~A" speaker text)))

;;; Listener for grabbing

(let ((last-said-table (make-hash-table :test #'equalp)))
  (defun update-last-said-for-nick (nick text)
    (setf (gethash nick last-said-table) text))
  (defun get-last-said-for-nick (nick)
    (gethash nick last-said-table)))

(deflistener remember-last-thing-said
  (update-last-said-for-nick *sender* *message*))


;;; Notes
;; (member element list :test #'string-equal) <-- case-insensitive string comparison.

  