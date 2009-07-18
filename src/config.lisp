;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defvar *default-channels* nil)
(defvar *server* nil)
(defvar *port* nil)
(defvar *identify-with-nickserv?* nil)
(defvar *nickserv-password* nil)
(defvar *nickname* nil)
(defvar *username* nil)
(defvar *realname* nil)
(defvar *bot-dir* nil)
(defvar *default-listeners* '(command-listener scan-for-url remember-last-thing-said
			      send-memos scan-for-fact scan-for-more scan-for-now-playing seen-listener interpreter-listener))
(defvar *default-listeners-by-channel* nil)
(defvar *default-timestamp-function* #'get-kilosecond-timestamp)
(defvar *cmd-prefix* nil)


(defvar *home* (merge-pathnames ".sykobot/" (user-homedir-pathname)))

(defmessage bot-dir (bot))
(defreply bot-dir ((bot (proto 'sykobot)))
  (ensure-directories-exist (merge-pathnames (dir bot) *home*)))

(defun run-bot (&optional (bot-prototype (clone (proto 'quotes-bot) 
						(proto 'memos-bot)
						(proto 'eliza-bot)
						(proto 'facts-bot)
						(proto 'karma-bot)
						(proto 'seen-bot)
						(proto 'interpreter-bot)
						(proto 'command-bot))))
  ;; Force unicode output. TODO: Support more implementations.
  ;; Prevents errors attempting to output unicode in non-unicode locales.
  #+(and sbcl #.(cl:if (cl:find-package "SWANK") '(or) '(and)))
  (progn (setf sb-impl::*default-external-format* :UTF-8)
         (setf sb-alien::*default-c-string-external-format* :UTF-8)
         (sb-kernel:stream-cold-init-or-reset))
  (let ((bot (clone bot-prototype)))
    (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                           (let ((r (find-restart 'continue c)))
                                             (when r (invoke-restart r))))))
      (when *bot-dir*
        (setf (dir bot) *bot-dir*))
      (let ((init-file (merge-pathnames "init" (bot-dir bot))))
        (when (probe-file init-file)
          (handler-case (load init-file)
            (end-of-file () (error "You missed a paren somewhere")))))
      (when *nickname*
        (setf (nickname bot) *nickname*))
      (when *username*
        (setf (username bot) *username*))
      (when *realname*
        (setf (realname bot) *realname*))
      (when *server*
        (setf (server bot) *server*))
      (when *port*
        (setf (port bot) *port*))
      (when *cmd-prefix*
	(setf (command-prefix bot) *cmd-prefix*))
      (init-bot bot)
      (when *identify-with-nickserv?*
        (identify bot *nickserv-password*))
      (dolist (channel *default-channels* bot)
        (join bot channel)))))
