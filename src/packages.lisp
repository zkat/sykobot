(defpackage #:sykobot
  (:use :cl :cl-ppcre :sheeple)
  (:export :sykobot :run-bot :connect :disconnect :join :part
           :identify :nick :send-notice :send-msg :topic :add-command
           :remove-command :connection :nickname :server :password
           :*default-channels* :*server* :*port* :*identify-with-nickserv?*
           :*nickserv-password* :*nickname* :*cmd-prefix* :*default-listeners*
           :*username* :*realname* :*bot-dir* :command-listener :send-memos
           :scan-for-fact :scan-for-more :scan-for-url :remember-last-thing-said
	   :*default-listeners-by-channel* :alref :*default-timestamp-function*
	   :scan-for-now-playing))

(defpackage #:sykobot-user
  (:use :cl :sykobot :sheeple :cl-ppcre))
