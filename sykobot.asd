(asdf:defsystem sykobot
  :version "0"
  :description "IRC bot for #sykosomatic"
  :licence "MIT"
  :depends-on (cl-irc cl-ppcre drakma html-entities sheeple)
  :serial t
  :components ((:file "sykobot")
               (:file "commands")
               (:file "config")))


