(asdf:defsystem sykobot
  :version "0"
  :description "IRC bot for #sykosomatic"
  :licence "MIT"
  :depends-on (cl-irc
               cl-ppcre
               bordeaux-threads
               drakma
               html-entities
               sheeple
               alref)
  :serial t
  :components ((:file "sykobot")
               (:file "commands")
               (:file "config")
               (:file "chant")))
