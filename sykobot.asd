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
               cl-store)
  :serial t
  :components ((:file "sykobot")
               (:file "alref")
               (:file "listeners")
               (:file "commands")
               (:file "config")
               (:file "chant")))
