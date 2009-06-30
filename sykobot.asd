(asdf:defsystem sykobot
  :version "0"
  :description "IRC bot for #sykosomatic"
  :licence "MIT"
  :depends-on (cl-irc cl-ppcre bordeaux-threads drakma)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "sykobot")))

