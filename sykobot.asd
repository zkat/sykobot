(asdf:defsystem sykobot
  :version "0"
  :description "IRC bot for #sykosomatic"
  :licence "AGPL"
  :depends-on (cl-irc
               cl-ppcre
               bordeaux-threads
               drakma
               html-entities
               sheeple
               cl-store
               cl-json)
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "alref")
	     (:file "packages")
             (:module "utils"
                      :serial t
                      :components
                      ((:file "hacks")
                       (:file "strings")
                       (:file "utils")
                       (:file "time-utils")))
             (:module "core-bots"
                      :serial t
                      :components
                      ((:file "sykobot")
                       (:file "listeners")
                       (:file "commands")))
             (:module "etc-bots"
                      :serial t
                      :components
                      ((:file "memos")
                       (:file "facts")
		       (:file "eliza")
		       (:file "chant")
		       (:file "now-playing")
		       (:file "karma")
		       (:file "seen")
		       (:file "interpreter")
		       (:file "spy")
                       (:file "quotegrabs")))
             (:file "config")))))

(asdf:defsystem sykobot-tests
  :version "0"
  :description "Unit tests for sykobot"
  :licence "AGPL"
  :depends-on (:sykobot :fiveam)
  :serial t
  :components
  ((:module "tests"
            :serial t
            :components
            ((:file "packages")
	     (:file "suite")
	     (:module "utils"
                      :serial t
                      :components
                      ((:file "time-utils")))
	     (:module "core-bots"
		      :serial t
		      :components
		      ((:file "sykobot")))))))


(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :sykobot))))
  (declare (ignore o c))
  (format t "~&~%*******************~%~
                 ** Loading tests **~%~
                 *******************~%")
  (asdf:oos 'asdf:load-op 'sykobot-tests)
  (asdf:oos 'asdf:test-op 'sykobot-tests))
