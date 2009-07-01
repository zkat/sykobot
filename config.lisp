(in-package :sykobot)

(defun config-exists-p ()
  (if (probe-file (merge-pathnames ".sykobotrc" (user-homedir-pathname)))
      t nil))

(defun run-sykobot ()
  (if (config-exists-p)
      (load (merge-pathnames ".sykobotrc" (user-homedir-pathname)))
      (run-bot #@sykobot "irc.freenode.net")))

