;;;; Hacks go here. If appropriate, they should be sent to their project maintainers
;;;; once tested. This file is loaded before any other sykobot files, including utils.lisp
;;;; Please document why the hack is necessary, and make the librar(y|ies) it affects obvious.

;; Good Medicine
;; Please explain why this is necessary - syko
(setf drakma:*drakma-default-external-format* :utf-8
      flexi-streams:*substitution-char* #\?)