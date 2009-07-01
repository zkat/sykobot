(in-package :sykobot)

(let ((command-table (make-hash-table :test #'equalp)))
  (defun add-command (cmd fn)
    (setf (gethash cmd command-table) fn))

  (defun remove-command (cmd)
    (remhash cmd command-table))

  (defun command-function (cmd)
    (multiple-value-bind (fn hasp)
        (gethash cmd command-table)
      (if hasp fn
          (lambda (bot args sender channel)
            (declare (ignore channel args))
            (send-notice bot sender (format nil "I don't know how to ~A." cmd))))))

  (defun erase-all-commands ()
    (clrhash command-table))
  )

(add-command "echo" (lambda (bot args sender channel)
                     (send-msg bot channel args)))
(add-command "ping" (lambda (bot args sender channel)
                     (send-msg bot channel "pong")))
(add-command "google" (lambda (bot args sender channel)
                        (multiple-value-bind (title url)
                            (google-search args)
                          (send-msg bot channel
                                    (format nil "~A: ~A <~A>"
                                            sender title url)))))
(add-command "shut" (lambda (bot args sender channel)
                      (send-msg bot channel
                                (format nil "~A: Fine. Be that way. Tell me to talk when you realize ~
                                                 just how lonely and pathetic you really are." sender))
                      (shut-up bot)))
(add-command "chant" (lambda (bot args sender channel)
                      (send-msg bot channel "FUCK REGEX")))
(add-command "help" (lambda (bot args sender channel)
                      (send-msg bot channel (format nil "~A: I'm not a psychiatrist. Go away." sender))))
#+nil(add-command "tell" (lambda (bot args sender channel)
                      (send-msg bot channel)))

(add-command "cliki" (lambda (bot args sender channel)
                       (send-msg bot channel
                                 (format nil "~A, I found ~D result~:P. Check it out at <~A> or leave me alone."
                                         sender (nth-value 1 (cliki-urls args))
                                         (search-url "http://www.cliki.net/admin/search?words=~A" args)))))

(defun search-url (engine query)
  (format nil engine (regex-replace-all "\\s+" query "+")))

(defun google-search (query)
  (url-info (search-url
             "http://google.com/search?filter=1&safe=on&q=~A&btnI"
             query)))

(defun url-info (url)
  (handler-case
      (multiple-value-bind (body status-code headers uri)
          (drakma:http-request url)
        (declare (ignore status-code headers))
        (values (multiple-value-bind (match vec)
                    (scan-to-strings
                     (create-scanner
                      "<title\\s*>\\s*(.+)\\s*</title\\s*>"
                      :case-insensitive-mode t) body)
                  (declare (ignore match))
                  (if (< 0 (length vec))
                      (decode-html-string (elt vec 0))
                      nil))
                (with-output-to-string (s)
                  (puri:render-uri uri s))))
    (usocket:ns-host-not-found-error () (error "Host not found"))))

(defun cliki-urls (query)
  (let ((links NIL)
        (page (drakma:http-request
               (search-url "http://www.cliki.net/admin/search?words=~A"
                           query))))
    (ppcre:do-register-groups (url)
        ("\\d <b><a href=\"(.*?)\">(.*?)<" page)
      (push url links))
    (values links
            (parse-integer (ppcre:scan-to-strings "(\\d*) results? found" page)
                           :junk-allowed T))))


(defun decode-html-string (string)
  (html-entities:decode-entities string))

(defun has-url-p (string)
  (when (scan "https?://.*[.$| |>]" string) t))

(defun grab-url (string)
  (find-if #'has-url-p (split "[\\s+><,]" string)))

