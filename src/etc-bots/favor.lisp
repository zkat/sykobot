;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defproto =favor-bot= (=command-bot=)
  ((favors nil)
   (users nil)))

(defreply init-object :after ((bot =favor-bot=) &key)
  (setf (favors bot) nil
        (users bot) nil))

(defreply init-bot :after ((bot =favor-bot=))
  (load-favors bot)
  (load-users bot))

(defvar *reputation-transactions*)

;;;
;;; Persistence
;;;
(defun user-db (bot)
  (merge-pathnames "users.db" (bot-dir bot)))
(defun favor-db (bot)
  (merge-pathnames "favors.db" (bot-dir bot)))

(defmessage load-favors (bot)
  (:reply ((bot =sykobot=))
    (when (probe-file (favor-db bot))
      (setf (favors bot) (cl-store:restore (favor-db bot))))))

(defmessage save-favors (bot)
  (:reply ((bot =sykobot=))
    (cl-store:store (favors bot) (favor-db bot))))

(defmessage load-users (bot)
  (:reply ((bot =sykobot=))
    (when (probe-file (user-db bot))
      (setf (users bot) (cl-store:restore (user-db bot))))))

(defmessage save-users (bot)
  (:reply ((bot =sykobot=))
    (cl-store:store (users bot) (user-db bot))))

;;;
;;; Time utils
;;;
(defstruct (time (:constructor %make-time
                               (universal-time second minute
                                hour date month year day daylight-p zone)))
  universal-time second minute hour date month year day daylight-p zone)

(defun make-time (&optional (universal-time (get-universal-time)))
  (multiple-value-call #'%make-time universal-time (decode-universal-time universal-time)))

;;;
;;; Transactions
;;;
(defclass transaction ()
  ((time :initform (make-time)
         :initarg :time)
   (description :initform "No description"
                :initarg :description)
   (source :initform (error "Transaction must be given a source PC.")
           :initarg :source)
   (target :initform (error "Transaction must be given a target PC.")
           :initarg :target)))

(defmethod print-object ((obj transaction) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (let ((from (slot-value obj 'source))
          (to  (slot-value obj 'target)))
      (format stream "From: ~A, To: ~A" from to))))

(defclass favor (transaction) ())
(defgeneric favorp (obj)
  (:method ((obj t)) nil)
  (:method ((obj favor)) t))

(defclass disfavor (transaction) ())
(defgeneric disfavorp (obj)
  (:method ((obj t)) nil)
  (:method ((obj disfavor)) t))

(defun favor (actor target &optional (description "No description."))
  (if (string-equal actor target)
      (error "No, you can't favor yourself.")
      (push (make-instance 'favor
                           :description description
                           :source actor
                           :target target)
            (favors *active-bot*))))

(defun disfavor (actor target &optional (description "No description."))
  (if (string-equal actor target)
      (error "No, you can't disfavor yourself.")
      (push (make-instance 'disfavor
                           :description description
                           :source actor
                           :target target)
            (favors *active-bot*))))

;;;
;;; Path-finding
;;;
(defun node-neighbors (node inclusion-function)
  "Returns a list of neighbors of NODE. A PC is a neighbor iff there is a transaction from node to
that PC, and the transaction passes INCLUSION-FUNCTION. INCLUSION-FUNCTION should be a two-argument
function that accepts a PC (the current node we're getting neighbors for), and a transaction object,
and returns a generalized boolean that answers whether that transaction's target should be in the
list of NODE's neighbors."
  (remove-duplicates
   (mapcar (lambda (txn) (slot-value txn 'target))
           (remove-if-not (lambda (txn)
                            (funcall inclusion-function node txn))
                          *reputation-transactions*))))

(defun dijkstra (graph source inclusion-func)
  "Mostly standard implementation of Dijkstra's algorithm. Returns a hash table of distances and
a hash table with shortest paths. INCLUSION-FUNC is used by #'NODE-NEIGHBORS."
  (let ((distances (make-hash-table :test 'equalp))
        (previous (make-hash-table :test 'equalp)))

    (loop for node in graph do
         (setf (gethash node distances) nil))

    ;; Distance from source to source
    (setf (gethash source distances) 0)

    (flet ((smallest-distance ()
             (let (smallest smallest-value)
               (maphash (lambda (k v)
                          (when (find k graph)
                            (cond ((and (numberp v)
                                        (numberp smallest-value)
                                        (> smallest-value v))
                                   (setf smallest k smallest-value v))
                                  ((and (numberp v)
                                        (null smallest-value))
                                   (setf smallest k smallest-value v))
                                  ((null smallest)
                                   (setf smallest k smallest-value v))
                                  (t nil))))
                        distances)
               smallest)))
      (loop while graph
         for node = (smallest-distance)
         do (setf graph (remove node graph))
         when (gethash node distances)
         do (loop for neighbor in (node-neighbors node inclusion-func)
               do (let ((node-distance (gethash node distances))
                        (neighbor-distance (gethash neighbor distances)))
                    (when (or (not (or node-distance neighbor-distance))
                              (and node-distance (null neighbor-distance))
                              (< (1+ node-distance)
                                 neighbor-distance))
                      (setf (gethash neighbor distances) node-distance
                            (gethash neighbor previous) node)))))
      (values distances previous))))

(defun shortest-indirect-path (graph source target inclusion-func)
  "Finds the shortest *INDIRECT* path between SOURCE and TARGET. That is, SOURCE->TARGET is not
considered a valid path."
  (multiple-value-bind (distances previous)
      (dijkstra graph source inclusion-func)
    (declare (ignore distances))
    (loop with list = nil
       with u = target
       while (gethash u previous)
       do (push u list)
         (setf u (gethash u previous))
       finally (return (when list (cons source list))))))

(defun all-indirect-paths (graph source target inclusion-func)
  "Finds all indirect shortest paths between SOURCE and TARGET."
  (loop for shortest = (shortest-indirect-path graph source target inclusion-func)
     while shortest
     do (setf graph (remove (car (last (butlast shortest))) graph))
     collect shortest))

;;;
;;; Metrics
;;;
(defparameter *distance-decay-factor* 1)
(defparameter *repeated-favor-decay* 4/5)

(defun relevant-time-p (time upper-bound lower-bound)
  "Evaluates to true if TIME is between UPPER-BOUND and LOWER-BOUND."
  (apply #'> (mapcar #'time-universal-time (list upper-bound time lower-bound))))

(defun clamp-transactions (txn-list from to)
  "Returns a list of transactions from TXN-LIST whose time is between FROM and TO."
  (remove-if-not (lambda (txn)
                   (relevant-time-p (slot-value txn 'time) to from))
                 txn-list))

(defun direct-favor (judge target from to)
  (when (string-equal judge target)
    (error "Can't check direct favor for yourself."))
  (let ((*reputation-transactions* (clamp-transactions (favors *active-bot*) from to)))
    (%direct-favor judge target *repeated-favor-decay*)))

(defun %direct-favor (judge target decay-factor)
  "Calculates the direct favor from JUDGE to TARGET. DECAY-FACTOR represents how quickly repeated
favor/disfavors might decay in value. When DECAY-FACTOR < 1, an infinite number of transactions will
eventually converge on a single number. When > 1, favor can grow unbounded into infinity."
  (flet ((relevant-transaction-p (transaction)
           (when (and (string-equal (slot-value transaction 'target) target)
                      (string-equal (slot-value transaction 'source) judge))
             t))
     (geometric-sum (first-term common-ratio num-terms)
       "sum of a + ar + ar^2 + ... + ar^(n-1)"
       (/ (* first-term (- 1 (expt common-ratio num-terms)))
          (- 1 common-ratio))))
    (let* ((relevant-transactions (remove-if-not #'relevant-transaction-p
                                                 *reputation-transactions*))
           (favors (remove-if-not #'favorp relevant-transactions))
           (disfavors (remove-if-not #'disfavorp relevant-transactions))
       (favor-value (geometric-sum 1 decay-factor (length favors)))
       (disfavor-value (geometric-sum 1 decay-factor (length disfavors))))
      (- favor-value disfavor-value))))

;;; relative favor
(defun path-favor (path transaction-decay)
  "Given a path, calculates its total value. TRANSACTION-DECAY measures how quickly repeated
favor/disfavors decay in value."
  (if (or (null path) (= 1 (length path)))
      (error "Invalid path ~S" path)
      ;; We only care about the actual opinion of the second-to-last node in the path.
      (let ((judge (car (last (butlast path))))
            (target (car (last path))))
        (let ((unweighted (%direct-favor judge target transaction-decay))
              (weight (* *distance-decay-factor* (1- (length path)))))
          (/ unweighted weight)))))

(defun global-favor (target from to)
  (let* ((*reputation-transactions* (clamp-transactions (favors *active-bot*) from to))
         (relevant-transactions (remove-if-not
                                 (lambda (txn)
                                   (string-equal target (slot-value txn 'target)))
                                 *reputation-transactions*)))
    (reduce #'+ (mapcar (lambda (txn)
                          (%direct-favor (slot-value txn 'source)
                                         target
                                         *repeated-favor-decay*))
                        (remove-duplicates relevant-transactions
                                           :test #'string-equal
                                           :key (lambda (txn)
                                                  (slot-value txn 'source)))))))

(defun relative-favor (observer specimen from to inclusion-function)
  (when (string-equal observer specimen)
    (error "Can't check relative favor for yourself."))
  (let ((*reputation-transactions* (clamp-transactions (favors *active-bot*) from to)))
    (reduce #'+ (cons (%direct-favor observer specimen *repeated-favor-decay*)
                      (mapcar (lambda (path) (path-favor path *repeated-favor-decay*))
                              (all-indirect-paths (users *active-bot*) observer specimen
                                                  inclusion-function))))))

(defun right-handed-favor (observer specimen from to)
  (relative-favor observer specimen from to
                  (lambda (node txn)
                    (and (string-equal node (slot-value txn 'source))
                         (if (string-equal specimen (slot-value txn 'target))
                             t
                             (plusp (%direct-favor node (slot-value txn 'target) *repeated-favor-decay*)))
                         (not (and (string-equal observer (slot-value txn 'source))
                                   (string-equal specimen (slot-value txn 'target))))))))

(defun left-handed-favor (observer specimen from to)
  (relative-favor observer specimen from to
                  (lambda (node txn)
                    (if (string-equal observer (slot-value txn 'source))
                        (unless (string-equal specimen (slot-value txn 'target))
                          (minusp (%direct-favor node (slot-value txn 'target) *repeated-favor-decay*)))
                        (and (string-equal node (slot-value txn 'source))
                             (if (string-equal specimen (slot-value txn 'target))
                                 t
                                 (plusp (%direct-favor node (slot-value txn 'target) *repeated-favor-decay*)))
                             (not (and (string-equal observer (slot-value txn 'source))
                                       (string-equal specimen (slot-value txn 'target)))))))))

;;;
;;; Commands
;;;
(defun ensure-user (username)
  (pushnew username (users *bot*) :test #'string-equal)
  username)

(defcommand favor ("(\\S+) ?(.*)?" name reason)
  "Syntax: 'favor <name> [<reason>]' - grants favor to <name>. Use check-favor to query the database."
  (favor (ensure-user *sender*) (ensure-user name) reason)
  (save-users *bot*)
  (save-favors *bot*)
  "Favor granted!")

(defcommand disfavor ("(\\S+) ?(.*)?" name reason)
  "Syntax: 'disfavor <name> [<reason>]' - disfavors <name>. Use check-favor to query the database."
  (disfavor (ensure-user *sender*) (ensure-user name) reason)
  (save-users *bot*)
  (save-favors *bot*)
  "Disfavor granted!")

(defcommand check-favor ("(\\S+) ?(\\S+)?" name favor-type)
  "Syntax: 'check-favor <name> [<favor type>]'- Checks favor for <name>. Available favor types are: global, direct, right-handed (what your 'friends' think), and left-handed (what your 'enemies' think). The default is right-handed."
  (let ((actual-favor-type (if (> 1 (length favor-type))
                               "right-handed"
                               favor-type)))
    (build-string "~A favor for ~A: ~A"
                  (string-capitalize actual-favor-type)
                  name
                  (coerce (favor-by-type actual-favor-type (ensure-user *sender*) (ensure-user name))
                          'float))))

(defun favor-by-type (type observer target)
  (let ((from (make-time (- (get-universal-time)
                            (* 60 60 24 28))))
        (to (make-time)))
    (cond ((string-equal type "direct")
           (direct-favor observer target from to))
          ((string-equal type "global")
           (global-favor target from to))
          ((or (string-equal type "right-handed")
               (string-equal type "right"))
           (right-handed-favor observer target from to))
          ((or (string-equal type "left-handed")
               (string-equal type "left"))
           (left-handed-favor observer target from to))
          (t (error "No such favor type: ~A" type)))))

