(in-package :sl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (source n)
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (rec rest (cons
                                (subseq source 0 n)
                                acc))
                     (nreverse
                      (cons source acc))))))
      (if source (rec source nil) nil)))
  (defvar *preference-list* '("slynk" "swank")
    "The order in which to choose the IDE for pinging. Use the name of the CL
interface, not the Emacs name (ie sly->slynk, slime->swank)."))

(defun select (fn lst)
  "Return the first element that matches the function."
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (select fn (cdr lst))))))

(defun select-preference (boundp qnames &optional (preflist *preference-list*))
  (when preflist
    (let* ((res (select (lambda (x) (string-equal (car x) (car preflist)))
                  qnames))
           (package (string-upcase (car res)))
           (symname (string-upcase (cadr res))))
      (if (and res (find-package package))
          symname
          (select-preference boundp qnames (cdr preflist))))))

#+5am (select-preference 'fboundp '(("slynk" "slynk:operator-arglist"))
                         '("slynk" "swank"))

(defmacro defweak (namespace boundp sl-name &rest qualified-names)
  "Make a weak definition, based on preferences. Probably should use wrapper
functions instead of this one."
  ;; FIXME: Why can't I get the symbol's package without errors?
  `(setf (,namespace (intern ,sl-name))
         (select-preference ',boundp ',(group qualified-names 2))))

(defmacro defweakfn (sl-name &rest qualified-names)
  "Define a weak function, based on preferences."
  `(defweak symbol-function fboundp ,sl-name ,@qualified-names))

(defmacro defweakvar (sl-name &rest qualified-names)
  `(defweak symbol-value boundp ,sl-name ,@qualified-names))

(defmacro defweakequiv (defweak sl-name &optional (preflist *preference-list*))
  `(,defweak ,sl-name
       ,@(mapcar #'(lambda (x) (list x (concatenate 'string x ":" sl-name)))
                preflist)))

(defweakfn "operator-arglist"
  "swank" "swank:operator-arglist"
  "slynk" "slynk:operator-arglist")

(defweakequiv defweakfn "operator-arglist")
