;;; This file is meant to provide a macro layer for Sly and Slime


;;; This contains a few things:
;;; - 5AM is used for testing, but only if it is a *features* member.

;;; The macros get more specific as you go to the end of the file.
(in-package :sl)

#+5am (defmacro show-bound (unbind predicate thing &body body)
        `(progn (,unbind ',thing)
                ,@body
                (,predicate ',thing)))
#+5am (defmacro show-boundfn (fn &body body)
       `(show-bound fmakunbound fboundp ,fn ,@body))
#+5am (defmacro show-boundvar (var &body body)
        `(show-bound makunbound boundp ,var ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *preference-list* '(slynk swank)
    "The order in which to choose the IDE for pinging. Use the name of the CL
interface, not the Emacs name (ie sly->slynk, slime->swank)."))

(defun select-preference (boundp qnames &optional (preflist *preference-list*))
  "Choose the function to use based on user preference."
  (when preflist
    (let* ((res (select (lambda (x) (string-equal (car x) (symbol-name
                                                           (car preflist))))
                  qnames))
           (package (car res))
           (symname (cadr res)))
      (if (and res (find-package package))
          (values symname package)
          (select-preference boundp qnames (cdr preflist))))))
#+5am (select-preference 'fboundp '(("SLYNK" "OPERATOR-ARGLIST")))

(defmacro defweak-strings (namespace boundp sl-name &rest qualified-names)
  "Make a weak definition, based on preferences. Probably should use wrapper
functions instead of this one, as it takes string arguments."
  `(setf (,namespace (intern ,sl-name))
         (multiple-value-bind (symname package)
             (select-preference ',boundp ',(group qualified-names 2))
           (,namespace (intern symname package)))))
#+5am (show-boundfn operator-arglist
                  (defweak-strings symbol-function fboundp "OPERATOR-ARGLIST"
                    "SWANK" "OPERATOR-ARGLIST"
                    "SLYNK" "OPERATOR-ARGLIST"))

(defmacro defweak (namespace boundp sl-name &rest qualified-names)
  "Symbol wrapper for defweak-strings."
  `(defweak-strings ,namespace ,boundp ,(symbol-name sl-name)
     ,@(loop for q in qualified-names
             collect (symbol-name q))))
#+5am (show-boundfn operator-arglist
                    (defweak-syms symbol-function fboundp operator-arglist
                      swank operator-arglist
                      slynk operator-arglist))

(defmacro defequivs (namespace boundp sl-name &rest packages)
  "Define a name which is already the same in multiple packages."
  `(defweak-syms ,namespace ,boundp ,sl-name
     ,@(append (interpol sl-name packages) (list sl-name))))
#+5am (show-boundfn operator-arglist (defequivs symbol-function fboundp operator-arglist swank slynk))

(defmacro andcond (&rest pairs)
  "Insert AND into a list in the condition position of a COND."
  `(cond ,@(loop for p in pairs
                 collect (cons (list* 'and (car p))
                               (cdr p)))))
#+5am (andcond (((eql t nil) (eql t t)) 'x)
               ((t nil) 'y))

(defun gen-case (base ex)
  (if (symbolp ex)
      (list 'eql base ex)
      (append ex (list base))))
#+5am (and (gen-case :fn :fn)
           (not (gen-case :fn '(listp))))

(defmacro bicase (a b &rest cases)
  `(cond ,@(loop for c in cases
                 collect (list* (and (gen-case a (car c))
                                     (gen-case b (cadr c)))
                                (cddr c)))))
#+5am (bicase :a :b
              ((eql :a) (eql :b) t)
              (:a :b 'h))
(defmacro defsl (sl-name fn-sym eq-ql &optional (preflist *preference-list*))
  "Magic macro to associate a name with functions in other packages, based on the preferences."
  `(bicase
    ,fn-sym ,eq-ql
    (:fn :eq           (defequivs symbol-function fboundp ,sl-name ,@preflist))
    (:fn (not :eq)     (defweak   symbol-function fboundp ,sl-name ,@eq-ql))
    (:sym :eq          (defequivs symbol-name     boundp  ,sl-name ,@preflist))
    (:sym (not :eq)    (defweak   symbol-name     boundp  ,sl-name ,@eq-ql))
    ((listp) :eq       (defequivs ,@(pack fn-sym) ,sl-name ,@preflist))
    ((listp) (not :eq) (defweak   ,@(pack fn-sym) ,sl-name ,@eq-ql))))
#+5am (defsl operator-arglist (symbol-function fboundp) :eq)
#+5am (defsl operator-arglist :fn :eq)
 ;; (defmacro defmagic (sl-name &key fn var sl-only packages)
 ;;   "Attempt to magically use the right macro with minimum information."
 ;;  ())
;; (defweakequiv defweakfn "operator-arglist")
