;;; This file is meant to provide a macro layer for Sly and Slime


;;; This contains a few things:
;;; - 5AM is used for testing, but only if it is a *features* member.

;;; The abstraction gets more specific as you go to the end of the file.

(in-package :sl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-case (base ex)
    (if (symbolp ex)
        (list 'eql base ex)
        (append ex (list base))))
  (defun select-preference (boundp qnames)
    "Choose the function to use based on user preference."
    (labels ((select-preference-aux (boundp qnames preflist)
               (when preflist
                 (let* ((res (select (lambda (x)
                                       (string-equal (car x) (car preflist)))
                               qnames))
                        (package (car res))
                        (symname (cadr res)))
                   (if (and res (find-package package))
                       (values symname package)
                       (select-preference-aux boundp qnames (cdr preflist)))))))
      (select-preference-aux boundp qnames (mapcar #'car qnames)))))

#+5am (defmacro show-bound (unbind predicate thing &body body)
        `(progn (,unbind ',thing)
                ,@body
                (,predicate ',thing)))
#+5am (defmacro show-boundfn (fn &body body)
       `(show-bound fmakunbound fboundp ,fn ,@body))
#+5am (defmacro show-boundvar (var &body body)
        `(show-bound makunbound boundp ,var ,@body))

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
        (defweak symbol-function fboundp operator-arglist
                      swank operator-arglist
                      slynk operator-arglist))

(defmacro defequivs (namespace boundp sl-name &rest packages)
  "Define a name which is already the same in multiple packages."
  `(defweak ,namespace ,boundp ,sl-name
     ,@(append (interpol sl-name packages) (list sl-name))))
#+5am (show-boundfn operator-arglist (defequivs symbol-function fboundp operator-arglist swank slynk))

(defmacro andcond (&rest pairs)
  "Insert AND into a list in the condition position of a COND."
  `(cond ,@(loop for p in pairs
                 collect (cons (list* 'and (car p))
                               (cdr p)))))
#+5am (andcond (((eql t nil) (eql t t)) 'x)
               ((t nil) 'y))
#+5am (and (gen-case :fn :fn)
           (not (gen-case :fn '(listp))))

(defmacro bicase (a b &rest cases)
  "Cover a CASE with two objects, and insert them into predicates."
  ;; FIXME: the predicate insertion is fragile: only flat lists allowed
  ;; FIXME: This should be generatlized for n cases (not just two).
  `(cond ,@(loop for c in cases
                 collect (list* (and (gen-case a (car c))
                                     (gen-case b (cadr c)))
                                (cddr c)))))
#+5am (bicase :a :b
              ((eql :a) (eql :b) t)
              (:a :b 'h))

(defun neql (a b)
  "Not eql."
  ;; KLUDGE: this thing only exists to keep `bicase' flat.
  (not (eql a b)))
(defmacro defsl (sl-name fn-sym eq-ql &rest preflist)
"Magic macro to associate a name with functions in other packages, based on the preferences."
  ;; FIXME: This thing generates invalid--though inaccessible code. This makes
  ;; those compilation errors.
  ;; FIXME: This thing is way unhygienic. Once-only, and in-order would be nice.
  `(bicase
    ,fn-sym ,eq-ql
    (:fn :eq            (defequivs symbol-function fboundp ,sl-name ,@preflist))
    (:fn (neql :eq)     (defweak symbol-function fboundp ,sl-name ,@(pack eq-ql)))
    (:sym :eq           (defequivs symbol-value boundp ,sl-name ,@preflist))
    (:sym (neql :eq)    (defweak symbol-value boundp ,sl-name ,@(pack eq-ql)))
    ((listp) :eq        (defequivs ,@(pack fn-sym) ,sl-name ,@preflist))
    ((listp) (neql :eq) (defweak ,@(pack fn-sym) ,sl-name ,@(pack eq-ql)))))
#+5am (defsl operator-arglist (symbol-function fboundp) :eq slynk swank)
#+5am (defsl operator-arglist :fn :eq slynk swank)
