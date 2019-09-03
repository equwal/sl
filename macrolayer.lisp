;;; This file is meant to provide a macro layer for Sly and Slime


;;; This contains a few things:
;;; - 5AM is used for testing, but only if it is a *features* member.

;;; The abstraction gets more specific as you go to the end of the file.

(in-package :sl)

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


(defmacro defsl (sl-name fn-sym eq-ql &rest preflist)
  (alexandria:with-gensyms (fs qq)
    `(let ((,fs ,fn-sym)
           (,qq ,eq-ql))

       `(case ,(list ,fs ,qq)
          ;; raw case
          ((:fn :eq)  (defequivs symbol-function fboundp ,sl-name ,@preflist))
          ((:sym :eq) (defequivs symbol-value boundp ,sl-name ,@preflist))

          ;; car cases
          (otherwise
           (case ,fs
             ((:fn ,qq)  (defweak symbol-function fboundp ,sl-name ,@(pack qq)))
             ((:sym ,qq) (defweak symbol-value boundp ,sl-name ,@(pack qq)))
             (otherwise
              (if (eql ,qq :eq)
                  (defequivs ,@(pack fn-sym) ,sl-name ,@preflist)
                  (defweak ,@(pack fs) ,sl-name ,@(pack qq))))))))))
#+5am (defsl operator-arglist (symbol-function 'fboundp) :eq slynk swank)
#+5am (defsl operator-arglist :fn :eq slynk swank)
