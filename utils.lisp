(in-package :sl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; From Graham's OnLisp
  (defun pack (obj)
    "Ensure obj is a cons."
    (if (consp obj) obj (list obj)))
  (defun shuffle (x y)
    "Interpolate lists x and y with the first item being from x."
    (cond ((null x) y)
          ((null y) x)
          (t (list* (car x) (car y)
                    (shuffle (cdr x) (cdr y))))))
  (defun interpol (obj lst)
    "Intersperse an object in a list."
    (shuffle lst (loop for #1=#.(gensym) in (cdr lst)
                       collect obj)))
  (defun group (source n)
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (rec rest (cons
                                (subseq source 0 n)
                                acc))
                     (nreverse
                      (cons source acc))))))
      (if source (rec source nil) nil))))

(defun select (fn lst)
  "Return the first element that matches the function."
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (select fn (cdr lst))))))
