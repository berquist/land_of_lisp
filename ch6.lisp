(load "ch5.lisp")

(print '3)     ; => 3     An integer
(print '3.4)   ; => 3.4   A float
(print 'foo)   ; => FOO   A symbol It may be printed in all caps, since Common Lisp symbols are blind to letter case.
(print '"foo") ; => "foo" A string
(print '#\a)   ; => #\a   A character

(princ '3)     ; => 3
(princ '3.4)   ; => 3.4
(princ 'foo)   ; => FOO
(princ '"foo") ; => foo
(princ '#\a)   ; => a

(defun game-repl ()
  (loop (print (eval (read)))))

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

(game-print '(not only does this sentence have a "comma," it also mentions the "iPad."))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
