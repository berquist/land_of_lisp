(defun half (n)
  (/ n 2))

(lambda (n)
  (/ n 2))

(assert (equal (mapcar (lambda (n) (/ n 2)) '(2 4 6))
               (mapcar #'half '(2 4 6))))
