(if '()
    'i-am-true
  'i-am-false)

(if '(1)
    'i-am-true
  'i-am-false)

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
    0))

(my-length '(list with four symbols))

;; An alternate version.
(defun my-length-2 (list)
  (if (eq list nil)
      0
    (1+ (my-length-2 (cdr list)))))

(my-length-2 '(list with four symbols))

(if (oddp 5)
    'odd-number
  (/ 1 0))

(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
  'even-number)
