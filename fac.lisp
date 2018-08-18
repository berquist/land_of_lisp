(defun factorial (n)
  (if (= n 0)
      1
    (* n (factorial (- n 1)))))

(factorial 1000)

(defun factorial (n &optional (acc 1))
  (if (= n 0)
      acc
    (factorial (- n 1) (* acc n))))

(factorial 1000)

(defun factorial (n)
  (loop for i from 1 to n
        for fac = 1 then (* fac i)
        finally (return fac)))

(factorial 1000)
