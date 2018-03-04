;1
(defun PAD(n)
  (cond ((= 0 n) 1)
    ((= 1 n) 1)
    ((= 2 n) 1)
    (t (+ (PAD(- n 2)) (PAD(- n 3))))))

;2. 
(defun SUMS(N)
  (cond((= 0 N) 0)
  ((= 1 N) 0)
  ((= 2 N) 0)
  (t (+ (SUMS(- N 2)) (SUMS(- N 3)) 1))))

;3.
(defun ANON(L)
  (cond ((null L) nil)
  ((atom L) '?)
  (t (cons (ANON(CAR L)) (ANON(CDR L))))))


