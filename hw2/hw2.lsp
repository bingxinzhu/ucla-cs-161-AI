;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;
; for problem 1, the input is a list and every time I just check the first element of the input
; for the remaining of the list, I will pass it to the function again(basically..)
; if the first element is a list, which shows that it is not what we want, 
;(we want leaf node but if it is a list, which means it must have child nodes)
; in this case, we move it to the end of the list, whichi is because as you could image, after you travese the list, it will come again 
; if it is an atom then it is a leaf node, so feel free to add to our result list and call BFS for remaining
(defun BFS (FRINGE)
  (cond
     ((null FRINGE) nil)
     (    (listp (car FRINGE))(BFS(append (cdr FRINGE) (car FRINGE))))
     (T   (cons (car FRINGE)(BFS(cdr FRINGE)) ))
  )
)



;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
  (cond ((equal S '(T T T T))T) 
    (T nil)))
; for every action we have to check whether it is a valid action by
; 1, if the action can be applied, 
; 2, if the result state is true
(defun NEXT-STATE (S A)
  (cond (( and (not (car S)) (car(cdr S)) (car(cdr (cdr S))) ) nil)
        (( and (not (car S)) (car(cdr S)) (car(cdr(cdr (cdr S))) )) nil)
        ((equal A 'h) (list (list (not(car S))   (car (cdr S))  (car(cdr (cdr S))) (car(cdr(cdr (cdr S)))))))
        ((and (equal A 'b) (equal (car S) (car(cdr S)) )) 
          (list (list (not(car S))   (not(car (cdr S)))  (car(cdr (cdr S))) (car(cdr(cdr (cdr S)))))))
        ((and (equal A 'd) (equal (car S) (car(cdr (cdr S)))) ) 
          (list(list (not(car S))(car (cdr S)) (not(car(cdr (cdr S))) )(car(cdr(cdr (cdr S))))))) 
        ((and (equal A 'p)  (equal (car S) (car(cdr (cdr S))))) 
          (list (list  (not(car S) )(car (cdr S)) (car(cdr (cdr S))) (not(car(cdr(cdr (cdr S))))))))
        (T nil)))


; in this function, it applies all actions to current state and use NEXT-STATE to generate states
; if NEXT-STATE returns state that is not nil, means it is a successful state
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))

; this function checks that if s is already in the path(STATES)
(defun ON-PATH (S STATES)
  ( cond ((null STATES) nil) 
    ((equal s (car STATES)) T)
    (T (ON-PATH S (cdr STATES)))))

;idea is to loop here to get every state in STATES, and DFS again
(defun MULT-DFS (STATES PATH)
    (cond ((null STATES) nil)
        ((DFS (car STATES) PATH) (DFS (car STATES) PATH)) ; want to use let here but failed
        (T (MULT-DFS (cdr STATES) PATH))))

; main function, check if the current state is the goal state, or if it is already visited,
; if neither, then call MULT-DFS
(defun DFS (S PATH)
  (cond  ((FINAL-STATE S) (append PATH (list S)))
       ((equal  (ON-PATH S PATH ) T) nil)
       (T  (MULT-DFS (SUCC-FN S) (append PATH (list S))))))
