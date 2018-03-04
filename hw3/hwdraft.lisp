
;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )


(defun boxNum (s)
	(cond ((null s) 0)
		(t (+ (count 2 (car s)) (boxNum (cdr s))))
		))

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 



; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) t) ;  if s is a null list, means that we don't detect a 2 until the end of the graph, then it means it is a goal state
  		( (null (checkFuncCol (car s))) (goal-test (cdr s)))
  		(t nil)
  		); end cond
  );end defun

(defun checkFuncCol (s)
  (cond ((null s) nil) ; at the end of the list s, means that at the current row, we don't find any '2'
        ((= 2 (car s)) t) ; if find a '2', then return true
        (t (checkFuncCol (cdr s)))
    ); end cond
  );end defun

; Helper function of get-square.
; find-rc takes in a list, a row number or a column number rc
; It should return the content of that row or integer content at that column

(defun findTargetHelper (S target)
	(cond ((= target 0) (car S))
		(t (findTargetHelper (cdr S) (- target 1)))
		)
	)


; this is helper function for next-states
; input: state S, row, col
; return the value at point(row, col) if row and col value are valid
; return 1(which means it is wall) if row and col are out of bound
(defun get-square (S row col)
	(let* ( (maxRowLen (- (length S) 1))
		(maxColLen (- (length (car S)) 1))
		)
		(cond ((null S) nil)
			((< row 0) 1)
			((> row maxRowLen) 1)
		    ((> col  maxColLen) 1)
			((< col 0) 1)
		(t (findTargetHelper (findTargetHelper S row) col))
		)
		)
	)

; Helper function of set-square
; set_c takes in a row S, a column number c, an integer v
; It should set the integer content at the column to v
(defun set_c (S c v)
	(cond ((= c 0) (cons v (cdr S)))
			(t (cons (car S) (set_c (cdr S) (- c 1) v)))		
		)
	)

; Helper function of set-square
; set_r takes in a state S, a row number r, a column number c and an integer v
; It should set the integer content at position (r,c) to v
(defun set_r (S r c v)
	(cond ((null S) nil)
		((or (> r (- (length S) 1)) (< r 0)) nil)
		((or (> c (- (length (car S)) 1)) (< c 0)) nil)
		((= r 0) (cons (set_c (car S) c v) (cdr S)))
			(t (cons (car S) (set_r (cdr S) (- r 1) c v)))		
		)
	)

; this is a helper function for set-square
; input : state S, r, c, (the point(r,c) is where we want to set value), v(value)

(defun set_r (S r c v)
	(cond ((null S) nil)
	;	((or (> r (- (length S) 1)) (< r 0)) nil)
	;	((or (> c (- (length (car S)) 1)) (< c 0)) nil)
		((= r 0) (cons (set_c (car S) c v) (cdr S)))
			(t (cons (car S) (set_r (cdr S) (- r 1) c v)))		
		)
	)

; set-square is helper function for try-move
; input: state S, row, col (we want to move to point(row, col)), r, c (we move from point (r,c) )
; output: the state after move
; set-square use get-square function to get the current value of different points
; (mainly consider three points here: point(row, col) is the point we want to move to, point(r,c) is the point we move from,
; point(a, b) is the point behind the point(row, col), I consider point(a, b) is because it will influence my pushing behavior)
; if the point we want to move to has a box and the point behind the box is a wall or a box also, then we can't move, otherwise we could move

(defun set-square(S row col r c)
	(let* (   (pTo (get-square S row col)) 
		(pFrom (get-square S r c))
		(a (+ row (- row r))) 
		(b (+ col (- col c)))
		(pBehind (get-square S a b))
		(behave (cond ((isBlank pTo) 1) ; this is the case when pTo is a blank then feel free to move to there, 1 means case1
			((isStar pTo) 2) ; this is the case when pTo is a goal state, then feel free to move to there, 2 means case2
			;;when pTo has a box there and we have to consider the point behind pTo, pTo maybe a box or a box with goal state
			;;then we could only push the box if the point behind the box is not wall, not box, not box and goal, 
			;;thus the cases we remain are when the point behind is blank or the point behind is a goal state
			((and (isBox pTo) (isBlank pBehind)) 3) ; when only push box and the point behind is blank, 3 means case 3 
			((and (isBox pTo) (isStar pBehind)) 4) ; when only push box and the point behind is a goal state, 4 means case 4
			((and (isBoxStar pTo) (isBlank pBehind)) 5); when push box at goal state and the point behind is blank, 5 means case 5
			((and (isBoxStar pTo) (isStar pBehind)) 6); when  push box at goal state and the point behind is a goal state, 6 means case 6
			(t 0) ; other cases, when we are not able to move
			))
		(state S) ;initalize the state first since we can not change the orignial state, make a copy of it
		;; change the point we move from, if the point is keeper, set it to 0, otherwise(the case is the point is keeper + goal state), set it to 4
		(state (cond ((isKeeper pFrom) (set_r state r c 0)) 
			(t (set_r state r c 4))))
		;; if the point we move to has a goal state, we assign it to 6, if it is blank, we assign it to 3,
		(state (cond ((= 2 behave) (set_r state row col 6) )
			((= 5 behave) (set_r state row col 6) )
			((= 6 behave) (set_r state row col 6) )
			(t (set_r state row col 3)))))
		(cond
			((null state) nil)
			((= behave 0) nil) ; if can not move
			((= behave 5) (set_r state a b 2))
			((= behave 3) (set_r state a b 2))
			((= behave 5) (set_r state a b 5))
			((= behave 6) (set_r state a b 5))
			(t state)
			)
		)
	)

; Helper function of next-states
; try-move takes in a state S, and a move Direction D
; this function should return the state that is the result 
; of moving the keeper in state S in direction D
; nil should be return if there is a wall in that direction
(defun try-move (S D)
	(let* ((a (getKeeperPosition S 0))
		(b (car a))
		(c (second a))
		)
	(cond  ((equal D 'UP) (set-square S (- c 1) b c b))
			((equal D 'DOWN) (set-square S (+ c 1) b c b) )
			((equal D 'LEFT) (set-square S c (- b 1) c b))
			((equal D 'RIGHT) (set-square S c (+ b 1) c b)))
		)
	)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

(defun next-states (s)
  (let (
  	(result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT))))
  	(cleanUpList result)); end let
  ); end defun



; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; this is an admissible heuristic
(defun h1 (s)
  (boxNum s)
  )

; Helper function of compute_distance
; absolute takes two numbers and returns the absolute value of their difference
(defun absolute (a b)
	(cond ((< a b) (- b a))
		(t (- a b))
		)
	)

; Helper funtion of h6044309206
; computer_distance takes two locations and calculate their manhattan distance
(defun compute_distance (a b)
	(+ (absolute (first a) (first b)) (absolute (second a) (second b)))
	)


; Helper function of h604439206
; box_list takes a state s and a row number r and a column number c
; it returns the list of locations of boxes
(defun box_list (s r)
	(cond ((null s) nil) 
		(t (append (box_list_helper (first s) r 0) (box_list (rest s) (+ r 1))))
		)
	)


; helper function of box_list
; box_list_helper takes a row called row, a row number r and a column number c 
(defun box_list_helper (row r c)
	(cond ((null row) nil)  
		((isBox (first row)) (cons (list r c) (box_list_helper (rest row) r (+ c 1))))
		(t (box_list_helper (rest row) r (+ c 1)))
		)
	)


; Helper function of h604439206
; goal_list takes a state s and a row number r and a column number c
; it returns the list of locations of goals
(defun goal_list (s r) 
	(cond ((null s) nil) 
		(t (append (goal_list_helper (first s) r 0) (goal_list (rest s) (+ r 1))))
		)
	)


; helper function of goal_list
; goal_list_helper takes a row called row, a row number r and a column number c 
(defun goal_list_helper (row r c)
	(cond ((null row) nil) 
		((or (isStar (first row)) (isKeeperStar (first row)) (isBoxStar (first row))) 
				(cons (list r c) (goal_list_helper (rest row) r (+ c 1)))) 
		(t (goal_list_helper (rest row) r (+ c 1))) 
		)
	)


; Helper function of h 604439206
; keeper_to_boxes takes a state s and a list of location of boxes
; It returns the sum of the distance from keeper to boxes not at goals
(defun keeper_to_boxes (s boxes)
	(let* ((pos (getKeeperPosition s 0))
	 (a (car pos))
	 (b (cadr pos))) 
	 (cond ((null boxes) 0) 
	 	(t (- (+ (compute_distance (list b a) (first boxes)) (keeper_to_boxes s (rest boxes)) ) 1)) 
	 	)
	 )
	)


; Helper function of h604439206
; box_to_goal_list takes the list of box locations and the list of goal locations
; It returns the sum of shortest distance for a box to any goal
(defun box_to_goal_list (boxes goals)
	(cond 
		((null boxes) 0) 
		 
		(t (+ (box_to_goal_helper (first boxes) goals 0) (box_to_goal_list (rest boxes) goals)))
		)
	)

; helper function of box_to_goal_list
; box_to_goal_helper takes the position of a box, the list positions of goals, and current minimum
; it returns the shortest distance from the box to the goal
(defun box_to_goal_helper (box goals minimum)
	(cond
		((null goals) minimum) 
		((or (= minimum 0) (< (compute_distance box (first goals)) minimum)) 
			(box_to_goal_helper box (rest goals) (compute_distance box (first goals))) ) 
		(t (box_to_goal_helper box (rest goals) minimum)) 
		)
	)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 

; Sum of minimum distance from keeper to box and minimum distance from boxes to goals
(defun h604439206 (s)
	(cond ((null s) 0) 
		(t (+ (box_to_goal_list (box_list s 0) (goal_list s 0)) 
			(keeper_to_boxes s (box_list s 0))) 
		)
		)
	)




; Check if there is any box that is temporarily stucked
; (if it is not blocked by two walls, then it is still movable)
; In such cases, we need at least 3 more moves to make it possible to be pushed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
