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

; *********************
;    MY IMPLEMENTATION
; ********************* 
;this funciton will return the value at rth row and cth col if the input index is legal.
;If it's outside the range, it will return -1
;the input index is bounded in 0<= r < num_rol; 0 <= r < num_col
(defun lookat (board r c)
	(
		if (and (>= r 0) (>= c 0) (< r (get_size board)) (< c (get_size (car board))))
		(nth c (nth r board))
		-1
	)
)

;get the size of a list
(defun get_size (l)
	(cond ((null l) 0)
	      (t (+ 1 (get_size (cdr l))))
	)
)
;give in a value, check it it's a place the keeper/box can go into
;this one is not exhaustive because a box can't go into a box also, but
;a keeper but can push to box forward, provided the place in front of the box
;is not a Wall or a Box or a BoxStar
(defun isLegal (stuff)
	(
		if (or (null stuff) (isWall stuff)) nil t
	)
)

;takes in a board, a list of row numbers, a list of col numbers, a list of values, and the number of places to update
;this function is in this weird form because it was orginally built for update a single value at a time, but later
;I realize we may have to update two places at a time(a keeper and a box), but this realization is still not quite
;correct: if we a keeper pushes a box forward, the place of the keeper, of the box, and the place in front of the box
;must all be updated.
(defun update (old_board rs cs vals cnt)
	(
		let ((fst_r (car rs)) (fst_c (car cs)) (fst_val (car vals))  (snd_r (cadr rs)) (snd_c (cadr cs)) (snd_val (cadr vals)))
		(if (= cnt 2) (
			let* ((unmodified_hl (butlast old_board (- (get_size old_board) fst_r))) (unmodified_tl (nthcdr (+ 1 fst_r) old_board)) (old_row (nth fst_r old_board)) ;we need to retain all rows unmodified
		     (unmodified_c_hl (butlast old_row (- (get_size old_row) fst_c))) (unmodified_c_tl (nthcdr (+ 1 fst_c) old_row)));we need to retain all values in a rows unmodified
			(update (append unmodified_hl (list (append unmodified_c_hl (list fst_val) unmodified_c_tl)) unmodified_tl) (list snd_r) (list snd_c) (list snd_val) 1);we concatonate the updated value with values in the original row, 
																																									;and concatonate the updated row with the original rows
		)
		(
			let* ((unmodified_hl (butlast old_board (- (get_size old_board) fst_r))) (unmodified_tl (nthcdr (+ 1 fst_r) old_board)) (old_row (nth fst_r old_board))
		     (unmodified_c_hl (butlast old_row (- (get_size old_row) fst_c))) (unmodified_c_tl (nthcdr (+ 1 fst_c) old_row)))
			(append unmodified_hl (list (append unmodified_c_hl (list fst_val) unmodified_c_tl)) unmodified_tl)
		)
		)	
	)
)


; *****************************
;    ENDOF MY IMPLEMENTATION
; *****************************


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

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (r c).
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
		 (list row x)
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



; *****************************
;    MY IMPLEMENTATION
; *****************************


;helper function for goal-test.Check if there is any Box not on the star.
(defun apply-check (l)
	( let ((rstl (cdr l)))
	  (cond ((null l) t)
		    (t (and (apply-check rstl) (not (isBox (car l)))))
		)
	 )
 )
; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

;if there is no box on the playground, we are in the goal state. This means
;no 2 in the list of lists.
(defun goal-test (s)
  	(cond ((null s) t)
          (t (and (goal-test (cdr s)) (apply-check (car s))))             
	)
  );end defun

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


;for each direction we do the following:
;	check if the next place along that direction is legal for the keeper to walk into
;		-if so, check if it is a box
;			-if so, check if the place ahead the tile is legal for the tile to move into
;				-if so, both the box and the keeper advance along that directions, update the playground correspondingly
;				-if not, illegal move, return nil
;			-if not, advance the keeper into that place, update the playground correspondingly
;		-if not, illegal move, return nil
(defun try-move (s r c A)
	(
		cond ((equal A 'UP) ( 
			if (isLegal (lookat s (- r 1) c)) 
				(
					if (or (isBox (lookat s (- r 1) c)) (isBoxStar (lookat s (- r 1) c)))
						(if (or (not (isLegal (lookat s (- r 2) c))) (isBox (lookat s (- r 2) c)) (isBoxStar (lookat s (- r 2) c)))
							nil
							(
								let ((mypos (lookat s r c)) (nextpos (lookat s (- r 1) c)) (nextnextpos (lookat s (- r 2) c)))
								(cond ((and (isKeeper mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list (- r 1) (- r 2)) (list c c) (list keeper box) 2) (list r) (list c) (list blank) 1
										)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list (- r 1) (- r 2)) (list c c) (list keeperstar box) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list (- r 1) (- r 2)) (list c c) (list keeper boxstar) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list (- r 1) (- r 2)) (list c c) (list keeperstar boxstar) 2) (list r) (list c) (list blank) 1
									)
								)

								((and (isKeeperStar mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list (- r 1) (- r 2)) (list c c) (list keeper box) 2) (list r) (list c) (list star) 1
										)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list (- r 1) (- r 2)) (list c c) (list keeperstar box) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list (- r 1) (- r 2)) (list c c) (list keeper boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list (- r 1) (- r 2)) (list c c) (list keeperstar boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								(t nil))
							)
						)				
						(;second case, no box, just move the player
							let ((mypos (lookat s r c)) (nextpos (lookat s (- r 1) c)))
							(cond ((and (isKeeper mypos) (isBlank nextpos))
								(
									update s (list r (- r 1)) (list c c) (list blank keeper) 2
								)
							)
						 	((and (isKeeper mypos) (isStar nextpos))
								(
									update s (list r (- r 1)) (list c c) (list blank keeperstar) 2
								)
							)
							 ((and (isKeeperStar mypos) (isBlank nextpos))
								(
									update s (list r (- r 1)) (list c c) (list star keeper) 2
								)
							)
						 	((and (isKeeperStar mypos) (isStar nextpos))
								(
									update s (list r (- r 1)) (list c c) (list star keeperstar) 2
								)
							)
							(t nil))
						)
				)
				nil
			)
		)
		((equal A 'DOWN) (
			if (isLegal (lookat s (+ r 1) c)) 
				(
					if (or (isBox (lookat s (+ r 1) c)) (isBoxStar (lookat s (+ r 1) c)))
						(if (or (not (isLegal (lookat s (+ r 2) c))) (isBox (lookat s (+ r 2) c)) (isBoxStar (lookat s (+ r 2) c)))
							nil
							(
								let ((mypos (lookat s r c)) (nextpos (lookat s (+ r 1) c)) (nextnextpos (lookat s (+ r 2) c)))
								(cond ((and (isKeeper mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeper box) 2) (list r) (list c) (list blank) 1
										)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeperstar box) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeper boxstar) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeperstar boxstar) 2) (list r) (list c) (list blank) 1
									)
								)

								((and (isKeeperStar mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeper box) 2) (list r) (list c) (list star) 1
										)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeperstar box) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeper boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list (+ r 1) (+ r 2)) (list c c) (list keeperstar boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								(t nil))
							)
						)				
						(;second case, no box, just move the player
							let ((mypos (lookat s r c)) (nextpos (lookat s (+ r 1) c)))
							(cond ((and (isKeeper mypos) (isBlank nextpos))
								(
									update s (list r (+ r 1)) (list c c) (list blank keeper) 2
								)
							)
						 	((and (isKeeper mypos) (isStar nextpos))
								(
									update s (list r (+ r 1)) (list c c) (list blank keeperstar) 2
								)
							)
							 ((and (isKeeperStar mypos) (isBlank nextpos))
								(
									update s (list r (+ r 1)) (list c c) (list star keeper) 2
								)
							)
						 	((and (isKeeperStar mypos) (isStar nextpos))
								(
									update s (list r (+ r 1)) (list c c) (list star keeperstar) 2
								)
							)
							(t nil))
						)
				)
				nil
			)
		)
		((equal A 'RIGHT) (
			if (isLegal (lookat s r (+ c 1))) 
				(
					if (or (isBox (lookat s r (+ c 1))) (isBoxStar (lookat s r (+ c 1))))
						(if (or (not (isLegal (lookat s r (+ c 2)))) (isBox (lookat s  r (+ c 2))) (isBoxStar (lookat s  r (+ c 2))))
							nil
							(
								let ((mypos (lookat s r c)) (nextpos (lookat s r (+ c 1))) (nextnextpos (lookat s r (+ c 2))))
								(cond ((and (isKeeper mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeper box) 2) (list r) (list c) (list blank) 1
										)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeperstar box) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeper boxstar) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeperstar boxstar) 2) (list r) (list c) (list blank) 1
									)
								)

								((and (isKeeperStar mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeper box) 2) (list r) (list c) (list star) 1
										)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeperstar box) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeper boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (+ c 1) (+ c 2)) (list keeperstar boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								(t nil))
							)
						)				
						(;second case, no box, just move the player
							let ((mypos (lookat s r c)) (nextpos (lookat s r (+ c 1))))
							(cond ((and (isKeeper mypos) (isBlank nextpos))
								(
									update s (list r r) (list c (+ c 1)) (list blank keeper) 2
								)
							)
						 	((and (isKeeper mypos) (isStar nextpos))
								(
									update s (list r r) (list c (+ c 1)) (list blank keeperstar) 2
								)
							)
							 ((and (isKeeperStar mypos) (isBlank nextpos))
								(
									update s (list r r) (list c (+ c 1)) (list star keeper) 2
								)
							)
						 	((and (isKeeperStar mypos) (isStar nextpos))
								(
									update s (list r r) (list c (+ c 1)) (list star keeperstar) 2
								)
							)
							(t nil))
						)
				)
				nil
			)
		)
		((equal A 'LEFT) (
						if (isLegal (lookat s r (- c 1))) 
				(
					if (or (isBox (lookat s r (- c 1))) (isBoxStar (lookat s r (- c 1))))
						(if (or (not (isLegal (lookat s r (- c 2)))) (isBox (lookat s  r (- c 2))) (isBoxStar (lookat s  r (- c 2))))
							nil
							(
								let ((mypos (lookat s r c)) (nextpos (lookat s r (- c 1))) (nextnextpos (lookat s r (- c 2))))
								(cond ((and (isKeeper mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list r r) (list (- c 1) (- c 2)) (list keeper box) 2) (list r) (list c) (list blank) 1
										)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list r r) (list (- c 1) (- c 2)) (list keeperstar box) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (- c 1) (- c 2)) (list keeper boxstar) 2) (list r) (list c) (list blank) 1
									)
								)
								((and (isKeeper mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (- c 1) (- c 2)) (list keeperstar boxstar) 2) (list r) (list c) (list blank) 1
									)
								)

								((and (isKeeperStar mypos) (isBox nextpos) (isBlank nextnextpos))
										(
											update (update s (list r r) (list (- c 1) (- c 2)) (list keeper box) 2) (list r) (list c) (list star) 1
										)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isBlank nextnextpos))
									(
										update (update s (list r r) (list (- c 1) (- c 2)) (list keeperstar box) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBox nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (- c 1) (- c 2)) (list keeper boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								((and (isKeeperStar mypos) (isBoxStar nextpos) (isStar nextnextpos))
									(
										update (update s (list r r) (list (- c 1) (- c 2)) (list keeperstar boxstar) 2) (list r) (list c) (list star) 1
									)
								)
								(t nil))
							)
						)				
						(;second case, no box, just move the player
							let ((mypos (lookat s r c)) (nextpos (lookat s r (- c 1))))
							(cond ((and (isKeeper mypos) (isBlank nextpos))
								(
									update s (list r r) (list c (- c 1)) (list blank keeper) 2
								)
							)
						 	((and (isKeeper mypos) (isStar nextpos))
								(
									update s (list r r) (list c (- c 1)) (list blank keeperstar) 2
								)
							)
							 ((and (isKeeperStar mypos) (isBlank nextpos))
								(
									update s (list r r) (list c (- c 1)) (list star keeper) 2
								)
							)
						 	((and (isKeeperStar mypos) (isStar nextpos))
								(
									update s (list r r) (list c (- c 1)) (list star keeperstar) 2
								)
							)
							(t nil))
						)
				)
				nil
			)
		)
		(t nil)
		;if equal down, left right, correspondingly()
	)
)








;the result of next-states function is generated by trying going down along all directions.
;we then filter the result out of nil.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (r (car pos))
	 (c (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s r c 'LEFT) (try-move s r c 'RIGHT) (try-move s r c 'UP) (try-move s r c 'DOWN)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;

;A trivial heuristics return 0 for all move
(defun h0 (s) 
 (cond(t 0)
	)
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
;count the number of box(2) inside the playground
;this heuristics is admissable: each misplaced box, in the ideal world
;in which teleportation is possible, will still need 1 teleportation
;to be placed correctly. Therefore, this heuristic is guarantteed to be less than or equal to the cost
;of the optimal solution
(defun h1 (s) 
	(
	cond ((null s) 0)
	(t (+ (helperh1 (car s)) (h1 (cdr s))))
	)
)

;a helper function for h1 to check if there is a box(2) inside a row
(defun helperh1 (s)
	(
	if (null s) 0 (if (= (car s) box) (+ 1 (helperh1 (cdr s))) (helperh1 (cdr s)))
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

;my heuristics first find all unmatched boxes and stars on the playground O(n), calculate the distance between first found box
;and first found star, second found box second found star,etc.. accumulate the distance, and use
;the accumulated distance as the heuristics
(defun h605336075 (s)
	(
		let*((check_box_star (myhelper s 0 0 nil nil)) (box_cor_l (car check_box_star)) (star_cor_l (cadr check_box_star)))
		(calculate box_cor_l star_cor_l 0 )

	)
  )

;calculate the distance between boxes ans stars
(defun calculate (box_l star_l accu)
	(
		cond((null box_l) accu)
		(t
			(let* ((first_box (car box_l)) (first_star (car star_l)))
				(if (or (null first_box) (null first_star)) 
					accu
					(
						let ((first_box_r (car first_box)) (first_box_c (cadr first_box))(first_star_r (car first_star)) (first_star_c (cadr first_star)))
						(if (or (null first_box_r) (null first_star_r) (null first_box_c) (null first_star_c))
							accu
							(
								let ((diff_distance (+ (abs (- first_box_r first_star_r)) (abs (- first_box_c first_star_c)))))
								(calculate (cdr box_l) (cdr star_l) (+ accu diff_distance))
							)
						)
					)     	
				)
			)      
		)
	)
)

;helper function that return a list with position of boxes and positions of goals
(defun myhelper (s r c bl gl)
	(
		let((this_row (car s)))
		(cond((null s) (list bl gl))
		(t (
			let*((this_row_result (myhelper2 this_row r c bl gl)) (this_row_bl (car this_row_result)) (this_row_gl (cadr this_row_result))
			(next_row_result (myhelper (cdr s) (+ r 1) 0 nil nil)) (next_row_bl (car next_row_result)) (next_row_gl (cadr next_row_result)))
			(list (append this_row_bl next_row_bl) (append this_row_gl next_row_gl))
		   )
		)
		)
	)
)

;the helper function of the myhelper function that return a list of boxes and a list of stars in the current row.
(defun myhelper2 (l r c bl gl)
	(
		cond((null l) (list bl gl))
		(t  (
				cond((= (car l) box)  (myhelper2 (cdr l) r (+ c 1) (append bl (list (list r c))) gl))
				((= (car l) star) (myhelper2 (cdr l) r (+ c 1) bl (append gl (list (list r c)))))
				(t (myhelper2 (cdr l) r (+ c 1) bl gl))
			) 
		)
	)
)


; *****************************
;    ENDOF MY IMPLEMENTATION
; *****************************




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
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
