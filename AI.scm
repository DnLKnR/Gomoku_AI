;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;;;; CSci 1901 Project - Spring 2013
;;;; gomoku++ Player AI

;;;======================;;;
;;;  SUBMISSION DETAILS  ;;;
;;;======================;;;

;; List both partners' information below.
;; Leave the second list blank if you worked alone.
(define authors 
  '((
     "Atsuko Fukushi"
     )
    (
     "Daniel Koniar"   ;; Author 2 Name
     )))
;;;====================;;;
;;;  Player-Procedure  ;;;
;;;====================;;;
(define player-procedure
  (let ()
    ;;===================;;
    ;; Helper Procedures ;;
    ;;===============================================================;;
    ;; Include procedures used by get-move that are not available in ;;
    ;;  the util.scm file.  Note: gomoku++.scm loads util.scm, so you;;
    ;;  do not need to load it from this file.                       ;;
    ;; You also have access to the constants defined inside of       ;;
    ;;  gomoku++.scm.                                                ;;
    ;;===============================================================;;
    ;; Returns a random-element from a list.
    (define (random-element lst)
      (list-ref lst (random (length lst))))
    (define (get-random-valid-position board)
      (random-element (get-open-positions board)))   
    (define (my-sleep t)
      (define (helper t)
	(if (> (system-clock) t)
	    0
	    (helper t)))
      (helper (+ (system-clock) (/ t 1000))))
    ;;============;;
    ;; Predicates ;;
    ;;============;;
    (define validandopen?
      (lambda (x board)
	(if (and (valid-position? x)
		 (open-position? x board))
	    #t #f)))
    (define validandtaken?
      (lambda (x board)
	(if (and (valid-position? x)
		 (not (open-position? x board)))
	    #t #f)))
    (define validandowned?
      (lambda (x player board)
	(if (and (valid-position? x)
		 (owns-position? player x board))
	    #t #f)))
    (define validopenowned?
      (lambda (x player board)
	(if (and (valid-position? x)
		 (or (open-position? x board)
		     (owns-position? player x board)))
	    #t #f)))
    (define valid-taken-by-other
      (lambda (x board other-player)
	(if (and (valid-position? x)
		 (owns-position? other-player x board))
	    #t #f)))
    ;;================;;
    ;; For Increments ;;
    ;;================;;
    (define -4+ (lambda (x) (- x 4)))
    (define 4+ (lambda (x) (+ x 4)))
    (define -3+ (lambda (x) (- x 3)))
    (define 3+ (lambda (x) (+ x 3)))
    (define 2+ (lambda (x) (+ 2 x)))
    (define -2+ (lambda (x) (- x 2)))
    (define 0+ (lambda (x) (+ x 0)))
    (define 1+ (lambda (x) (+ 1 x)))
    (define -1+ (lambda (x) (- x 1)))
    ;;================;;
    ;; Reverses a tag ;;
    ;;================;;
    (define reverse-tag
      (lambda (x) 
	(cond ((eq? x 'diagul) 
	       'diagdr)
	      ((eq? x 'diagur) 
	       'diagdl)
	      ((eq? x 'diagdl) 
	       'diagur)
	      ((eq? x 'diagdr) 
	       'diagul)
	      ((eq? x 'horizl) 
	       'horizr)
	      ((eq? x 'horizr) 
	       'horizl)
	      ((eq? x 'vertu) 
	       'vertd)
	      ((eq? x 'vertd) 
	       'vertu))))   
    ;;=================;;
    ;; Creates a chain ;;
    ;;=======================================================;;
    ;;  Start-chain jumps forward two positions iteratively  ;;
    ;; Start-chain2 moves backwards one position iteratively ;; 
    ;;=======================================================;;
    (define (start-chain tag position board other-player)
      (define (helper row column inc1 inc2 prevrow prevcolumn)
	(if (validandopen? (make-position row column) board) 
	    (make-position row column)
	    (if (valid-taken-by-other (make-position row column) board other-player)
		(start-chain2 (reverse-tag tag) (make-position prevrow prevcolumn) board other-player)
		(if (validandtaken? (make-position row column) board)
		    (helper (inc1 row) (inc2 column) inc1 inc2 row column)
		    (start-chain2 (reverse-tag tag) (make-position prevrow prevcolumn) board other-player)))))
      (cond ((eq? tag 'diagul) 
	     (helper (get-row position) (get-column position) -2+ -2+ (get-row position) (get-column position)))
	    ((eq? tag 'diagur) 
	     (helper (get-row position) (get-column position) -2+ 2+ (get-row position) (get-column position)))
	    ((eq? tag 'diagdl) 
	     (helper (get-row position) (get-column position) 2+ -2+ (get-row position) (get-column position)))
	    ((eq? tag 'diagdr) 
	     (helper (get-row position) (get-column position) 2+ 2+ (get-row position) (get-column position)))
	    ((eq? tag 'horizl) 
	     (helper (get-row position) (get-column position) 0+ -2+ (get-row position) (get-column position)))
	    ((eq? tag 'horizr) 
	     (helper (get-row position) (get-column position) 0+ 2+ (get-row position) (get-column position)))
	    ((eq? tag 'vertu) 
	     (helper (get-row position) (get-column position) -2+ 0+ (get-row position) (get-column position)))
	    ((eq? tag 'vertd) 
	     (helper (get-row position) (get-column position) 2+ 0+ (get-row position) (get-column position)))
	    (else (get-random-valid-position board))))
    (define (start-chain2 tag position board other-player)
      (define (helper2 r c next1 next2)
	(if (validandopen? (make-position r c) board) 
	    (make-position r c)
	    (if (valid-taken-by-other (make-position r c) board other-player)
		(get-random-valid-position board)
		(if (validandtaken? (make-position r c) board)
		    (helper2 (next1 r) (next2 c) next1 next2)
		    (get-random-valid-position board)))))
      (cond ((eq? tag 'diagul) 
	     (helper2 (get-row position) (get-column position) -1+ -1+))
	    ((eq? tag 'diagur) 
	     (helper2 (get-row position) (get-column position) -1+ 1+))
	    ((eq? tag 'diagdl) 
	     (helper2 (get-row position) (get-column position) 1+ -1+))
	    ((eq? tag 'diagdr) 
	     (helper2 (get-row position) (get-column position) 1+ 1+))
	    ((eq? tag 'horizl) 
	     (helper2 (get-row position) (get-column position) 0+ -1+))
	    ((eq? tag 'horizr) 
	     (helper2 (get-row position) (get-column position) 0+ 1+))
	    ((eq? tag 'vertu) 
	     (helper2 (get-row position) (get-column position) -1+ 0+))
	    ((eq? tag 'vertd) 
	     (helper2 (get-row position) (get-column position) 1+ 0+))
	    (else (get-random-valid-position board))))
    ;;===============================================;;
    ;; Checks a direction to which a player can move ;;
    ;;      to create a 5 chain uninterrupted        ;;
    ;;===============================================;;
    (define (open-chain? position player board)
      (cond ((and (validopenowned?  
		   (make-position 
		    (get-row position)
		    (-1+ (get-column position))) player board)
		  (validopenowned?  
		   (make-position
		    (get-row position)
		    (-2+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (get-row position)
		    (-3+ (get-column position))) player board)
		  (validopenowned?
		   (make-position
		    (get-row position)
		    (-4+ (get-column position))) player board))
	     'horizl)
	    ((and (validopenowned?
		   (make-position
		    (get-row position)
		    (1+ (get-column position))) player board)
		  (validopenowned?
		   (make-position
		    (get-row position)
		    (2+ (get-column position))) player board)
		  (validopenowned?
		   (make-position
		    (get-row position)
		    (3+ (get-column position))) player board)
		  (validopenowned?
		   (make-position
		    (get-row position)
		    (4+ (get-column position))) player board))
	     'horizr)
	    ((and (validopenowned? 
		   (make-position 
		    (-1+ (get-row position))
		    (get-column position)) player board)
		  (validopenowned?
		   (make-position
		    (-2+ (get-row position))
		    (get-column position)) player board)
		  (validopenowned? 
		   (make-position
		    (-3+ (get-row position))
		    (get-column position)) player board)
		  (validopenowned?
		   (make-position
		    (-4+ (get-row position))
		    (get-column position)) player board))
	     'vertu)
	    ((and (validopenowned?
		   (make-position 
		    (1+ (get-row position))
		    (get-column position)) player board)
		  (validopenowned?
		   (make-position
		    (2+ (get-row position))
		    (get-column position)) player board)
		  (validopenowned?
		   (make-position
		    (3+ (get-row position))
		    (get-column position)) player board)
		  (validopenowned?
		   (make-position
		    (4+ (get-row position))
		    (get-column position)) player board))
	     'vertd)
	    ((and (validopenowned? 
		   (make-position 
		    (1+ (get-row position))
		    (1+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (2+ (get-row position))
		    (2+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (3+ (get-row position))
		    (3+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (4+ (get-row position))
		    (4+ (get-column position))) player board))
	     'diagdr)
	    ((and (validopenowned? 
		   (make-position 
		    (1+ (get-row position))
		    (-1+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (2+ (get-row position))
		    (-2+ (get-column position))) player board)
		  (validopenowned?  
		   (make-position
		    (3+ (get-row position))
		    (-3+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (4+ (get-row position))
		    (-4+ (get-column position))) player board))
	     'diagdl)
	    ((and (validopenowned? 
		   (make-position 
		    (-1+ (get-row position))
		    (1+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (-2+ (get-row position))
		    (2+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (-3+ (get-row position))
		    (3+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (-4+ (get-row position))
		    (4+ (get-column position))) player board))
	     'diagur)
	    ((and (validopenowned? 
		   (make-position 
		    (-1+ (get-row position))
		    (-1+ (get-column position))) player board)
		  (validopenowned?  
		   (make-position
		    (-2+ (get-row position))
		    (-2+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (-3+ (get-row position))
		    (-3+ (get-column position))) player board)
		  (validopenowned? 
		   (make-position
		    (-4+ (get-row position))
		    (-4+ (get-column position))) player board))
	     'diagul)
	    (else #f)))
    ;;============================;;
    ;; Cycles through a list and  ;;
    ;; checks which positions can ;;
    ;; create a winning chain or  ;; 
    ;;             not            ;;
    ;;============================;;
    (define (Worth-it? player board lst)
      (fast-filter (lambda (position)
		     (cond ((and (validopenowned? 
				  (make-position 
				   (1+ (get-row position))
				   (1+ (get-column position))) player board)
				 (validopenowned? 
				  (make-position
				   (2+ (get-row position))
				   (2+ (get-column position))) player board)
				 (or (and (validopenowned? 
					   (make-position
					    (3+ (get-row position))
					    (3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (4+ (get-row position))
					    (4+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (3+ (get-row position))
					    (3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (-1+ (get-row position))
					    (-1+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (-1+ (get-row position))
					    (-1+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (-2+ (get-row position))
					    (-2+ (get-column position))) player board))))
			    #t)
			   ((and (validopenowned? 
				  (make-position 
				   (1+ (get-row position))
				   (-1+ (get-column position))) player board)
				 (validopenowned? 
				  (make-position
				   (2+ (get-row position))
				   (-2+ (get-column position))) player board)
				 (or (and (validopenowned?  
					   (make-position
					    (3+ (get-row position))
					    (-3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (4+ (get-row position))
					    (-4+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (3+ (get-row position))
					    (-3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (-1+ (get-row position))
					    (1+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (-1+ (get-row position))
					    (1+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (-2+ (get-row position))
					    (2+ (get-column position))) player board))))
			    #t)
			   ((and (validopenowned? 
				  (make-position 
				   (-1+ (get-row position))
				   (1+ (get-column position))) player board)
				 (validopenowned? 
				  (make-position
				   (-2+ (get-row position))
				   (2+ (get-column position))) player board)
				 (or (and (validopenowned? 
					    (make-position
					     (-3+ (get-row position))
					     (3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (-4+ (get-row position))
					    (4+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (-3+ (get-row position))
					    (3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (1+ (get-row position))
					    (-1+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (1+ (get-row position))
					    (-1+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (2+ (get-row position))
					    (-2+ (get-column position))) player board))))
			    #t)
			   ((and (validopenowned? 
				  (make-position 
				   (-1+ (get-row position))
				   (-1+ (get-column position))) player board)
				 (validopenowned?  
				  (make-position
				   (-2+ (get-row position))
				   (-2+ (get-column position))) player board)
				 (or (and (validopenowned? 
					   (make-position
					    (-3+ (get-row position))
					    (-3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (-4+ (get-row position))
					    (-4+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (-3+ (get-row position))
					    (-3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (1+ (get-row position))
					    (1+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (1+ (get-row position))
					    (1+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (2+ (get-row position))
					    (2+ (get-column position))) player board))))
			    #t)
			   ((and (validopenowned?  
				  (make-position 
				   (get-row position)
				   (-1+ (get-column position))) player board)
				 (validopenowned?  
				  (make-position
				   (get-row position)
				   (-2+ (get-column position))) player board)
				 (or (and (validopenowned? 
					   (make-position
					    (get-row position)
					    (-3+ (get-column position))) player board)
					  (validopenowned?
					   (make-position
					    (get-row position)
					    (-4+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (get-row position)
					    (-3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (get-row position)
					    (1+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (get-row position)
					    (1+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (get-row position)
					    (2+ (get-column position))) player board))))
			    #t)
			   ((and (validopenowned?
				  (make-position
				   (get-row position)
				   (1+ (get-column position))) player board)
				 (validopenowned?
				  (make-position
				   (get-row position)
				   (2+ (get-column position))) player board)
				 (or (and (validopenowned?
					   (make-position
					    (get-row position)
					    (3+ (get-column position))) player board)
					  (validopenowned?
					   (make-position
					    (get-row position)
					    (4+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (get-row position)
					    (3+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (get-row position)
					    (-1+ (get-column position))) player board))
				     (and (validopenowned? 
					   (make-position
					    (get-row position)
					    (-1+ (get-column position))) player board)
					  (validopenowned? 
					   (make-position
					    (get-row position)
					    (-2+ (get-column position))) player board))))
			    #t)
			   ((and (validopenowned? 
				  (make-position 
				   (-1+ (get-row position))
				   (get-column position)) player board)
				 (validopenowned?
				  (make-position
				   (-2+ (get-row position))
				   (get-column position)) player board)
				 (or (and (validopenowned? 
					   (make-position
					    (-3+ (get-row position))
					    (get-column position)) player board)
					  (validopenowned?
					   (make-position
					    (-4+ (get-row position))
					    (get-column position)) player board))
				     (and (validopenowned? 
					   (make-position
					    (-3+ (get-row position))
					    (get-column position)) player board)
					  (validopenowned?
					   (make-position
					    (1+ (get-row position))
					    (get-column position)) player board))
				     (and (validopenowned? 
					   (make-position
					    (1+ (get-row position))
					    (get-column position)) player board)
					  (validopenowned?
					   (make-position
					    (2+ (get-row position))
					    (get-column position)) player board)))) 
			    #t)
			   ((and (validopenowned?
				  (make-position 
				   (1+ (get-row position))
				   (get-column position)) player board)
				 (validopenowned?
				  (make-position
				   (2+ (get-row position))
				   (get-column position)) player board)
				 (or (and (validopenowned?
					   (make-position
					    (3+ (get-row position))
					    (get-column position)) player board)
					  (validopenowned?
					   (make-position
					    (4+ (get-row position))
					    (get-column position)) player board))
				     (and (validopenowned? 
					   (make-position
					    (3+ (get-row position))
					    (get-column position)) player board)
					  (validopenowned?
					   (make-position
					    (-1+ (get-row position))
					    (get-column position)) player board))
				     (and (validopenowned? 
					   (make-position
					     (-1+ (get-row position))
					     (get-column position)) player board)
					  (validopenowned?
					   (make-position
					    (-2+ (get-row position))
					    (get-column position)) player board))))  
			    #t)
			   (else #f)))
		   lst))
    ;;======================================;;
    ;; Gives a list of open positions that, ;;
    ;;    if picked, could end the game.    ;;
    ;;======================================;;
    (define (4chain player board)
      (fast-filter (lambda (position) 
		     (cond ((and (validandowned? 
				  (make-position 
				   (1+ (get-row position))
				   (1+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (2+ (get-row position))
				   (2+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (3+ (get-row position))
				   (3+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (4+ (get-row position))
				   (4+ (get-column position))) player board))
			    #t)
			   ((and (validandowned? 
				  (make-position 
				   (1+ (get-row position))
				   (-1+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (2+ (get-row position))
				   (-2+ (get-column position))) player board)
				 (validandowned?  
				  (make-position
				   (3+ (get-row position))
				   (-3+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (4+ (get-row position))
				   (-4+ (get-column position))) player board))
			    #t)
			   ((and (validandowned? 
				  (make-position 
				   (-1+ (get-row position))
				   (1+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (-2+ (get-row position))
				   (2+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (-3+ (get-row position))
				   (3+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (-4+ (get-row position))
				   (4+ (get-column position))) player board))
			    #t)
			   ((and (validandowned? 
				  (make-position 
				   (-1+ (get-row position))
				   (-1+ (get-column position))) player board)
				 (validandowned?  
				  (make-position
				   (-2+ (get-row position))
				   (-2+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (-3+ (get-row position))
				   (-3+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (-4+ (get-row position))
				   (-4+ (get-column position))) player board))
			    #t)
			   ((and (validandowned?  
				  (make-position 
				   (get-row position)
				   (-1+ (get-column position))) player board)
				 (validandowned?  
				  (make-position
				   (get-row position)
				   (-2+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
				   (get-row position)
				   (-3+ (get-column position))) player board)
				 (validandowned?
				  (make-position
				   (get-row position)
				   (-4+ (get-column position))) player board))	    
			    #t)
			   ((and (validandowned?
				  (make-position
				   (get-row position)
				   (1+ (get-column position))) player board)
				 (validandowned?
				  (make-position
				   (get-row position)
				   (2+ (get-column position))) player board)
				 (validandowned?
				  (make-position
				   (get-row position)
				   (3+ (get-column position))) player board)
				 (validandowned?
				  (make-position
				   (get-row position)
				   (4+ (get-column position))) player board))
			    #t)
			   ((and (validandowned? 
				  (make-position 
				   (-1+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (-2+ (get-row position))
				   (get-column position)) player board)
				 (validandowned? 
				  (make-position
				   (-3+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (-4+ (get-row position))
				   (get-column position)) player board))
			    #t)
			   ((and (validandowned?
				  (make-position 
				   (1+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (2+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (3+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (4+ (get-row position))
				   (get-column position)) player board)) 
			    #t)
			   (else #f)))
		   (get-open-positions board)))
    ;;=================================;;
    ;; Gives a list of positions that, ;;
    ;; if picked, could end the game.  ;;
    ;;=================================;;
    (define (Hazard player board)
      (fast-filter (lambda (position)
		     (cond ((and (validandowned?
				  (make-position
				   (get-row position)
				   (1+ (get-column position))) player board)
				 (validandowned?
				  (make-position
				   (get-row position)
				   (2+ (get-column position))) player board)
				 (validandowned?
				  (make-position
				   (get-row position)
				   (-1+ (get-column position))) player board)
				 (validandowned?
				  (make-position
				   (get-row position)
				   (-2+ (get-column position))) player board))	
			    #t)
			   ((and (validandowned?
				  (make-position 
				   (1+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (2+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (-1+ (get-row position))
				   (get-column position)) player board)
				 (validandowned?
				  (make-position
				   (-2+ (get-row position))
				   (get-column position)) player board))
			    #t)
			   ((and (validandowned? 
				  (make-position 
				   (1+ (get-row position))
				   (1+ (get-column position))) player board)
				 (validandowned? 
			      (make-position
			       (2+ (get-row position))
			       (2+ (get-column position))) player board)
				 (validandowned? 
			      (make-position
			       (-1+ (get-row position))
			       (-1+ (get-column position))) player board)
				 (validandowned? 
				  (make-position
			       (-2+ (get-row position))
			       (-2+ (get-column position))) player board))
			    #t)
		       ((and (validandowned? 
			      (make-position 
			       (1+ (get-row position))
			       (-1+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (2+ (get-row position))
			       (-2+ (get-column position))) player board)
			     (validandowned?  
			      (make-position
			       (-1+ (get-row position))
			       (1+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (-2+ (get-row position))
			       (2+ (get-column position))) player board))
			#t)	    
		       ((and (validandowned? 
			      (make-position 
			       (1+ (get-row position))
			       (1+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (-1+ (get-row position))
			       (-1+ (get-column position))) player board)
			     (or (and (validandowned? 
				       (make-position
					(-2+ (get-row position))
					(-2+ (get-column position))) player board)
				      (validandowned? 
				       (make-position
					(-3+ (get-row position))
					(-3+ (get-column position))) player board))
				 (and (validandowned? 
				       (make-position
					(2+ (get-row position))
					(2+ (get-column position))) player board)
				      (validandowned? 
				       (make-position
					(3+ (get-row position))
					(3+ (get-column position))) player board))))		      
			#t)
		       ((and (validandowned? 
			      (make-position 
			       (1+ (get-row position))
			       (-1+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (-1+ (get-row position))
			       (1+ (get-column position))) player board)
			     (or (and (validandowned? 
				       (make-position
					(-2+ (get-row position))
					(2+ (get-column position))) player board)
				      (validandowned? 
				       (make-position
					(-3+ (get-row position))
					(3+ (get-column position))) player board))
				 (and (validandowned? 
				       (make-position
					(2+ (get-row position))
					(-2+ (get-column position))) player board)
				      (validandowned? 
				       (make-position
					(3+ (get-row position))
					(-3+ (get-column position))) player board))))
			#t)
		       ((and (validandowned?
			      (make-position
			       (get-row position)
			       (1+ (get-column position))) player board)
			     (validandowned?
			      (make-position
			       (get-row position)
			       (-1+ (get-column position))) player board)
			     (or (and (validandowned? 
				       (make-position
					(get-row position)
					(-2+ (get-column position))) player board)
				      (validandowned? 
				       (make-position
					(get-row position)
					(-3+ (get-column position))) player board))
				 (and (validandowned? 
				       (make-position
					(get-row position)
					(2+ (get-column position))) player board)
				      (validandowned? 
				       (make-position
					(get-row position)
					(3+ (get-column position))) player board))))
			
			#t)
		       ((and (validandowned?
			      (make-position 
			       (1+ (get-row position))
			       (get-column position)) player board)
			     (validandowned?
			      (make-position
			       (-1+ (get-row position))
			       (get-column position)) player board)
			     (or (and (validandowned? 
				       (make-position
					(2+ (get-row position))
					(get-column position)) player board)
				      (validandowned? 
				       (make-position
					(3+ (get-row position))
					(get-column position)) player board))
				 (and (validandowned? 
				       (make-position
					(-2+ (get-row position))
					(get-column position)) player board)
				      (validandowned? 
				       (make-position
					(-3+ (get-row position))
					(get-column position)) player board))))
			#t)
		       (else #f)))
		   (get-open-positions board)))
    ;;========================================;;
    ;; Creates a list of possibly game-ending ;; 
    ;;               3chain                   ;;
    ;;========================================;;
    (define (Hazard2 player board)
      (filter (lambda (position)
		(cond ((and (validandowned? 
			     (make-position 
			      (1+ (get-row position))
			      (get-column position)) player board)
			    (validandowned?  
			     (make-position
			      (-1+ (get-row position))
			      (get-column position)) player board)
			    (or (and (validandowned? 
				      (make-position 
				       (2+ (get-row position))
				       (get-column position)) player board)
				     (validandopen? 
				      (make-position
				       (3+ (get-row position))
				       (get-column position)) board)
				     (validandopen? 
				      (make-position
				       (-2+ (get-row position))
				       (get-column position)) board))
				(and (validandowned?  
				      (make-position
				       (-2+ (get-row position))
				       (get-column position)) player board)
				     (validandopen? 
				      (make-position
				       (-3+ (get-row position))
				       (get-column position)) board)
				     (validandopen? 
				      (make-position
				       (2+ (get-row position))
				       (get-column position)) board))))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (get-row position)
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (get-row position)
			      (-1+ (get-column position))) player board)
			    (or (and (validandowned? 
				      (make-position 
				       (get-row position)
				       (2+ (get-column position))) player board)
				     (validandopen? 
				      (make-position
				       (get-row position)
				       (3+ (get-column position))) board)
				     (validandopen? 
				      (make-position
				       (get-row position)
				       (-2+ (get-column position))) board))
				(and (validandowned?  
				      (make-position
				       (get-row position)
				       (-2+ (get-column position))) player board)	
				     (validandopen? 
				      (make-position
				       (get-row position)
				       (-3+ (get-column position))) board)
				     (validandopen? 
				      (make-position
				       (get-row position)
				       (2+ (get-column position))) board))))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (-1+ (get-row position))
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (1+ (get-row position))
			      (-1+ (get-column position))) player board)
			    (or (and (validandowned? 
				      (make-position 
				       (-2+ (get-row position))
				       (2+ (get-column position))) player board)
				     (validandopen? 
				      (make-position
				       (-3+ (get-row position))
				       (3+ (get-column position))) board)
				     (validandopen? 
				      (make-position
				       (2+ (get-row position))
				       (-2+ (get-column position))) board))
				(and (validandowned?  
				      (make-position
				       (2+ (get-row position))
				       (-2+ (get-column position))) player board)
				     (validandopen? 
				      (make-position
				       (3+ (get-row position))
				       (-3+ (get-column position))) board)
				     (validandopen? 
				      (make-position
				       (-2+ (get-row position))
				       (2+ (get-column position))) board))))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (1+ (get-row position))
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (-1+ (get-row position))
			      (-1+ (get-column position))) player board)
			    (or (and (validandowned? 
				      (make-position 
				       (2+ (get-row position))
				       (2+ (get-column position))) player board)
				     (validandopen? 
				      (make-position
				       (3+ (get-row position))
				       (3+ (get-column position))) board)
				     (validandopen? 
				      (make-position
				       (-2+ (get-row position))
				       (-2+ (get-column position))) board))
				(and (validandowned?  
				      (make-position
				       (-2+ (get-row position))
				       (-2+ (get-column position))) player board)
				     (validandopen? 
				      (make-position
				       (-3+ (get-row position))
				       (-3+ (get-column position))) board)
				     (validandopen? 
				      (make-position
				       (2+ (get-row position))
				       (2+ (get-column position))) board))))
		       
		       #t)
		      (else #f)))
	      (get-open-positions board)))
    ;;=====================================;;
    ;; Gives a list of positions that are  ;;
    ;; one side of a two sided, open three ;;
    ;;              chain.                 ;;
    ;;=====================================;;
    (define (3chain player board)
      (filter (lambda (position) 
		(cond ((and (validandowned? 
			     (make-position 
			      (get-row position)
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (get-row position)
			      (2+ (get-column position))) player board)
			    (validandowned? 
			      (make-position
			       (get-row position)
			       (3+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (get-row position)
			      (4+ (get-column position))) board)
			    (validandopen? 
			     (make-position
			      (get-row position)
			      (-1+ (get-column position))) board))
		       #t)
		      ((and (validandowned? 
			     (make-position 
			      (1+ (get-row position))
			      (get-column position)) player board)
			    (validandowned?  
			     (make-position
			      (2+ (get-row position))
			      (get-column position)) player board)
			    (validandowned? 
			     (make-position
			      (3+ (get-row position))
			      (get-column position)) player board)
			    (validandopen? 
			     (make-position
			      (4+ (get-row position))
			      (get-column position)) board)
			    (validandopen? 
			     (make-position
			      (-1+ (get-row position))
			      (get-column position)) board))
		       #t)
		      ((and (validandowned? 
			     (make-position 
			      (-1+ (get-row position))
			      (-1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (-2+ (get-row position))
			      (-2+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (-3+ (get-row position))
			      (-3+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (-4+ (get-row position))
			      (-4+ (get-column position))) board)
			    (validandopen? 
			     (make-position
			      (1+ (get-row position))
			      (1+ (get-column position))) board))
		       #t)
		      ((and (validandowned? 
			     (make-position 
			      (-1+ (get-row position))
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (-2+ (get-row position))
			      (2+ (get-column position))) player board)
			    (validandowned?  
			     (make-position
			      (-3+ (get-row position))
			      (3+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (-4+ (get-row position))
			      (4+ (get-column position))) board)
			    (validandopen? 
			     (make-position
			      (1+ (get-row position))
			      (-1+ (get-column position))) board))
		       #t)
		       ((and (validandowned? 
			      (make-position 
			       (get-row position)
			       (-1+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (get-row position)
			       (-2+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (get-row position)
			       (-3+ (get-column position))) player board)
			     (validandopen? 
			      (make-position
			       (get-row position)
			       (-4+ (get-column position))) board)
			     (validandopen? 
			      (make-position
			       (get-row position)
			       (1+ (get-column position))) board))
			#t)
		       ((and (validandowned? 
			      (make-position 
			       (1+ (get-row position))
			       (1+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (2+ (get-row position))
			       (2+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (3+ (get-row position))
			       (3+ (get-column position))) player board)
			     (validandopen? 
			      (make-position
			       (4+ (get-row position))
			       (4+ (get-column position))) board)
			     (validandopen? 
			      (make-position
			       (-1+ (get-row position))
			       (-1+ (get-column position))) board))
			#t)
		       ((and (validandowned? 
			      (make-position 
			       (1+ (get-row position))
			       (-1+ (get-column position))) player board)
			     (validandowned? 
			      (make-position
			       (2+ (get-row position))
			       (-2+ (get-column position))) player board)
			     (validandowned?  
			      (make-position
			       (3+ (get-row position))
			       (-3+ (get-column position))) player board)
			     (validandopen? 
			      (make-position
			       (4+ (get-row position))
			       (-4+ (get-column position))) board)
			     (validandopen? 
			      (make-position
			       (-1+ (get-row position))
			       (1+ (get-column position))) board))
			#t)
		       ((and (validandowned? 
			      (make-position 
			       (-1+ (get-row position))
			       (get-column position)) player board)
			     (validandowned?  
			      (make-position
			       (-2+ (get-row position))
			       (get-column position)) player board)
			     (validandowned? 
			      (make-position
			       (-3+ (get-row position))
			       (get-column position)) player board)
			     (validandopen? 
			      (make-position
			       (-4+ (get-row position))
			       (get-column position)) board)
			     (validandopen? 
			      (make-position
			       (1+ (get-row position))
			       (get-column position)) board))
			#t)
		       (else #f)))
	      (get-open-positions board)))   
    ;;=============================================;;
    ;; List of open positions in between two taken ;;
    ;;                 positions                   ;;
    ;;=============================================;;
    (define (hole-filler player board)
      (filter (lambda (position)
		(cond ((and (validandowned? 
			     (make-position 
			      (1+ (get-row position))
			      (get-column position)) player board)
			    (validandowned?  
			     (make-position
			      (-1+ (get-row position))
			      (get-column position)) player board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (get-row position)
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (get-row position)
			      (-1+ (get-column position))) player board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (-1+ (get-row position))
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (1+ (get-row position))
			      (-1+ (get-column position))) player board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (1+ (get-row position))
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (-1+ (get-row position))
			      (-1+ (get-column position))) player board))
		       #t)
		      (else #f)))
	      (reverse (get-open-positions board))))
    ;;==================================;;
    ;; List of open ended chains of two ;;
    ;;==================================;;
    (define (2chain player board)
      (filter (lambda (position)
		(cond ((and (validandowned? 
			     (make-position 
			      (1+ (get-row position))
			      (get-column position)) player board)
			    (validandowned?  
			     (make-position
			      (2+ (get-row position))
			      (get-column position)) player board)
			    (validandopen? 
			     (make-position
			      (3+ (get-row position))
			      (get-column position)) board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (get-row position)
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (get-row position)
			      (2+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (get-row position)
			      (3+ (get-column position))) board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (-1+ (get-row position))
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (-2+ (get-row position))
			      (2+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (-3+ (get-row position))
			      (3+ (get-column position))) board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (1+ (get-row position))
			      (1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (2+ (get-row position))
			      (2+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (3+ (get-row position))
			      (3+ (get-column position))) board))
		       #t)
		      ((and (validandowned? 
			     (make-position 
			      (-1+ (get-row position))
			      (get-column position)) player board)
			    (validandowned?  
			     (make-position
			      (-2+ (get-row position))
			      (get-column position)) player board)
			    (validandopen? 
			     (make-position
			      (-3+ (get-row position))
			      (get-column position)) board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (get-row position)
			      (-1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (get-row position)
			      (-2+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (get-row position)
			      (-3+ (get-column position))) board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (1+ (get-row position))
			      (-1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (2+ (get-row position))
			      (-2+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (3+ (get-row position))
			      (-3+ (get-column position))) board))
		       #t)
		      ((and (validandowned? 
			     (make-position
			      (-1+ (get-row position))
			      (-1+ (get-column position))) player board)
			    (validandowned? 
			     (make-position
			      (-2+ (get-row position))
			      (-2+ (get-column position))) player board)
			    (validandopen? 
			     (make-position
			      (-3+ (get-row position))
			      (-3+ (get-column position))) board))
		       #t)
		      (else #f)))
	      (get-open-positions board)))
    ;;=================;;
    ;; Creates a chain ;;
    ;;=================;;
    (define (create-chain player other-player board)
      (define (helper a)
	(if (null? a)
	    (get-random-valid-position board)
	    (if (eq? (open-chain? (car a) player board) #f)
		(helper (cdr a))
		(start-chain (open-chain? (car a) player board) (car a) board other-player)
		)
	    )
	)
      (helper (reverse (get-owned-positions player board)))
      )      
    ;;==============;;
    ;; Optimization ;;
    ;;=======================================================;;
    ;;  Fast-filter stops after the predicate is met once.   ;;
    ;;   For some of the procedures, this comes in handy,    ;;
    ;; since theres no need for more than that one position. ;;
    ;;=======================================================;;
    (define (fast-filter predicate sequence)
      (cond ((null? sequence) ())
	    ((predicate (car sequence))
	     (list (car sequence)))
	    (else (fast-filter predicate (cdr sequence)))))
    ;;===========================;;
    ;; Simplification Procedures ;;
    ;;===========================;;
    (define (get-owned-positions player board)
      (filter (lambda (position) (owns-position? player position board))
	      (enumerate-positions)))
    (define (make-move position)
      (make-position (get-row position) (get-column position)))
    (define (end-game player board)
      (append (4chain player board) (Hazard player board)))
    ;;====================;;
    ;; Get-Move Procedure ;;
    ;;===============================================================;;
    ;; This is the procedure called by gomoku++.scm to get your move.;;
    ;; Returns a position object.                                    ;;
    ;;===============================================================;;
    (define (get-move player board)
      (define them (if (eq? player *PLAYER-1-SYMBOL*) 2 1))
      (define us (if (eq? player *PLAYER-1-SYMBOL*) 1 2))
      (cond ((pair? (end-game us board))
	     (make-move (car (end-game us board))))
	    ((pair? (end-game them board))
	     (make-move (car (end-game them board))))
	    ((pair? (Worth-it? us board (Hazard2 us board)))
	     (make-move (car (Worth-it? us board (Hazard2 us board)))))
	    ((pair? (Worth-it? them board (Hazard2 them board)))
	     (make-move (car (Worth-it? them board (Hazard2 them board)))))
	    ((pair? (Worth-it? them board (3chain us board)))
	     (make-move (car (Worth-it? them board (3chain us board)))))
	    ((pair? (Worth-it? us board (3chain us board)))
	     (make-move (car (Worth-it? us board (3chain us board)))))
	    ((pair? (Worth-it? them board (3chain them board)))
	     (make-move (car (Worth-it? them board (3chain them board)))))
	    ((pair? (Worth-it? them board (2chain us board)))
	     (make-move (car (Worth-it? them board (2chain us board)))))
	    ((pair? (Worth-it? us board (2chain us board)))
	     (make-move (car (Worth-it? us board (2chain us board)))))
	    ((pair? (Worth-it? them board (2chain them board)))
	     (make-move (car (Worth-it? them board (2chain them board)))))
	    ((pair? (Worth-it? them board (hole-filler us board)))
	     (make-move (car (Worth-it? them board (hole-filler us board)))))
	    ((pair? (Worth-it? us board (hole-filler us board)))
	     (make-move (car (Worth-it? us board (hole-filler us board)))))
	    ((pair? (Worth-it? them board (hole-filler them board)))
	     (make-move (car (Worth-it? them board (hole-filler them board)))))
	    (else (create-chain us them board))))
     ;; Return get-move procedure
    get-move
    )) ;; End of player-procedure
