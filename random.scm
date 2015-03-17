;;;; CSci 1901 Project - Spring 2013
;;;; gomoku++ Player AI

;;;======================;;;
;;;  SUBMISSION DETAILS  ;;;
;;;======================;;;

;; List both partners' information below.
;; Leave the second list blank if you worked alone.
(define authors 
  '((
     ""   ;; Author 1 Name
     ""   ;; Author 1 X500
     ""   ;; Author 1 ID
     ""   ;; Author 1 Section
     )
    (
     ""   ;; Author 2 Name
     ""   ;; Author 2 X500
     ""   ;; Author 2 ID
     ""   ;; Author 2 Section
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

    ;;====================;;
    ;; Get-Move Procedure ;;
    ;;===============================================================;;
    ;; This is the procedure called by gomoku++.scm to get your move.;;
    ;; Returns a position object.
    ;;===============================================================;;

    (define (get-move player board)
      (get-random-valid-position board))


    ;; Return get-move procedure
    get-move

    )) ;; End of player-procedure
    
