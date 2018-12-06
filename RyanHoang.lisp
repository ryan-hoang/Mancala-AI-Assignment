;;;;Ryan Hoang 
;;;;CS480 Fall 2018
;;;;Assignment 5

(defpackage :RyanHoang
   (:use :common-lisp-user :common-lisp)
   (:export computer-make-move))
(in-package :RyanHoang)

;;;; Here is a description of the stuff that would go into your
;;;; file.

;;;; The first thing in your function is a package declaration.
;;;; You should name your package something beginning with a colon
;;;; followed by your full name with hyphens for spaces.
;;;; I named my package :sean-luke .  Keep in mind that the name
;;;; is CASE-INSENSITIVE.  The only symbol you should export is the
;;;; symbol COMPUTER-MAKE-MOVE, which should be the name of your top-
;;;; level computer move function.  Name your file the same
;;;; name as your package declaration, minus the colon.  For example,
;;;; my file is named "sean-luke.lisp"

;; (defpackage :sean-luke
;;   (:use :common-lisp-user :common-lisp)
;;   (:export computer-make-move))
;; (in-package :sean-luke)


;;Taken from the previous assignment's utilities.lisp to simplify the code and make it easier to read.
(defmacro for-each (var in list do &body body)
  "Execute body for each element of list.  VAR can be a list or tree
  of variables, in which case the elements are destructured."
  (assert (eq in 'in)) (assert (eq do 'do))
  (typecase var
    (symbol `(dolist (,var ,list) ,@body))
    (cons (let ((list-var (gensym)))
	    `(dolist (,list-var ,list)
	       (destructuring-bind ,var ,list-var ,@body))))
    (t (error "~V is an illegal variable in (for each ~V in ~A ...)"
	      var list))))


;;Helper         
(defun maxturnp (s max-player)
  (if (equalp max-player (state-turn s)) T Nil))        


;;;; Once you've done this, you need to write your code.  Here
;;;; is a rough sketch of three functions you will find handy.
;;;; You don't need to implement them like this, except for the
;;;; COMPUTER-MAKE-MOVE function. You can write your code in
;;;; any fashion you like in this file, so long as you do a
;;;; proper alpha-beta heuristic search and your evaluation
;;;; function is stronger than just comparing the differences in
;;;; the mancalas.

(defun alpha-beta (state depth maxdepth maxplayer expand terminal evaluate alpha beta)
 "Does alpha-beta search.  Note that there is the addition of
a variable called MAX-PLAYER rather than a function which specifies
if it's max's turn.  It's just more convenient in this system.
The MAX-PLAYER variable is set to either *player-1*
or to *player-2* and indicates if *player-1* or *player-2* should
be considered 'max' (the other player is then considered to be 'min')"
  (let* 
    ((children (funcall expand state)))
    (if (or (funcall terminal state) (>= depth maxdepth)) (return-from alpha-beta (funcall evaluate state maxplayer)))
    (if (maxturnp state maxplayer)
        (for-each child in children do 
            (let*
              (
                (mm (alpha-beta child (+ 1 depth) maxdepth maxplayer expand terminal evaluate alpha beta))
                (a (max alpha mm))
              )
              (if (>= a beta) (return-from alpha-beta beta) (return-from alpha-beta a))
            )
        )
        (for-each child in children do 
            (let*
              (
                (mm (alpha-beta child (+ 1 depth) maxdepth maxplayer expand terminal evaluate alpha beta))
                (b (min beta mm))
              )
              (if (>= alpha b) (return-from alpha-beta alpha) (return-from alpha-beta b))
            )
        )
    )
  )
)


;;score+relative_score*.03

(defun evaluate (state max-player)
 "Evaluates the game situation for MAX-PLAYER.
Returns the value of STATE for MAX-PLAYER (who
is either *player-1* or *player-2*).  This should
be a value ranging from *min-wins* to *max-wins*."
(return-from evaluate (+ (score state) (* 0.3 (- (score state max-player) (score state (other-player max-player))))))
)

(defun computer-make-move (state max-depth)
 "Given a state, makes a move and returns the new state.
If there is no move to make (end of game) returns nil.
Each time this function calls the top-level
alpha-beta function to search for the quality of a state,
computer-make-move should print out the state (using PRINT,
not PRINT-STATE) that is being searched.
Only search up to max-depth.  The computer should assume
that he is the player who's turn it is right now in STATE"
  (let*
      (
        (current-turn (state-turn state))
        (possible-moves (moves state))
      )
      (if (game-overp state) 
          (return-from computer-make-move Nil)
          (for-each move in possible-moves do 
              
            
          )
      
      
      
      
      )
  )
)

;;;; In comments your file, you put your project notes.

;;;; The last thing in your file should be this line (uncommented
;;;; of course).

(in-package :cl-user)
