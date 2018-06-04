;;;; 
;;;; Kandidatnummer: 182249
;;;; Dato 01.12.09
;;;; Emnekode: DASP101
;;;; 

;;;; 
;;;; Oppgaven bestar av et othellospill hvor brukeren kan velge a spille mot maskinen,
;;;; mot et annet menneske, eller a se pa maskinen spille mot meg selv.
;;;; Programmet bruker et alfabetasok for a finne best trekk for maskinen.
;;;; Programmet er sterkt nok til a sla meg deler av tiden pa normal vansklighetsgrad, og burde kunne
;;;; sla en som ikke har spilt mye othello. Styrken til programmet kan endres
;;;; ved a endre pa hvor hoyt *nc* kan ga i iterative-deepening funksjonen.
;;;; Nivaet dybden ligger pa i normal gir en grei balanse mellom spillstyrke,
;;;; og utfordring.

;;;; Spillet ligger i pakken othello og startes ved hjelp av (start-game) funksjonen

;;;; Skriv inn:
;;;; (in-package othello)
;;;; (start-game)


 (defpackage :othello
   (:use :common-lisp))
 (in-package :othello)

;;; Et brett bestar av et brett med 64 tiles som enten har eller ikke har en brikke pa seg, 
;;; en spiller som er i trekket, en liste av lovlige trekk pa brettet,
;;; og alfa-beta verdien til brettet

(defclass board ()
  ((board :initarg :br :initform 'Empty  :accessor board)
   (possiblemoves :initform 'tomM :accessor moves)
   (player-in-move :initarg :pl :accessor player)
   (alpha-beta-value :initform 'tomABS :accessor value)))

(defclass tile ()
  ((board     :initarg :br :accessor board)
   (value     :initform 2 :reader value)
   (x-pos     :initarg :x :reader x-pos)
   (y-pos     :initarg :y :reader y-pos)
   (piece     :initarg :pc :accessor piece)))

(defclass player ()
  ((name :initarg :nm :accessor name)
   (color :initarg :cl :reader color)
   (comp :initarg :cp :initform t :accessor comp)))


;;; Spillet gar fremover ved at en ny lovlig tilstand blir sendt til main-gameloopen.
;;; Mulige trekk blir undersokt, og lovlige tilstander blir lagret.

(defclass legal-move? ()
  ((move    :initarg :mv :accessor move)
   (from    :initarg :fr :accessor from)
   (start   :initarg :st :reader start)
   (end     :initarg :en :accessor end)))

(defclass legal-move ()
  ((board           :initarg :br :reader board)
   (move            :initarg :mv :reader move)
   (sboard :initarg :sb :initform '() :accessor sboard)))

(defclass takes ()
  ((move  :initarg :mv :accessor move)
   (start :initarg :st :reader start)
   (on    :initarg :on :accessor on)
   (take  :initarg :tk :accessor take)))

(defparameter *main* ())
(defparameter *x* ())
(defparameter *o* ())
(defparameter *players* '())
(defparameter *exp* 0)
(defparameter *nc* 0)
(defparameter *vanskelighetsgrad* 50)

(defmacro def-move (name y-check x-check func1 func2 new-y new-x)
 `(defun ,name (tile)
    (if (or (equal (y-pos tile) ,y-check) (equal (x-pos tile) ,x-check)) nil
  (get-tile (,func1 (y-pos tile) ,new-y) (,func2 (x-pos tile) ,new-x) (board tile)))))
(defparameter *moves* (list   
		      (def-move get-yx- 1 1 - - 1 1)
		      (def-move get-y- 1 nil - + 1 0)
		      (def-move get-yx+ 1 8 - + 1 1)
		      (def-move get-x- nil 1 + - 0 1)
		      (def-move get-x+ nil 8 + + 0 1)
		      (def-move get-xy- 8 1 + - 1 1)
		      (def-move get-y+ 8 nil + + 1 0)
		      (def-move get-xy+ 8 8 + + 1 1))) 

;;; Hovedfunksjonen i spillet.

(defun start-game ()
  "Starter spillet, og gir beskjed om hvem som til slutt vinner"
  (start-board)
  (start-spillere)
  (let ((slutt (main-gameloop *main*)))
    (format t "~A" slutt)
    (cond ((equal (length (get-o-pieces slutt)) (length (get-x-pieces slutt)))
	   (format t "Begge spillere har ~A brikker~%Spillet slutter uavgjort" (length (get-x-pieces slutt))))
	  (t 
	   (format t "Nr. brikker ~A:~A ~%Nr. brikker ~A:~A~%Vinneren er ~A"
		   (name *o*)
		   (length (get-o-pieces slutt))
		   (name *x*)
		   (length (get-x-pieces slutt)) 
		   (if (> (length (get-x-pieces slutt)) (length (get-o-pieces slutt))) (name *x*) (name *o*)))))))




;;; Lager det forste brettet

(defun start-board ()
  "Setter spillet til startposisjonen"

  (setf *x* (make-instance 'player
			     :nm 'Max
			     :cl 1))
  (setf *o* (make-instance 'player
			     :nm 'Min
			     :cl 0))
  (setf *players* (list *x* *o*))
  
  (setf *main* (make-instance 'board
			      :pl *x*))
  
  (setf (board *main*) (make-board *main*))
  
  (place-piece 4 4 1 *main*)
  (place-piece 5 5 1 *main*)
  (place-piece 4 5 0 *main*)
  (place-piece 5 4 0 *main*)
  (successors *main*)
  t)

;;; Folgende funksjoner henter input fra brukeren og definerer spilltype

(defun start-spillere ()
  "Brukeren velger spilltype og definerer spillerene"
  (let ((typespill (get-input 
		     "Velg et spill: 
1) Menneske vs Maskin
2) Menneske vs Menneske
3) Maskin vs Maskin
")))
    (cond ((equal typespill 1)
	   (loop
	      (let ((farge (get-input "Vil du spille som X eller O? X starter spillet.
1) X
2) O
"))) 
		(cond ((equal farge 1)
		       (let ((navn (get-input "Hva heter du?
Skriv inn navn:  ")))
			 (setf (name *x*) navn)
			 (setf (comp *x*) nil))
		       (setf (name *o*) 'Maskinen)
		       (setf (comp *o*) t)
		       (sett-vanskelighetsgrad)
		       (return))
		      ((equal farge 2)
		    (let ((navn (get-input "Hva heter du?
Skriv inn navn:  ")))
		    (setf (name *x*)'Maskinen)
		    (setf (comp *x*) t)
		    (setf (name *o*) navn)
		    (setf (comp *o*) nil)
		    (sett-vanskelighetsgrad)
		    (return)))))))
	  ((equal typespill 2)
	   (let ((navn1 (get-input "Hva heter spilleren som spiller X? X starter spillet.
Skriv inn navn:  ")) 
		 (navn2 (get-input "Hva heter spilleren som spiller O
Skriv inn navn:  ")))
	     (setf (name *x*) navn1)
	     (setf (comp *x*) nil)
	     (setf (name *o*) navn2)
	     (setf (comp *o*) nil)))
	  ((equal typespill 3)
	   (setf *x* (make-instance 'player
				    :nm 'Max
				    :cp t))
	   (setf *o* (make-instance 'player
				    :nm 'Min
				    :cp t))
	   (sett-vanskelighetsgrad))
	  (t
	   (format t "Skriv inn ett tall( 1, 2 eller 3) og angi spilltype~%")
	   (start-spillere)))))

(defun get-input (streng)
  (format t "~A" streng)
  (read))


;;; Denne funksjon setter vansklighetsgraden pa spillet

(defun sett-vanskelighetsgrad ()
  (loop 
     (let ((styrke (get-input "Velg spillstyrken til maskinen:
(Sterkere maskin tar lengre tid)
1) Lett
2) Normal
")))
(cond ((equal styrke 1)
       (setf *vanskelighetsgrad* 50)
       (return))
      ((equal styrke 2)
       (setf *vanskelighetsgrad* 1500)
       (return))
      (t (format t "Velg et av tallene fra listen.~%"))))))




(defun main-gameloop (board)
  "Tar inn et brett, og kaller seg selv med et nytt brett fra listen over lovlige trekk 
inntill spillet er i en maltilstand"
  (format t "~A" board)
  (force-output)
  (if (not (typep board 'board))
      (format t "~%Dette var litt pinlig... Det skjedde noe feil og vi fikk inn en slik ~a istedet for et brett" board )
	 (cond
	   ((goal-test board)
	    board)
	   ((and (comp (player board)) (listp (moves board)) (> (length (moves board)) 0)) ;Kaller maskinens trekk
	    (format t "~A velger sitt trekk~%" (name (player board)))
	    (force-output)
	    (main-gameloop (comp-move board)))
	   ((>(length (moves board)) 0) ;Henter inn trekk fra bruker
	    (format t "Spiller er ~a ~%Velg et trekk ~%~{~{~a~}~%~}" (name (player board)) (number-moves (moves board)))
	    (let ((x (read)))
	      (loop 
		 (if (numberp x)
		     (if (and (> x 0) (< x (1+ (length (moves board))))) (return)))
		 (format t "Velg et trekk fra listen ved aa skrive inn tallet ved siden av trekket~%")
		 (setf x (read)))
		     (successors (sboard(elt (moves board) (1- x))))
		     (main-gameloop (sboard (elt (moves board) (1- x))))))
	   (t ;Bytter spiller i trekket viss spilleren i trekket ikke har lovlige trekk
	    (format t "~A har ingen lovlige trekk og mister sitt trekk~%" (name (player board)))
	    (main-gameloop (switch-player board))))))

(defun goal-test (board)
  "Returnerer T viss brettet er i en sluttilstand"
  (and (not (successors board))
       (not (successors (switch-player board)))))

(defun number-moves (l)
  "Returnerer hvor mange elementer det er i listen"
  (let ((n 0))
    (mapcar #'(lambda (m) (list (setf n (1+ n)) m)) l)))

;;; Funksjonene under lager listen over lovlige trekk/successor-tilstander

(defun successors (board)
  "Returnerer alle lovlige successorstates til et brett"
  (setf (moves board) (mapcar #'(lambda (move) 
				  (make-instance 'legal-move
						 :br board
						 :mv move))
			      (remove-duplicates (flatten (check-moves (all-possible-successor-moves board)))))))


(defun check-moves (mt-pairs)
  "Returnerer en liste over lovlige trekk"
  (mapcar #'(lambda (mt-pair) (lsearch (make-instance 'legal-move?
					     :mv (first mt-pair)
					     :fr (second mt-pair)
					     :st (second mt-pair)
					     :en '())
				       :test #'test-move
				       :next #'next-move
				       :take #'end))
	  mt-pairs))

(defun test-move (move)
  "Returnerer T viss tilstanden er en lovlig sluttilstand"
  (or
   (if (has-piecep (funcall (move move) (from move)))
       (not (eq (piece (start move)) (piece (funcall (move move) (from move))))))
   (end move)))

(defun next-move (move)
  "Returnerer neste legal-move? tilstand"
  (cond ((null (from move)) nil)
	((funcall (move move) (from move))
	     (make-instance 'legal-move?
			    :mv (move move)
			    :fr (funcall (move move) (from move))
			    :st (start move)
			    :en (if (empty-tile (funcall (move move) (from move)))
				    (funcall (move move) (from move))
				    nil)))
	(t nil)))


(defun all-possible-successor-moves (board)
  "Returnerer en liste over alle mulige trekk"
  (group (flatten (mapcar #'(lambda (tile) (possible-successor-moves tile))
	 (if (equal (color (player board)) 1) (get-o-pieces board)
	     (get-x-pieces board)))) 2))


;;; Funksjonene under brukes til a hente inn de brikkene som skal snus
(defun get-turnable-pieces (tile)
  "Returnerer en liste av lister over alle brikker som kan snus"
  (mapcar #'(lambda (mt-pair) 
	      (lsearch 
	       (make-instance 'takes
			      :mv (elt mt-pair 0)
			      :st (elt mt-pair 1)
			      :on (elt mt-pair 1)
			      :tk '())
	       :test #'test-take
	       :next #'next-take 
	       :take #'take))
	  (group (flatten (possible-take-moves tile)) 2)))

(defun test-take (take)
  "Returnerer T viss start brikken og brikken en kommer til har samme farge"
  (eq (piece (start take)) (has-piecep (funcall (move take) (on take)))))

(defun next-take (take)
 "Returnerer neste lovlige take tilstand" 
  (cond 
    ((or (null take)
	 (not (funcall (move take) (on take)))
	 (empty-tile (funcall (move take) (on take))))
     nil)
    (t (make-instance 'takes
		      :mv (move take)
		      :st (start take)
		      :on (funcall (move take) (on take))
		      :tk (append (list (funcall (move take)(on take))) (take take))))))

;;; Disse funksjonene utvider soketreet ved a finne successortrekkene til trekk nedover i treet

(defun iterative-deepening (board)
  "Returnerer dybden til siste fullstendige depth-limited-sok"
  (setf *exp* 0)
  (setf *nc* 0)
  (loop
     (setf *exp* (1+ *exp*))
     (when (numberp (limited-depth *exp* board)) 
       (return (1- *exp*)))))
     
(defun limited-depth (depth board)
  "Utvider spilltreet med lovlige trekk"
  (if (< *vanskelighetsgrad* *nc*) (return-from limited-depth depth))
  (setf *nc* (1+ *nc*))
  (when (plusp depth)
    (dolist (move (moves board))
      (if (not(listp (moves (sboard move))))
	  (successors (sboard move)))
      (limited-depth (1- depth) (sboard move)))))

;;; alpha-betasokene som gir brettene en verdi. Soker gjennom de mulige trekkene
;;; sa dypt som det siste fullforte iterative-deepening soket.

(defun alpha-beta-max (board)
  "Returnerer alfa-beta verdien viss max er i trekket"
  (max-value board most-negative-fixnum most-positive-fixnum (iterative-deepening board)))

(defun alpha-beta-min (board)
  "Returnerer alfa-beta verdien viss min er i trekket"
  (min-value board most-negative-fixnum most-positive-fixnum (iterative-deepening board)))

(defun max-value (board alpha beta limit)
  "Returnerer alfa-beta-verdien til et trekk"
  (cond ((terminal-test board limit) 
	 (setf (value board) (eval-function board)))
	(t
	 (setf (value board) most-negative-fixnum)
	 (dolist (s (moves board))
	   (setf (value board) (max (value board) (min-value (sboard s) alpha beta (1- limit))))
	   (cond ((<= (value board) beta) (value board))
		 (t (setf alpha (max alpha (value board))))))
	 (value board))))

(defun min-value (board alpha beta limit)
  "Returnerer alfa-beta-verdien til et trekk"
  (cond ((terminal-test board limit)
	 (setf (value board) (eval-function board)))
      (t
       (setf (value board) most-positive-fixnum)
       (dolist (s (moves board))
	 (setf (value board) (min (value board) (max-value (sboard s) alpha beta (1- limit))))
	 (cond ((>= (value board) alpha) (value board))
	       (t (setf beta (min beta (value board))))))
	 (value board))))

(defun terminal-test (board limit)
  "Ser om alpha-beta-soket er i en terminaltilstand"
  (or (equal (moves board) 'tom)
      (not (moves board))
      (not (plusp limit))))

(defun eval-function (board)
  "Returnerer en integerverdi som skal vaere en evaluering av hvits posisjon i en gitt situasjon. Hoyere verdi representerer en bedre situasjon for hvit"
 (- (list-value (get-x-pieces board)) (list-value (get-o-pieces board))))

(defun list-value (l)
  "Tar inn en liste med ruter og returnerer den samlede verdien til rutene"
  (cond ((null l) 0)
	(t (+ (value (first l)) (list-value (rest l))))))

;;; Disse funksjonene velger ut det beste trekket maskinen har utifra alfa-beta verdien.

(defun comp-move (board)
  "Returnerer brettet med best trekk for maskinen"
  (if (equal (length (moves board)) 1)
      (sboard (first (moves board)))
      (cond ((equal (player board) *o*)
	     (alpha-beta-min board)
	     (sboard (first (sort (moves board) #'best-move-min))))
	    (t 
	     (alpha-beta-max board)
	     (sboard (first (sort (moves board) #'best-move-max)))))))

(defun best-move-max (board1 board2)
  "Returnerer T viss board1 har storre verdi enn board2"
  (> (value (sboard board1)) (value (sboard board2))))

(defun best-move-min (board1 board2)
    "Returnerer T viss board1 har mindre verdi enn board2"
  (< (value (sboard board1)) (value (sboard board2))))


;;; Metodene under angir hvordan objekter skal printes ut

(defmethod print-object ((tile tile) stream)
  (if (has-piecep tile)
      (format stream "~[ O  ~; X  ~]" (piece tile))
      (format stream " ~[~;A~;B~;C~;D~;E~;F~;G~;H~]~A " (y-pos tile)(x-pos tile))))

(defmethod print-object ((board board) stream)
  (if (equal (board board) 'Empty)
	    (format stream "EMPTY BOARD")
	    (format stream "________________________________________~%~{|~{~A|~}~%________________________________________~%~}" (board board))))

(defmethod print-object ((player player) stream)
  (format stream "~A" (name player)))

(defmethod print-object ((move legal-move) stream)
  (format stream "~A" (move move)))


;;; AIen kan forbedred ved a finne verdier til ruter som stemmer bedre
;;; med den faktiske strategiske verdien.
;;; Verdiene her gir meg en utfordring, men er antakligvis ikke optimale
(defmethod initialize-instance :after ((tile tile) &key)
  "Setter verdien til ruter til antatt strategisk betydning "
  (let  ((x (x-pos tile)) (y (y-pos tile)))
    (cond ((or 
	    (and (eq x 8) (eq y 8)) (and (eq x 8) (eq y 1))
	    (and (eq x 1) (eq y 1)) (and (eq x 1) (eq y 8)))
	   (setf (slot-value tile 'value) 100))
	  ((or 
	    (and (eq x 2) (eq y 2)) (and (eq x 2) (eq y 7))
	    (and (eq x 7) (eq y 7)) (and (eq x 7) (eq y 2)))
	   (setf (slot-value tile 'value) 1))
	  ((or 
	    (and (eq x 2) (eq y 1)) (and (eq x 1) (eq y 2))
	    (and (eq x 7) (eq y 8)) (and (eq x 8) (eq y 7))
	    (and (eq x 1) (eq y 7)) (and (eq x 7) (eq y 1))
	    (and (eq x 2) (eq y 8)) (and (eq x 8) (eq y 2)))
	   (setf (slot-value tile 'value) 2))
	  ((or
	    (and (eq x 4) (eq y 4)) (and (eq x 4) (eq y 5))
	    (and (eq x 5) (eq y 4)) (and (eq x 5) (eq y 5)))
	   (setf (slot-value tile 'value) 20))
	  ((or (eq x 1) (eq y 1) (eq x 8) (eq y 8))
	  (setf (slot-value tile 'value) 10)))))


;;; Fullforer initialiseringen av hvert legal-move
  (defmethod initialize-instance :after ((move legal-move) &key)
    "Setter starttilstanden til trekket"
    (setf (slot-value move 'sboard) (make-instance 'board
						   :pl (change-player (board move))))

    (setf (slot-value (sboard move) 'board) (make-board (sboard move)))
    				       
    (mapcar #'(lambda (tile) (place-piece (y-pos tile) 
					  (x-pos tile) 
					  0 
					  (sboard move))) 
	    (get-o-pieces (board move)))
    
    (mapcar #'(lambda (tile) (place-piece (y-pos tile) 
					  (x-pos tile) 
					  1 
					  (sboard move))) 
	    (get-x-pieces (board move)))
    
    (place-piece (y-pos  (move move)) 
		 (x-pos  (move move)) 
		 (color (player (board move))) 
		 (sboard move))
    (turn (flatten (get-turnable-pieces (get-tile (y-pos (move move)) (x-pos (move move)) (sboard move))))))

(defun place-piece (x y color board)
  "Oppretter en brikke og plaserer den paa brettet"
  (let ((tile (get-tile x y board)))
    (setf (piece tile)  color )))

(defun turn (lp)
  "Returnerer en liste av brikker med motsatt farge"
  (mapcar #'change-color lp))

(defun make-board (board)
  "Returnerer et tomt brett"
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (y)
			  (make-instance 'tile
					 :br board
					 :x x
					 :y y
					 :pc nil))
			  '(1 2 3 4 5 6 7 8)))
	  '(8 7 6 5 4 3 2 1)))


;;; De folgende funksjonene endrer hvem som er spilleren i trekket

(defun switch-player (board)
  "Returnerer et brett der motsatt spiller er i trekket"
  (let ((new-board (make-instance 'board
		    :br (board board)
		    :pl (change-player board))))
    (successors new-board)
    new-board))

(defun change-player (board)
  "Returnerer den spilleren som ikke er i trekket"
  (if (equal (player board) (first *players*)) 
      (elt *players* 1)
      (elt *players* 0)))

;;; Disse metodene brukes til a hente ut brikker fra brettene

(defun get-x-pieces (board)
  "Returnerer en liste over alle de hvite brikkene"
  (flatten (mapcar #'(lambda (x) (append (if (eq (piece x) 1) x))) (list-pieces board))))

(defun get-o-pieces (board)
  "Returnerer en liste over alle de sorte brikkene"
  (flatten (mapcar #'(lambda (x) (append (if (eq (piece x) 0) x))) (list-pieces board))))

(defun list-pieces (board)
  "Returnerer en liste som inneholder alle brikkene paa brettet"
  (flatten (mapcar #'(lambda (x) (mapcar #'(lambda (y) (append (if (piece y) y))) x)) (board board))))

;;; Denne funksjonen er den som spillet bruker for a hente inn en rute

(defun get-tile (x y board)
  "Returnerer ruten som har koordinatet X Y paa brettet"
  (elt (elt (board board) (- 8 y)) (- x 1)))

;;; Disse funksjonene brukes til a hente ut ruter fra et brett

(defmacro make-get (name test get list doc-string)
  `(defun ,name (tile)
     ,doc-string
     (flatten (mapcar #'(lambda (x) (if ,test ,get)) ,list))))

(make-get get-adjacent (funcall x tile) (funcall x tile) *moves*  
	    "Returnerer alle rutene som ligger ved siden av en rute")

(make-get get-non-empty-adjacent (has-piecep x) x  (get-adjacent tile)
  "Returnerer alle de ikke tomme rutene ved siden av en rute")

(make-get  get-opposite-adjacent (opposite-color tile x) x (get-non-empty-adjacent tile)
  "Returnerer ruter som inneholder brikke med motsatt farge")

(make-get possible-take-moves (get-to-move? x tile) (list (get-to-move? x tile) tile) (get-opposite-adjacent tile)
	  "Returnerer liste over ruter en kan ta, og trekk en kan gjore for aa ta dem")

(make-get possible-successor-moves (get-to-move? tile x) (list (get-to-move? tile x ) tile) (get-opposite-adjacent tile)
	  "Returnerer liste over ruter en kan komme til, og trekket en gjor for aa komme dit")



;;; Disse metodende er typesjekker og tilstandssjekker for ruter

(defun empty-tile (tile)
  "Returnerer T dersom ruten er tom"
	 (not (has-piecep tile)))

(defun change-color (tile)
  "Skifter farge paa brikken til en rute"
  (if (equal (piece tile) 0) 
      (setf (piece tile) 1)
      (setf (piece tile) 0)))

(defun has-piecep (tile)
  "Returnerer T viss ruten har en brikke"
  (and (typep tile 'tile)
      (piece tile)))

(defun type-tile (object)
  "Returnerer T viss objektet er en rute"
  (if (typep object 'tile)
      object))

(defun equal-tile (tile1 tile2)
  "Returnerer T viss tilstanden til to ruter er lik"
  (if (and (type-tile tile1) (type-tile tile2))
  (and
   (has-piecep tile1)
   (has-piecep tile2)
   (eq (x-pos tile1) (x-pos tile2))
   (eq (y-pos tile1) (y-pos tile2))
   (eq (piece tile1) (piece tile2)))))

(defun opposite-color (tile1 tile2)
  "Forventer aa faa to tiles som har brikker paa seg. Returnerer T dersom brikkene har ulik farge"
  (if (not (equal (piece tile1) (piece tile2))) t))

;;; Funksjonen gir informasjon om hvordan en beveger seg fra en rute til en annen

(defun get-to-move? (tile tile2)
  "Returnerer trekket fra tile2 til tile"
  (flatten (append (mapcar #'(lambda (moves)  (if (funcall moves tile) (if (equal-tile tile (funcall moves tile2)) moves))) *moves*))))






;;;; Funksjonene under er ikke mine. De er hentet fra pensumsboker og er fritt tilgjenglig pa internett.


;;; Denne funksjonen er hentet fra boken onlisp s.49, tilgjenglig pa http://lib.store.yahoo.net/lib/paulgraham/onlisp.pdf 
(defun flatten (x) 
  "Tar en liste. Returnerer en liste som ikke inneholder lister eller nil"
  (labels ((rec (x acc) 
	     (cond ((null x) acc) 
		   ((atom x) (cons x acc)) 
		   (t (rec (car x) (rec (cdr x) acc)))))) 
    (rec x nil))) 

;;; Denne funksjonen er hentet fra boken onlisp s.47, tilgjenglig pa http://lib.store.yahoo.net/lib/paulgraham/onlisp.pdf 
(defun group (source n) 
  (if (zerop n) (error "zero length")) 
  (labels ((rec (source acc) 
	     (let ((rest (nthcdr n source))) 
	       (if (consp rest) 
		   (rec rest (cons (subseq source 0 n) acc)) 
		   (nreverse (cons source acc)))))) 
    (if source (rec source nil) nil))) 

;;; Denne funksjonen er hentet fra http://gandalf.aksis.uib.no/lingkurs/webroot/index.php?page=lispintro/lsearch&lang=nb&course=dasp101
(defun lsearch (situation &key test next (take #'identity))
  (cond ((null situation) nil)
	((funcall test situation) (funcall take situation))
	(t (lsearch (funcall next situation)
		    :test test
		    :next next
		    :take take))))